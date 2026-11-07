# Partnership agreements: hand-maintained inputs

Inputs of the partnership agreements layer (`docs/partnership_agreements.md`) that are kept by
hand. Each is read by `scripts/35_ingest_agreements.R` through an adapter in `R/agreements/`.

| File | Adapter | Source id |
|---|---|---|
| `curated_agreements.csv` (this folder) | `adapter_curated.R`, `agr_adapt_curated()` | `curated` |
| `data/raw/iisd_trade_climate_tracker.csv` (staged, gitignored) | `adapter_iisd.R`, `agr_adapt_iisd()` | `iisd` |

## 1. `curated_agreements.csv`

### Purpose

Government agreements that the structured datasets (GSP, DESTA, IISD) miss or record too coarsely:
critical-mineral MOUs, sectoral partnerships, investment frameworks, joint initiatives. There is no
complete registry of such documents, so this table is the place to add them one at a time, **each
from an official source**. Every row becomes one agreement record (`source_record_id`
`curated:<curated_id>`); its sectors, instruments and stages become codes with basis `curated`.

### Columns (in this order; every cell is text, empty when unknown)

| Column | Meaning |
|---|---|
| `curated_id` | Stable id, never reused: issuer or pair, topic, year, e.g. `EU-CRM-CHL-2023`. Letters, digits, `.`, `_`, `-`. |
| `title` | The agreement's own title, in English where an official English title exists. |
| `parties` | ISO3 codes joined by `|`, e.g. `CHL|EUU`. `EUU` is the European Union; `XKX` is Kosovo. At least two. |
| `agreement_type` | One id of `agreement_types` in `config/partnership_agreements.yml` (`treaty`, `fta`, `epa`, `strategic_partnership`, `framework`, `mou`, `joint_declaration`, `joint_statement`, `sectoral_partnership`, `ministerial_initiative`, `plurilateral_forum`, `financing_agreement`, `other`). |
| `agreement_type_raw` | The document's own words for its type, e.g. `Memorandum of Understanding`. |
| `partnership_label` | One id of `partnership_labels` when the document names a partnership (`comprehensive_strategic`, `global_strategic`, `special_strategic`, `strategic`, `comprehensive`, `other_partnership`), else empty. |
| `legal_status` | `binding`, `non_binding`, `mixed` or `unknown`. **`unknown` unless the official source itself says** whether the text is legally binding (e.g. "does not create legally binding obligations"). Never inferred from the title or the type. |
| `legal_status_note` | The official wording behind `legal_status` (required when it is not `unknown`). |
| `announcement_date`, `signature_date`, `effective_date`, `expiry_date` | `YYYY-MM-DD`, `YYYY-MM` or `YYYY`; empty if unknown. |
| `relationship_category` | `overall_relationship`, `trade_economic`, `sectoral`, `investment_finance`, `plurilateral_club` or `other`. |
| `sectors` | Config `sectors` ids joined by `|`: **only sectors the official text names as areas of cooperation.** |
| `instruments` | Config `instruments` ids joined by `|`: **only mechanisms the text establishes or commits to** (a working group it creates, financing it commits, an offtake it agrees), not aspirations it merely mentions. |
| `supply_chain_segments` | Optional: `Extraction`, `Refining`, `Components`, `Assembly`, `Deployment`, `Recovery` or `cross_stage`, joined by `|`. When given, the stages implied by the sectors are not added. |
| `implementation_status` | Config `implementation_statuses` id. `announced`, `signed` or `in_force` unless there is concrete delivery evidence; `implementation_underway` or `operational` need `implementation_evidence`. |
| `implementation_status_raw` | The status in the source's words. |
| `implementation_evidence` | Concrete delivery evidence in your own words with its URL, e.g. `first project financed: https://...`; empty if none found. |
| `source_url` | The official page you read (required). |
| `primary_document_url` | The URL of the agreement text itself when public, else empty. |
| `publisher` | The institution that published `source_url`. |
| `last_verified` | The date you checked the source (`YYYY-MM-DD`). |
| `verification` | `verified`: you read the official text or official page and it confirms parties, date and scope. `partial`: an official page confirms parties and date but not scope. `unverified`: secondary sources only. |
| `description` | One neutral sentence in your own words (no copied text). |
| `notes` | Anything else a reader needs (conflicting dates, renewals, what was not found). |

### Evidence rules

1. **An official source is required.** A government, EU institution or treaty-depositary page, or
   the agreement text. A news report alone makes the row `unverified` (shown with low confidence).
2. **Sectors and instruments only when the text establishes them.** "The sides discussed hydrogen"
   is not hydrogen cooperation; "the Participants will establish a Joint Working Group on hydrogen"
   is `hydrogen` + `working_group`. Financing counts when the text commits it (an amount, a lender
   or a facility), not when it hopes for it.
3. **Legal status only from the text.** MOUs are often expressly non-binding; record the sentence
   that says so in `legal_status_note`. If the text is silent, `unknown`.
4. **Existence is not delivery.** `implementation_underway` / `operational` need a concrete
   delivery fact (a project financed, a body that has met, a plant under construction) with a URL.

### Adding a row

1. Find the official source; read it.
2. Fill one row; keep `curated_id` stable and unique.
3. Run `scripts/35_ingest_agreements.R --source curated`. The adapter stops on a malformed row and
   names its `curated_id` and the problem (bad ISO3, a value outside the vocabularies, a bad date,
   a missing source, a legal status without its wording, a delivery status without evidence).

## 2. IISD Trade & Climate Tracker export (`data/raw/iisd_trade_climate_tracker.csv`)

IISD's tracker (<https://www.iisd.org/trade-climate-tracker/>) is behind a bot challenge and its
content is all rights reserved, so it is never fetched. Stage a CSV export **only with IISD's
permission** (ask the tracker team for an export for research use; record the date and the
permission in the manifest entry). The tracker covers economic agreements signed by at least two
governments between January 2025 and June 2026 with text available on 1 July 2026, and codes the
operative provisions of each (methodology of 1 July 2026).

### Expected columns

Names are matched case- and punctuation-insensitively, with "and" / "&" ignored, against the
aliases in `IISD_COLUMN_ALIASES` (`R/agreements/adapter_iisd.R`). Only `title` and `parties` are
required, plus either subtopic columns or a `topics` column.

| Canonical column | Accepted names (examples) | Content |
|---|---|---|
| `agreement_id` | `agreement_id`, `id`, `record_id` | IISD's id; else the id is built from title and signature date. |
| `title` | `title`, `agreement`, `agreement_name` | Agreement title. |
| `parties` | `parties`, `signatories`, `countries` | ISO3 codes or names, separated by `;` or `|` (one cell of names separated by `,` / `and` is also tried). `EU` / `European Union` -> `EUU`; `EFTA`, `MERCOSUR`, `GCC` expand to their members (`IISD_PARTY_GROUPS`). An unresolved name stops the adapter (`strict = TRUE`). |
| `signature_date` | `signature_date`, `date_of_signature`, `date_signed`, `date` | ISO dates, `DD/MM/YYYY`, `15 March 2025`, `March 15, 2025`, `March 2025`. |
| `announcement_date`, `effective_date` | `date_announced`; `entry_into_force` | Optional. |
| `agreement_type` | `agreement_type`, `type`, `instrument_type` | IISD categories: FTA, ART -> `fta`; ECA -> `framework`; MoU / MoC -> `mou`; CRM framework / agreement -> `framework`; strategic partnership -> `strategic_partnership`; EPA -> `epa`; else `other`. |
| `legal_status` | `legal_status`, `legally_binding` | Optional. binding / yes -> `binding`, non-binding / no -> `non_binding`, mixed -> `mixed` (basis `source_field`); absent -> `unknown`. |
| `status` | `status` | Optional; "in force" -> `in_force`. Otherwise `signed` (`announced` when only an announcement date is given). |
| `text_available` | `text_available`, `full_text_available` | Kept in the notes. |
| `source_url` | `source_url`, `url`, `link` | The tracker page of the agreement (defaults to the tracker home page). |
| `document_url` | `document_url`, `text_url`, `agreement_text_url` | The agreement text; an official host makes `primary_source_available` TRUE. |
| subtopic columns | `Energy: Renewables`, `Energy: Cooperation and Systems`, `Climate: Emissions`, `Climate: Deforestation (and Land Use)`, `Climate: Others`, `CRMs: Value Chain Stages`, `CRMs: Supply Chain Diversification / Security`, `Heavy Industry: Decarbonization`, `Heavy Industry: Circularity`, `Tariffs and Market Access` | The coded summary of the provisions, or a flag (`Yes`, `X`, `1`). Empty / `No` / `0` / `N/A` = not coded. |
| `topics` | `topics`, `subtopics` | Alternative long form: subtopic names separated by `;` or `|` (topic-level names such as `CRMs` or `Energy` are accepted). |
| `summary` | `summary`, `coded_summary` | Optional free-text summary, keyword-coded with the subtopic summaries. |
| `last_updated` | `last_updated`, `date_coded` | Optional; becomes `source_last_updated`. |

### Coding

| IISD subtopic | Codes (basis `source_coded`) |
|---|---|
| CRMs (either subtopic) | sector `critical_minerals` |
| Energy: Renewables; Energy: Cooperation and Systems | sector `general_clean_energy` |
| Heavy Industry (either subtopic) | sector `general_industrial` |
| Tariffs and Market Access (non-empty) | sector `trade`; instruments `tariff_reduction`, `market_access` |
| Climate subtopics | none (kept in `extras$topics`) |

The coded summaries are then keyword-coded (`agr_code_documents()`, basis `text_body`) for the
specific sectors (solar, hydrogen, nuclear ...) and instruments. `legal_status` stays `unknown`
unless a column codes it. To check a staged file, run the adapter and look at
`extras$column_map` (which export column fed which field) and `extras$unresolved_parties`.
