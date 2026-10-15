// Country Insights data contract (spec_country.md A8; frozen in phase 0 on 2026-10-12). The interface
// between scripts/88_export_country_data.R (which writes these files) and the views in src/country/.
// Owned by the country builder; changes after the freeze are additive only and logged in
// scratch_dash/country/build_log.md ("Contract changes"). Pure: no IO, no React.
//
// Files under <base>data/country/ (A7.4):
//   manifest.json                       CountryManifest (no-cache)
//   facts-<ISO3>.<h8>.json              FactSheet, one per economy with any score or partner row
//   memo-<ISO3>.<h8>.json               MemoDoc (core members, after the memo phase)
//   memo-<ISO3>.<h8>.{pdf,docx,md}      downloads
//
// Every number that is shown or cited is an Fv { v, d }: `v` the value (null when missing), `d` its
// display string, computed once in R (country_fmt()), so the screen, the PDF and the Word file print
// the same text. The UI never formats a cited number itself (axis ticks only).
//
// Runtime checks: checkCountryManifest(), checkFactSheet(), checkMemoDoc() return problem strings;
// validateFactSheet() throws CountryContractError. The R mirror is country_check_sheet()
// (R/dashboard/country/country_check.R); both are fed the same broken fixtures.

// ---- Schema --------------------------------------------------------------------------------------

export const COUNTRY_SCHEMA = 'osi-country';
export const COUNTRY_FACTS_SCHEMA = 'osi-country-facts';
export const COUNTRY_MEMO_SCHEMA = 'osi-country-memo';
export const COUNTRY_SCHEMA_VERSION = 1;
/** Relative to the app's base URL. Written only by scripts/88. */
export const COUNTRY_DATA_DIR = 'data/country/';
export const COUNTRY_MANIFEST_FILE = 'manifest.json';
/** File names scripts/88 writes (and deletes when the manifest no longer lists them). */
export const COUNTRY_FILE_RE = /^(facts|drivers|memo)-[A-Z0-9]{3}\.[0-9a-f]{8}\.(json|pdf|docx|md)$/;
/** gzip budgets in bytes (A10). */
export const COUNTRY_BUDGET_GZ = { manifest: 20_480, sheet: 40_960, memoJson: 25_600, memoPdf: 409_600, memoDocx: 102_400 } as const;
/** Path segments under /countries that are not ISO3 codes. */
export const COUNTRY_RESERVED = ['about'] as const;

/** The verbatim memo label (also in the manifest, from config/country_insights.yml). */
export const MEMO_LABEL_PREFIX = 'AI-generated from the data and checked against it; paper baseline as of';

// ---- Sectors ---------------------------------------------------------------------------------------

/** The 11 headline technologies in the Sector cell order (alphabetical, as src/sector/contract.ts). */
export const COUNTRY_TECHS = [
  { slug: 'batteries', label: 'Batteries' },
  { slug: 'coal', label: 'Coal' },
  { slug: 'electric-grid', label: 'Electric Grid' },
  { slug: 'electric-vehicles', label: 'Electric Vehicles' },
  { slug: 'gas', label: 'Gas' },
  { slug: 'geothermal', label: 'Geothermal' },
  { slug: 'green-hydrogen', label: 'Green Hydrogen' },
  { slug: 'nuclear', label: 'Nuclear' },
  { slug: 'oil', label: 'Oil' },
  { slug: 'solar', label: 'Solar' },
  { slug: 'wind', label: 'Wind' },
] as const;
export const COUNTRY_STAGES = [
  { slug: 'upstream', label: 'Upstream' },
  { slug: 'midstream', label: 'Midstream' },
  { slug: 'downstream', label: 'Downstream' },
] as const;
export type CountryTechSlug = (typeof COUNTRY_TECHS)[number]['slug'];
export type CountryStageSlug = (typeof COUNTRY_STAGES)[number]['slug'];
export type StageLabel = (typeof COUNTRY_STAGES)[number]['label'];

/** "batteries-midstream" (the Sector cellSlug() rule). */
export const cellKey = (tech: string, stage: string): string => `${tech}-${stage}`;

/** The 33 cell keys in the Sector cell order (technology, then Upstream, Midstream, Downstream). */
export const COUNTRY_CELL_KEYS: readonly string[] = COUNTRY_TECHS.flatMap((t) => COUNTRY_STAGES.map((s) => cellKey(t.slug, s.slug)));
const CELL_KEY_SET = new Set(COUNTRY_CELL_KEYS);
export const isCellKey = (k: unknown): k is string => typeof k === 'string' && CELL_KEY_SET.has(k);
/** Position of a cell key in the Sector cell order (-1 if unknown). */
export const cellOrder = (k: string): number => COUNTRY_CELL_KEYS.indexOf(k);

/** The (tech, stage) slugs of a cell key, or null. */
export function parseCellKey(k: string): { tech: CountryTechSlug; stage: CountryStageSlug } | null {
  for (const s of COUNTRY_STAGES) {
    if (k.endsWith(`-${s.slug}`)) {
      const t = k.slice(0, -(s.slug.length + 1));
      const tech = COUNTRY_TECHS.find((x) => x.slug === t);
      return tech ? { tech: tech.slug, stage: s.slug } : null;
    }
  }
  return null;
}

/** The paper has no Energy Security score for these sectors (A9.1); at most 30 can be placed. */
export const NO_ES_CELLS: readonly string[] = ['geothermal-upstream', 'geothermal-midstream', 'electric-grid-downstream'];

// ---- Enums ----------------------------------------------------------------------------------------

export type Quadrant = 'grow' | 'upgrade' | 'secure' | 'watch';
/** Order and words of the four corners (A4.2 S2); the user's names and RMI (2025) postures. */
export const QUADRANTS: readonly { id: Quadrant; label: string; corner: 'top right' | 'top left' | 'bottom left' | 'bottom right'; meaning: string; posture: string }[] = [
  { id: 'grow', label: 'Grow', corner: 'top right', meaning: 'High opportunity, high energy security: build on strength', posture: 'export promotion' },
  { id: 'upgrade', label: 'Upgrade', corner: 'top left', meaning: 'High opportunity, low energy security: build capability', posture: 'leapfrog' },
  { id: 'secure', label: 'Secure', corner: 'bottom left', meaning: 'Low opportunity, low energy security: secure supply through partners', posture: 'friendshore' },
  { id: 'watch', label: 'Watch', corner: 'bottom right', meaning: 'Low opportunity, high energy security: look for value elsewhere', posture: 'alternative value creation' },
];
export const QUADRANT_IDS: readonly Quadrant[] = QUADRANTS.map((q) => q.id);

export type Third = 'high' | 'middle' | 'low';
export const THIRDS: readonly Third[] = ['high', 'middle', 'low'];

export type AgreementClass = 'aligned_high' | 'gap' | 'policy_ahead' | 'aligned_low' | 'partial';
export const AGREEMENT_CLASSES: readonly AgreementClass[] = ['aligned_high', 'gap', 'policy_ahead', 'aligned_low', 'partial'];

export type PartnerMatch = 'priority_strong_ties' | 'priority_weak_ties' | 'ties_lower_priority' | null;
export const PARTNER_MATCHES: readonly Exclude<PartnerMatch, null>[] = ['priority_strong_ties', 'priority_weak_ties', 'ties_lower_priority'];

export type PartnerReason = 'fs' | 'po' | 'dp';
export const PARTNER_REASONS: readonly PartnerReason[] = ['fs', 'po', 'dp'];

export type PartnerGroup = 'china' | 'core' | 'partner' | 'rest';
export const PARTNER_GROUPS: readonly PartnerGroup[] = ['china', 'core', 'partner', 'rest'];

export type Tier = 'core' | 'partner' | 'none';
export const TIERS: readonly Tier[] = ['core', 'partner', 'none'];

export type SheetStatus = 'ok' | 'non_rankable' | 'no_data';
export const SHEET_STATUSES: readonly SheetStatus[] = ['ok', 'non_rankable', 'no_data'];

export type PriorityRule = 'upgrade' | 'top_si_fallback' | 'top_eo' | 'none';
export const PRIORITY_RULES: readonly PriorityRule[] = ['upgrade', 'top_si_fallback', 'top_eo', 'none'];

export type FlowStatus = 'importer' | 'mirror';
export type AlignmentBasis = 'own_votes' | 'inherited' | 'neutral';

/** Keys of the agreement matrix: <priority third>_<policy third>. */
export const MATRIX_KEYS: readonly string[] = THIRDS.flatMap((p) => THIRDS.map((q) => `${p}_${q}`));

/** Caveat ids (A8.4). */
export const CAVEAT_IDS = [
  'relative_scores', 'baseline', 'rankability', 'imputed_driver', 'policy_pams_only', 'no_si', 'near_tie', 'eu_node',
  'inherited_alignment', 'assumed_zero_production', 'cmfdi_coverage', 'trade_mirror', 'few_economies', 'cat_eu_bloc',
  'intra_eu', 'hkg_reexport', 'pick_stability', 'ownership_parent', 'eu_policy_records',
  // additive 2026-10-12: Global Trade Alert NIPO measures are context, not part of the Policy index
  'nipo_context',
  // additive 2026-10-14: a cited figure is the EU's regional value carried by each member (raw note eu_region_share)
  'eu_region_share',
] as const;
export type CaveatId = (typeof CAVEAT_IDS)[number];

// ---- Values ---------------------------------------------------------------------------------------

/** A shown or cited number: the value and its display string. */
export interface Fv {
  v: number | null;
  d: string;
}

/** A source of the sheet's numbers (the Data notes and the memo's source list). */
export interface SourceRef {
  id: string;
  label: string;
  edition: string | null;
  licence: string | null;
  url: string | null;
}

// ---- Manifest (A8.1) -------------------------------------------------------------------------------

export interface MemoEntry {
  json: string;
  pdf: string | null;
  docx: string | null;
  md: string | null;
  words: number;
  generated_at: string;
  verified_at: string;
  facts_sha256: string;
  /** a later fact sheet changed a claim: the UI hides the memo (B6.5) */
  stale: boolean;
}

export interface CountryEntry {
  iso3: string;
  name: string;
  tier: Tier;
  region: string | null;
  eu_member: boolean;
  status: SheetStatus;
  /** the fact sheet, null for status no_data */
  file: string | null;
  bytes_gz: number | null;
  n_placed: number | null;
  quadrants: Record<Quadrant, number> | null;
  memo: MemoEntry | null;
  /** additive 2026-10-12: the companion drivers file (the "why" data of the sectors not carried in the sheet) */
  drivers?: string | null;
  /** additive 2026-10-13: ISO 3166 numeric code ("392"), the world map's join key; null for entities without one */
  iso_n3?: string | null;
}

export interface ValidationRow {
  check: string;
  status: 'PASS' | 'WARN' | 'FAIL' | 'SKIP';
  n: number | null;
  max_abs_diff: number | null;
  note: string | null;
}

export interface CountryManifest {
  schema: typeof COUNTRY_SCHEMA;
  schema_version: number;
  created: string;
  git_commit: string | null;
  as_of: string;
  as_of_label: string;
  strategic_bundle_id: string | null;
  sector_bundle_id: string | null;
  label: string;
  counts: { economies: number; sheets: number; core: number; partners: number; memos: number };
  /** false when built with --no-validate-bundles or --synthetic (the memo phase refuses such sheets) */
  validated: boolean;
  synthetic?: boolean;
  /** additive 2026-10-13: the GTA New Industrial Policy Observatory run the sheets list measures from */
  nipo?: { vintage: string | null; vintage_label: string | null } | null;
  countries: CountryEntry[];
  sources: SourceRef[];
  validation: ValidationRow[];
}

// ---- Fact sheet (A8.2) ------------------------------------------------------------------------------

export interface SheetMeta {
  country: string;
  tier: Tier;
  routes: string[];
  region: string | null;
  income_group: string | null;
  sovereign: boolean;
  eu_member: boolean;
  /** the allied-network node (the ISO3 itself, or "EUU" for EU members), null outside the coalition */
  network_node: string | null;
  /** shared-agreement names the country is listed in (economic_security_agreements.csv; EU rows not propagated) */
  agreements: string[];
  /** EU members: the EU's own memberships ("MSP", "G7"), said once in words */
  eu_agreements: string[];
  bloc: string | null;
  /** position on the UN-vote axis (0 China, 1 United States) */
  alignment_s: Fv;
  alignment_basis: AlignmentBasis | null;
  /** territories: the sovereign whose position they inherit */
  alignment_from: string | null;
  status: SheetStatus;
  status_reason: string | null;
  has_si: boolean;
  /** "IEA PAMS + CAT" | "IEA PAMS only" | null */
  policy_basis: string | null;
  as_of: string;
  as_of_label: string;
  created: string;
  strategic_bundle_id: string | null;
  sector_bundle_id: string | null;
  /** the rankability threshold (max imputed weight share), d "half" */
  rank_threshold: Fv;
  trade_year: string;
  un_window: string;
  cmfdi_edition: string;
  memo_eligible: boolean;
}

/** enum -> display words (config/country_insights.yml); tokens and the UI print enums through it. */
export interface Words {
  quadrant: Record<Quadrant, string>;
  third: Record<Third, string>;
  policy_third: Record<Third, string>;
  class: Record<AgreementClass, string>;
  reason: Record<PartnerReason, string>;
  match: Record<Exclude<PartnerMatch, null>, string>;
  alignment: Record<string, string>;
  tier: Record<Tier, string>;
  agreement: Record<string, string>;
  group: Record<PartnerGroup, string>;
  [k: string]: Record<string, string>;
}

export interface Summary {
  n_cells: Fv;
  n_scored: Fv;
  n_placed: Fv;
  quadrants: Record<Quadrant, Fv>;
  /** upgrade + secure (the two low-energy-security corners) */
  n_insecure: Fv;
  /** grow + upgrade */
  n_high_eo: Fv;
  /** placed cell keys by es.pct ascending (lowest Energy Security first); ties by cell order */
  es_order: string[];
  /** placed cell keys by eo.pct descending (highest Economic Opportunity first); ties by cell order */
  eo_order: string[];
  /** in-sample cell keys by si.rank */
  si_order: string[];
  top_si: { cell: string; label: string; si: Fv; margin: Fv; next: string | null; next_label: string | null; near_tie: boolean } | null;
  psi_1: { iso3: string; name: string; psi: Fv; n_ranked: Fv } | null;
  as_partner: { rank: Fv; n: Fv; psi: Fv; view: string } | null;
}

/** One pillar (Energy Security or Economic Opportunity) of a cell; v/d make it an Fv. */
export interface PillarFacts extends Fv {
  rankable: boolean;
  /** why it is not rankable (the pipeline's rank_exclusion), else null */
  reason: string | null;
  imputed_share: Fv;
  /** share of the other economies of S_c with a lower value (ties half), 0-100 */
  pct: Fv;
  /** plotted position (pct pinned to its side of the median, A9.1); never cited */
  pos: number | null;
  /** median over S_c */
  med: Fv;
  /** |S_c| */
  n: Fv;
  /** |S_c without the country| */
  n_others: Fv;
  /** position in summary.es_order / eo_order (placed cells), d "3rd lowest" */
  order: Fv;
  /** the side of the median, in words */
  side: 'above' | 'below' | null;
}

export interface SiParts {
  eo: Fv;
  need: Fv;
  pol: Fv;
  stage: Fv;
  tech: Fv;
}

export interface SiFacts extends Fv {
  in_sample: boolean;
  rank: Fv;
  n: Fv;
  exclusion: string | null;
  /** SI minus the SI of the next-ranked sector (null for the last) */
  margin_next: Fv;
  top5: boolean;
  /** the five level terms (A9.3); sum = v */
  parts: SiParts | null;
  pick: boolean;
  resource_base: boolean | null;
  pick_exclusion: string | null;
}

export interface CellPolicy {
  /** the Policy index value of the cell */
  index: Fv;
  /** the within-country S-curve the SI uses */
  pol: Fv;
  rank: Fv;
  third: Third | null;
  class: AgreementClass | null;
  records_specific: Fv;
  records_tech: Fv;
  records_all: Fv;
  latest_year: Fv;
}

export interface TradeParty {
  iso3: string;
  name: string;
  group: PartnerGroup;
  share: Fv;
  value: Fv;
  status: FlowStatus;
}

export interface CellTrade {
  has_basket: boolean;
  year: string;
  imports: Fv;
  exports: Fv;
  sources: TradeParty[];
  markets: TradeParty[];
}

export interface RawValue extends Fv {
  unit: string | null;
  year: string | null;
  /** source id (src_map / sources[]) */
  source: string | null;
  /** short source name for the "(2024, UN Comtrade)" line */
  source_label: string | null;
  /** e.g. "production_assumed_zero", "eia_production", "eu_region_share" (additive 2026-10-14: the EU's regional
   *  figure, copied to each EU member: IEA ETP 2035 manufacturing shares, IEA EV-region figures) */
  note: string | null;
  /** additive 2026-10-14: the note in plain words ("the EU's announced share, carried by each EU member"); absent
   *  when there is no note */
  note_d?: string | null;
}

export interface DriverVar {
  key: string;
  variable: string;
  label: string;
  score: Fv;
  median: Fv;
  /** percentile of the (oriented) component among the other economies of S_c */
  pct: Fv;
  /** weight within the category (sums to 1 per category) */
  weight: Fv;
  imputed: boolean;
  rule: string | null;
  rule_d: string | null;
  /** the same for every economy of S_c (e.g. the world-market HHI): never a driver */
  constant: boolean;
  raw: RawValue | null;
  sector_link: string | null;
}

/** A category of a pillar. `vars` is filled for the top categories only (the "What's behind it"
 *  list and the memo's raw figures; other categories carry an empty list to keep the sheet small). */
export interface Driver {
  key: string;
  category: string;
  score: Fv;
  median: Fv;
  /** share of the pillar's weight (sums to 1 over the cell's categories) */
  weight: Fv;
  /** weight x (score - median) */
  impact: Fv;
  imputed: boolean;
  rule: string | null;
  rule_d: string | null;
  top: boolean;
  constant: boolean;
  vars: DriverVar[];
}

export interface CellFacts {
  key: string;
  tech: string;
  stage: StageLabel;
  /** "Oil — Upstream" */
  label: string;
  /** "upstream oil" (running text) */
  label_prose: string;
  es: PillarFacts | null;
  eo: PillarFacts | null;
  /** both scores rankable: the cell is placed on the scatter and has a quadrant */
  placed: boolean;
  quadrant: Quadrant | null;
  /** why it is not placed */
  quadrant_reason: string | null;
  si: SiFacts;
  priority: { v: Fv; rank: Fv; third: Third | null } | null;
  policy: CellPolicy | null;
  trade: CellTrade | null;
  fdi: { in_n: Fv; in_china_n: Fv; out_n: Fv } | null;
  drivers: { es: Driver[]; eo: Driver[] };
  /** additive 2026-10-12: false when this cell's drivers are in the companion drivers file (manifest entry `drivers`) */
  drivers_in_sheet?: boolean;
}

/** The companion drivers file (additive 2026-10-12): drivers of the cells whose `drivers_in_sheet` is false. */
export interface DriversDoc {
  schema: 'osi-country-drivers';
  schema_version: number;
  iso3: string;
  /** the fact sheet the file belongs to */
  facts_sha256: string;
  cells: Record<string, { es: Driver[]; eo: Driver[] }>;
}

/** Problems with a drivers file (empty when it is valid). */
export function checkDriversDoc(x: unknown, sheet?: FactSheet): string[] {
  const out: string[] = [];
  if (typeof x !== 'object' || x === null || Array.isArray(x)) return ['drivers: not an object'];
  const d = x as Record<string, unknown>;
  if (d.schema !== 'osi-country-drivers') out.push("drivers.schema: expected 'osi-country-drivers'");
  if (sheet && d.iso3 !== sheet.iso3) out.push(`drivers.iso3: ${String(d.iso3)} is not ${sheet.iso3}`);
  if (sheet && d.facts_sha256 !== sheet.checks.facts_sha256) out.push('drivers.facts_sha256: belongs to another export of the sheet');
  if (typeof d.cells !== 'object' || d.cells === null || Array.isArray(d.cells)) out.push('drivers.cells: not an object');
  else for (const k of Object.keys(d.cells)) if (!isCellKey(k)) out.push(`drivers.cells: '${k}' is not a sector key`);
  return out;
}

export interface CatRating {
  overall: string;
  /** as in the CAT file ("Nov-24") */
  date: string | null;
  date_label: string | null;
  scope: 'national' | 'eu_bloc';
  sub: Record<string, string | null>;
}

/** One GTA New Industrial Policy Observatory measure in force (additive 2026-10-12, the user's NIPO request). */
export interface NipoPolicy {
  id: number;
  title: string;
  /** implementation date, ISO (null only under the not_removed rule, when GTA records none) */
  date: string | null;
  date_label: string;
  /** additive 2026-10-13: implemented on or before the baseline date */
  in_force?: boolean;
  /** a future removal date, ISO, or null */
  removal: string | null;
  /** additive 2026-10-14: the removal date in words ("31 March 2027"), as date_label */
  removal_label?: string | null;
  /** policy_strength_pkg: strength counting the package (the ranking measure) */
  strength: Fv;
  /** policy_strength: without the package multiplier */
  strength_base: Fv;
  /** mapping_confidence_mean (0.10 to 2; 0.75 or more is a clear fit) */
  confidence: Fv;
  techs: string[];
  stages: string[];
  url: string | null;
  source: string | null;
  /** additive 2026-10-14: 'eu' (an EU-level measure Global Trade Alert records under each EU member it covers),
   *  'multi' (recorded under more than one implementing jurisdiction) or 'national'; words in `words.nipo_scope` */
  scope?: NipoScope;
  /** additive 2026-10-14: how many implementing jurisdictions the run records the measure under */
  n_jurisdictions?: Fv;
  /** additive 2026-10-14: mapped to `economy_wide_min_techs` or more technologies (sanctions, export controls, trade
   *  status changes); listed in a sector after its sector-specific measures */
  economy_wide?: boolean;
}

export type NipoScope = 'eu' | 'multi' | 'national';
export const NIPO_SCOPES: readonly NipoScope[] = ['eu', 'multi', 'national'];

/** The strongest active NIPO measures per sector (context; not part of the Policy index). */
export interface NipoFacts {
  /** the economy is among the run's implementing jurisdictions */
  covered: boolean;
  vintage: string | null;
  /** e.g. "Global Trade Alert, New Industrial Policy Observatory, October 2026 extract (as of 5 October 2026)" */
  vintage_label: string | null;
  /** the date "in force" is judged on (the paper baseline date) */
  as_of: string;
  measure: string;
  /** additive 2026-10-13: 'not_removed' (the user's rule and the default: no removal date or a later one, whatever the
   *  implementation date; measures not yet implemented are listed with in_force false) or 'in_force' (also implemented by
   *  as_of: the NIPO pipeline's rule; measures not yet implemented are counted, never listed) */
  active_rule?: 'in_force' | 'not_removed';
  /** additive 2026-10-14: a measure mapped to this many technologies or more is economy-wide (listed last in a sector) */
  economy_wide_min_techs?: number;
  n_active: Fv;
  /** additive 2026-10-14: active measures that are EU-wide, and that are recorded under more than one jurisdiction
   *  (EU-wide included); the same among the listed measures (n_listed: the distinct measures in `policies`) */
  n_active_eu?: Fv;
  n_active_shared?: Fv;
  n_listed?: Fv;
  n_listed_eu?: Fv;
  n_listed_shared?: Fv;
  /** additive 2026-10-13: active measures not yet in force on as_of (announced or scheduled); listed (marked) under
   *  'not_removed', counted only under 'in_force' */
  n_not_in_force?: Fv;
  /** additive 2026-10-13: how many implementing jurisdictions the NIPO run covers */
  jurisdictions?: Fv;
  /** additive 2026-10-13: the strength and mapping-confidence scores in plain words (config/country_insights.yml nipo) */
  strength_words?: string | null;
  confidence_words?: string | null;
  /** measures in force without product codes ("Cross-cutting" in NIPO): counted, not listed by sector */
  n_crosscut: Fv;
  /** measures in force per cell key */
  n_by_cell: Record<string, Fv>;
  /** cell key -> up to five policy ids, strongest first */
  cells: Record<string, number[]>;
  policies: NipoPolicy[];
}

export interface PolicyFacts {
  cat: CatRating | null;
  pams_total: Fv;
  pams_tech_specific: Fv;
  pams_no_tech: Fv;
  pams_critical_minerals: Fv;
  /** status_norm values of policy_clean: In force, Ended, Unknown (no "Planned") */
  pams_by_status: Record<string, Fv>;
  /** EU members: the EU's own records, not added to the country's */
  pams_eu_records: Fv | null;
  coverage: {
    rows: string[];
    cols: StageLabel[];
    /** records_specific per technology x stage */
    specific: Fv[][];
    /** records_tech per technology */
    tech_any_stage: Fv[];
    /** Critical Minerals records per stage (a PAMS tag outside the 33 sectors) */
    critical_minerals: Fv[];
  };
  agreement: {
    variation: boolean;
    n: number;
    k: number;
    /** keys <priority third>_<policy third> -> cell keys by priority rank */
    matrix: Record<string, string[]>;
    by_class: Record<AgreementClass, string[]>;
  } | null;
  /** additive 2026-10-12: GTA NIPO measures in force by sector (null in exports without a NIPO run) */
  nipo?: NipoFacts | null;
}

export interface PartnerTies {
  imp_mean: Fv;
  exp_mean: Fv;
  tie: Fv;
  tie_rank: Fv;
  top3_import_cells: Fv;
  n_import_cells: Fv;
  fdi_in: Fv;
  /** the partner is a China-parent home (CHN, HKG, MAC) */
  fdi_in_china_parent: boolean;
  fdi_out: Fv;
  agreements: string[];
  alignment: Fv;
  alignment_d: string;
  bloc: string | null;
  alignment_basis: AlignmentBasis | null;
}

export interface PartnerRow {
  iso3: string;
  name: string;
  tier: Tier;
  group: PartnerGroup;
  income_group: string | null;
  rank: Fv;
  rankable: boolean;
  rank_reason: string | null;
  psi: Fv;
  fs: Fv;
  po: Fv;
  dp: Fv;
  reason: PartnerReason | null;
  reason_d: string | null;
  fs_best5: string[];
  po_best5: string[];
  ties: PartnerTies;
  in_psi_top: boolean;
  in_tie_top: boolean;
  match: PartnerMatch;
  /** why the row is in the sheet */
  listed_for: ('psi' | 'tie' | 'matrix')[];
}

export interface PartnersFacts {
  n_scored: Fv;
  n_ranked: Fv;
  /** ISO3 of ranked partners by rank (first 25) */
  psi_order: string[];
  /** ISO3 by tie_rank (first 25) */
  tie_order: string[];
  n_import_cells: Fv;
  n_export_cells: Fv;
  rows: PartnerRow[];
  supplier_matrix: {
    partners: string[];
    cells: string[];
    /** share of the country's imports in the cell (null: no imports from that partner) */
    share: (number | null)[][];
    value: (number | null)[][];
    status: (FlowStatus | null)[][];
  };
}

export interface NetworkFacts {
  member: boolean;
  node: string | null;
  via_eu: boolean;
  n_stages: Fv | null;
  producer: { cell: string; share: Fv; derived: boolean }[];
  /** additive 2026-10-14: `excluded` = technologies left out of every allied pick by rule (green hydrogen,
   *  geothermal); `rule_d` = the pick rule in words */
  pick: { cell: string; si: Fv; runner_up: string | null; margin: Fv; near_tie: boolean; excluded?: string[]; rule_d?: string } | null;
}

export interface FactSheet {
  schema: typeof COUNTRY_FACTS_SCHEMA;
  schema_version: number;
  iso3: string;
  meta: SheetMeta;
  words: Words;
  src_map: { prefix: string; src: string }[];
  summary: Summary;
  cells: CellFacts[];
  priority_sectors: string[];
  priority_rule: PriorityRule;
  policy: PolicyFacts;
  partners: PartnersFacts;
  network: NetworkFacts;
  caveats: { id: string; text: string }[];
  sources: SourceRef[];
  checks: { built_with: string; facts_sha256: string };
}

// ---- Memo (B7.2) ------------------------------------------------------------------------------------

export type MemoInline =
  | { t: 'text'; v: string }
  | { t: 'strong'; v: string }
  | { t: 'em'; v: string }
  | { t: 'value'; v: string; path: string; claim: string; src?: number | null };

export interface MemoItem {
  inlines: MemoInline[];
  refs: number[];
}

export type MemoBlock = ({ type: 'p' } & MemoItem) | { type: 'ul'; items: MemoItem[] };

export interface MemoSection {
  n: number;
  title: string;
  blocks: MemoBlock[];
}

export interface MemoDoc {
  schema: typeof COUNTRY_MEMO_SCHEMA;
  schema_version: number;
  iso3: string;
  title: string;
  subtitle: string;
  label: string;
  as_of: string;
  as_of_label: string;
  generated_at: string;
  verified_at: string;
  writer: string;
  verifier: string;
  words: number;
  facts_sha256: string;
  src_sha256: string;
  claims_sha256: string;
  summary: MemoItem[];
  sections: MemoSection[];
  sources: { n: number; id: string; text: string }[];
  /** true on the synthetic fixture's placeholder memo */
  placeholder?: boolean;
  /** additive 2026-10-13: the closing line under the label ("Allied Industrial Base Explorer, Country Insights: {Country}") */
  footer?: string;
}

/** The five memo sections, verbatim and in order (B2). */
export const MEMO_SECTIONS = [
  'Energy security risks and why',
  'Economic opportunities and why',
  'Sectoral priorities and the existing policy posture',
  'Partnership priorities and why',
  'Data notes and caveats',
] as const;

// ---- Runtime checks ---------------------------------------------------------------------------------

export class CountryContractError extends Error {
  readonly problems: string[];
  constructor(what: string, problems: string[]) {
    super(`${what}: ${problems.slice(0, 5).join('; ')}${problems.length > 5 ? ` (+${problems.length - 5} more)` : ''}`);
    this.name = 'CountryContractError';
    this.problems = problems;
  }
}

type Obj = Record<string, unknown>;
const isObj = (x: unknown): x is Obj => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string';
const isNumber = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);
const isStrArr = (x: unknown): x is string[] => Array.isArray(x) && x.every(isStr);

/** An Fv: { v: finite number | null, d: string }, d non-empty when v is not null. */
export function isFv(x: unknown): x is Fv {
  if (!isObj(x) || !isStr(x.d)) return false;
  if (x.v === null) return true;
  return isNumber(x.v) && x.d.length > 0;
}

class Problems {
  readonly list: string[] = [];
  add(msg: string): void {
    if (this.list.length < 200) this.list.push(msg);
  }
  fv(x: unknown, path: string, opts: { nullable?: boolean; min?: number; max?: number; int?: boolean } = {}): void {
    if (!isFv(x)) {
      this.add(`${path}: not an Fv {v, d}`);
      return;
    }
    if (x.v === null) return;
    if (opts.min !== undefined && x.v < opts.min - 1e-9) this.add(`${path}: ${x.v} below ${opts.min}`);
    if (opts.max !== undefined && x.v > opts.max + 1e-9) this.add(`${path}: ${x.v} above ${opts.max}`);
    if (opts.int && !Number.isInteger(x.v)) this.add(`${path}: ${x.v} is not an integer`);
  }
  str(x: unknown, path: string, nullable = false): void {
    if (!(isStr(x) || (nullable && x === null))) this.add(`${path}: expected a string${nullable ? ' or null' : ''}`);
  }
  bool(x: unknown, path: string): void {
    if (typeof x !== 'boolean') this.add(`${path}: expected true or false`);
  }
  oneOf<T>(x: unknown, vals: readonly T[], path: string, nullable = false): void {
    if (nullable && x === null) return;
    if (!vals.includes(x as T)) this.add(`${path}: '${String(x)}' is not one of ${vals.join(', ')}`);
  }
}

function checkSource(p: Problems, s: unknown, path: string): void {
  if (!isObj(s)) return p.add(`${path}: not an object`);
  p.str(s.id, `${path}.id`);
  p.str(s.label, `${path}.label`);
  for (const k of ['edition', 'licence', 'url']) p.str(s[k], `${path}.${k}`, true);
}

/** Problems with a country manifest (empty when it is valid). */
export function checkCountryManifest(x: unknown): string[] {
  const p = new Problems();
  if (!isObj(x)) return ['manifest: not an object'];
  if (x.schema !== COUNTRY_SCHEMA) p.add(`manifest.schema: expected '${COUNTRY_SCHEMA}'`);
  if (!isNumber(x.schema_version)) p.add('manifest.schema_version: not a number');
  else if (Math.floor(x.schema_version) > COUNTRY_SCHEMA_VERSION) p.add(`manifest: schema_version ${x.schema_version} needs a newer app`);
  for (const k of ['created', 'as_of', 'as_of_label', 'label']) p.str(x[k], `manifest.${k}`);
  for (const k of ['git_commit', 'strategic_bundle_id', 'sector_bundle_id']) p.str(x[k], `manifest.${k}`, true);
  p.bool(x.validated, 'manifest.validated');
  if (!isObj(x.counts)) p.add('manifest.counts: not an object');
  if (!Array.isArray(x.countries)) p.add('manifest.countries: not an array');
  else {
    const seen = new Set<string>();
    x.countries.forEach((c, i) => {
      const at = `manifest.countries[${i}]`;
      if (!isObj(c)) return p.add(`${at}: not an object`);
      if (!isStr(c.iso3) || !/^[A-Z0-9]{3}$/.test(c.iso3)) p.add(`${at}.iso3: not an ISO3 code`);
      else if (seen.has(c.iso3)) p.add(`${at}.iso3: duplicate ${c.iso3}`);
      else seen.add(c.iso3);
      p.str(c.name, `${at}.name`);
      p.oneOf(c.tier, TIERS, `${at}.tier`);
      p.oneOf(c.status, SHEET_STATUSES, `${at}.status`);
      p.str(c.region, `${at}.region`, true);
      p.bool(c.eu_member, `${at}.eu_member`);
      if (c.status === 'no_data') {
        if (c.file !== null) p.add(`${at}.file: an economy without data has no fact sheet`);
      } else if (!isStr(c.file) || !/^facts-[A-Z0-9]{3}\.[0-9a-f]{8}\.json$/.test(c.file)) p.add(`${at}.file: not a fact-sheet file name`);
      if (c.memo !== null && c.memo !== undefined) {
        const m = c.memo;
        if (!isObj(m)) p.add(`${at}.memo: not an object`);
        else {
          if (!isStr(m.json) || !/^memo-[A-Z0-9]{3}\.[0-9a-f]{8}\.json$/.test(m.json)) p.add(`${at}.memo.json: not a memo file name`);
          for (const k of ['pdf', 'docx', 'md']) p.str(m[k], `${at}.memo.${k}`, true);
          p.bool(m.stale, `${at}.memo.stale`);
          if (c.tier !== 'core') p.add(`${at}.memo: memos are for core members only`);
        }
      }
      if (c.quadrants !== null && c.quadrants !== undefined) {
        if (!isObj(c.quadrants) || !QUADRANT_IDS.every((q) => isNumber((c.quadrants as Obj)[q]))) p.add(`${at}.quadrants: expected four counts`);
      }
    });
  }
  if (!Array.isArray(x.sources)) p.add('manifest.sources: not an array');
  else x.sources.forEach((s, i) => checkSource(p, s, `manifest.sources[${i}]`));
  if (!Array.isArray(x.validation)) p.add('manifest.validation: not an array');
  return p.list;
}

/** Recompute the quadrant of a placed cell from its values and medians (Space view regionOf()). */
export function quadrantOf(es: number, esMed: number, eo: number, eoMed: number): Quadrant {
  const highEo = eo >= eoMed;
  const highEs = es >= esMed;
  return highEo ? (highEs ? 'grow' : 'upgrade') : highEs ? 'watch' : 'secure';
}

function checkPillar(p: Problems, x: unknown, path: string): x is PillarFacts {
  if (x === null) return false;
  if (!isObj(x)) {
    p.add(`${path}: not an object`);
    return false;
  }
  p.fv(x, path, { min: 0, max: 1 });
  p.bool(x.rankable, `${path}.rankable`);
  p.str(x.reason, `${path}.reason`, true);
  p.fv(x.imputed_share, `${path}.imputed_share`, { min: 0, max: 1 });
  p.fv(x.pct, `${path}.pct`, { min: 0, max: 100 });
  if (!(x.pos === null || (isNumber(x.pos) && x.pos >= 0 && x.pos <= 100))) p.add(`${path}.pos: not a number in 0..100 or null`);
  p.fv(x.med, `${path}.med`, { min: 0, max: 1 });
  p.fv(x.n, `${path}.n`, { min: 0, int: true });
  p.fv(x.n_others, `${path}.n_others`, { min: 0, int: true });
  p.fv(x.order, `${path}.order`, { min: 1, int: true });
  p.oneOf(x.side, ['above', 'below'], `${path}.side`, true);
  return true;
}

function checkDrivers(p: Problems, xs: unknown, path: string): void {
  if (!Array.isArray(xs)) return p.add(`${path}: not an array`);
  const keys = new Set<string>();
  let wsum = 0;
  xs.forEach((d, i) => {
    const at = `${path}[${i}]`;
    if (!isObj(d)) return p.add(`${at}: not an object`);
    if (!isStr(d.key) || !d.key) p.add(`${at}.key: empty`);
    else if (keys.has(d.key)) p.add(`${at}.key: duplicate ${d.key}`);
    else keys.add(d.key);
    p.str(d.category, `${at}.category`);
    for (const k of ['score', 'median', 'weight', 'impact']) p.fv(d[k], `${at}.${k}`);
    if (isFv(d.weight) && d.weight.v !== null) wsum += d.weight.v;
    p.bool(d.imputed, `${at}.imputed`);
    p.bool(d.top, `${at}.top`);
    p.bool(d.constant, `${at}.constant`);
    if (d.top === true && d.constant === true) p.add(`${at}: a constant category is never a top driver`);
    if (!Array.isArray(d.vars)) p.add(`${at}.vars: not an array`);
    else
      d.vars.forEach((v, j) => {
        const vt = `${at}.vars[${j}]`;
        if (!isObj(v)) return p.add(`${vt}: not an object`);
        p.str(v.key, `${vt}.key`);
        p.str(v.label, `${vt}.label`);
        for (const k of ['score', 'median', 'pct', 'weight']) p.fv(v[k], `${vt}.${k}`);
        if (v.raw !== null) {
          if (!isFv(v.raw)) p.add(`${vt}.raw: not an Fv`);
          else {
            for (const k of ['unit', 'year', 'source', 'source_label', 'note']) p.str((v.raw as unknown as Obj)[k], `${vt}.raw.${k}`, true);
            if ((v.raw as unknown as Obj).note_d !== undefined) p.str((v.raw as unknown as Obj).note_d, `${vt}.raw.note_d`, true);
          }
        }
        p.str(v.sector_link, `${vt}.sector_link`, true);
      });
  });
  if (xs.length && Math.abs(wsum - 1) > 1e-6) p.add(`${path}: category weights sum to ${wsum.toFixed(6)}, not 1`);
}

const ordinalOk = (xs: unknown, set: Set<string>, path: string, p: Problems): xs is string[] => {
  if (!isStrArr(xs)) {
    p.add(`${path}: not an array of strings`);
    return false;
  }
  for (const k of xs) if (!set.has(k)) p.add(`${path}: '${k}' is not a cell of the sheet`);
  if (new Set(xs).size !== xs.length) p.add(`${path}: duplicate keys`);
  return true;
};

/**
 * Problems with a fact sheet (empty when it is valid): shapes, Fv values, enums, cell keys, ranks,
 * percentiles, quadrants re-derived from v and med, pos on the right side of 50, SI parts summing to
 * SI, summary counts and order lists consistent with the cells, unique partner ranks, enum words.
 * `entry` (the manifest row) adds the iso3 / tier / status agreement.
 */
export function checkFactSheet(x: unknown, entry?: CountryEntry): string[] {
  const p = new Problems();
  if (!isObj(x)) return ['sheet: not an object'];
  if (x.schema !== COUNTRY_FACTS_SCHEMA) p.add(`schema: expected '${COUNTRY_FACTS_SCHEMA}'`);
  if (!isNumber(x.schema_version)) p.add('schema_version: not a number');
  else if (Math.floor(x.schema_version) > COUNTRY_SCHEMA_VERSION) p.add(`schema_version ${x.schema_version} needs a newer app`);
  if (!isStr(x.iso3)) p.add('iso3: not a string');
  const meta = x.meta;
  if (!isObj(meta)) {
    p.add('meta: not an object');
    return p.list;
  }
  p.str(meta.country, 'meta.country');
  p.oneOf(meta.tier, TIERS, 'meta.tier');
  p.oneOf(meta.status, SHEET_STATUSES, 'meta.status');
  p.bool(meta.has_si, 'meta.has_si');
  p.bool(meta.eu_member, 'meta.eu_member');
  p.bool(meta.sovereign, 'meta.sovereign');
  p.bool(meta.memo_eligible, 'meta.memo_eligible');
  if (!isStrArr(meta.agreements)) p.add('meta.agreements: not an array of strings');
  if (!isStrArr(meta.routes)) p.add('meta.routes: not an array of strings');
  for (const k of ['as_of', 'as_of_label', 'created', 'trade_year', 'un_window', 'cmfdi_edition']) p.str(meta[k], `meta.${k}`);
  for (const k of ['region', 'income_group', 'bloc', 'policy_basis', 'status_reason', 'network_node']) p.str(meta[k], `meta.${k}`, true);
  p.fv(meta.rank_threshold, 'meta.rank_threshold');
  p.fv(meta.alignment_s, 'meta.alignment_s');
  if (entry) {
    if (entry.iso3 !== x.iso3) p.add(`iso3: ${String(x.iso3)} does not match the manifest's ${entry.iso3}`);
    if (entry.tier !== meta.tier) p.add('meta.tier: does not match the manifest');
    if (entry.status !== meta.status) p.add('meta.status: does not match the manifest');
  }

  const words = x.words;
  if (!isObj(words)) p.add('words: not an object');
  const wordOf = (group: string, key: string, path: string) => {
    if (!isObj(words)) return;
    const g = words[group];
    if (!isObj(g) || !isStr(g[key])) p.add(`${path}: no word for ${group}.${key}`);
  };

  if (!Array.isArray(x.src_map)) p.add('src_map: not an array');

  // cells
  const cells = x.cells;
  const keys = new Set<string>();
  const byKey = new Map<string, Obj>();
  if (!Array.isArray(cells)) p.add('cells: not an array');
  else {
    cells.forEach((c, i) => {
      const at = `cells[${i}]`;
      if (!isObj(c)) return p.add(`${at}: not an object`);
      if (!isCellKey(c.key)) p.add(`${at}.key: '${String(c.key)}' is not a sector key`);
      else if (keys.has(c.key)) p.add(`${at}.key: duplicate ${c.key}`);
      else {
        keys.add(c.key);
        byKey.set(c.key, c);
      }
      p.str(c.label, `${at}.label`);
      p.str(c.label_prose, `${at}.label_prose`);
      p.oneOf(c.stage, COUNTRY_STAGES.map((s) => s.label), `${at}.stage`);
      const hasEs = checkPillar(p, c.es, `${at}.es`);
      const hasEo = checkPillar(p, c.eo, `${at}.eo`);
      if (isCellKey(c.key) && NO_ES_CELLS.includes(c.key) && c.es !== null) p.add(`${at}.es: the paper has no Energy Security score for ${c.key}`);
      p.bool(c.placed, `${at}.placed`);
      p.oneOf(c.quadrant, QUADRANT_IDS, `${at}.quadrant`, true);
      if (c.placed === true) {
        if (!hasEs || !hasEo) p.add(`${at}: placed without both scores`);
        else {
          const es = c.es as PillarFacts;
          const eo = c.eo as PillarFacts;
          if (!es.rankable || !eo.rankable) p.add(`${at}: placed but a score is not rankable`);
          if (es.v === null || eo.v === null || es.med.v === null || eo.med.v === null) p.add(`${at}: placed without values or medians`);
          else {
            const q = quadrantOf(es.v, es.med.v, eo.v, eo.med.v);
            if (c.quadrant !== q) p.add(`${at}.quadrant: '${String(c.quadrant)}' but the values give '${q}'`);
            for (const [pl, nm] of [[es, 'es'], [eo, 'eo']] as const) {
              if (pl.pos !== null && pl.v !== null && pl.med.v !== null && (pl.pos >= 50) !== (pl.v >= pl.med.v)) p.add(`${at}.${nm}.pos: on the wrong side of 50`);
              if (pl.side !== null && pl.v !== null && pl.med.v !== null && (pl.side === 'above') !== (pl.v >= pl.med.v)) p.add(`${at}.${nm}.side: wrong`);
            }
          }
        }
      } else if (c.quadrant !== null) p.add(`${at}.quadrant: set on a cell that is not placed`);
      if (c.quadrant) wordOf('quadrant', String(c.quadrant), `${at}.quadrant`);
      // SI
      const si = c.si;
      if (!isObj(si)) p.add(`${at}.si: not an object`);
      else {
        p.bool(si.in_sample, `${at}.si.in_sample`);
        p.fv(si, `${at}.si`);
        p.fv(si.rank, `${at}.si.rank`, { min: 1, int: true });
        p.fv(si.n, `${at}.si.n`, { min: 0, int: true });
        p.fv(si.margin_next, `${at}.si.margin_next`);
        p.bool(si.top5, `${at}.si.top5`);
        p.bool(si.pick, `${at}.si.pick`);
        if (si.in_sample === true) {
          if (!isFv(si) || si.v === null) p.add(`${at}.si: in sample without a value`);
          const r = si.rank as Fv;
          const n = si.n as Fv;
          if (isFv(r) && isFv(n) && r.v !== null && n.v !== null && r.v > n.v) p.add(`${at}.si.rank: ${r.v} above n ${n.v}`);
          const parts = si.parts;
          if (!isObj(parts)) p.add(`${at}.si.parts: missing for an in-sample sector`);
          else {
            let s = 0;
            for (const k of ['eo', 'need', 'pol', 'stage', 'tech']) {
              p.fv(parts[k], `${at}.si.parts.${k}`);
              if (isFv(parts[k]) && (parts[k] as Fv).v !== null) s += (parts[k] as Fv).v as number;
            }
            if (isFv(si) && si.v !== null && Math.abs(s - si.v) > 1e-9) p.add(`${at}.si.parts: sum ${s} differs from the Strategic Index ${si.v}`);
          }
        } else if (isObj(si) && si.v !== null && si.v !== undefined) p.add(`${at}.si: a value outside the sample`);
      }
      // priority and policy ranks run 1..n over the in-sample sectors (2026-10-14: the rank-offset bug)
      const nIn = isObj(si) && si.in_sample === true && isFv(si.n) ? (si.n as Fv).v : null;
      const withinN = (x: unknown, path: string) => {
        if (nIn !== null && isFv(x) && x.v !== null && x.v > nIn) p.add(`${path}: ${x.v} above the ${nIn} in-sample sectors`);
      };
      if (c.priority !== null && c.priority !== undefined) {
        if (!isObj(c.priority)) p.add(`${at}.priority: not an object`);
        else {
          p.fv(c.priority.v, `${at}.priority.v`);
          p.fv(c.priority.rank, `${at}.priority.rank`, { min: 1 });
          withinN(c.priority.rank, `${at}.priority.rank`);
          p.oneOf(c.priority.third, THIRDS, `${at}.priority.third`, true);
          if (c.priority.third) wordOf('third', String(c.priority.third), `${at}.priority.third`);
        }
      }
      if (c.policy !== null && c.policy !== undefined) {
        if (!isObj(c.policy)) p.add(`${at}.policy: not an object`);
        else {
          for (const k of ['index', 'pol', 'rank', 'records_specific', 'records_tech', 'records_all', 'latest_year']) p.fv(c.policy[k], `${at}.policy.${k}`);
          withinN(c.policy.rank, `${at}.policy.rank`);
          p.oneOf(c.policy.third, THIRDS, `${at}.policy.third`, true);
          p.oneOf(c.policy.class, AGREEMENT_CLASSES, `${at}.policy.class`, true);
          if (c.policy.class) wordOf('class', String(c.policy.class), `${at}.policy.class`);
          if (c.policy.third) wordOf('policy_third', String(c.policy.third), `${at}.policy.third`);
        }
      }
      if (c.trade !== null && c.trade !== undefined) {
        const t = c.trade;
        if (!isObj(t)) p.add(`${at}.trade: not an object`);
        else {
          p.bool(t.has_basket, `${at}.trade.has_basket`);
          if (t.has_basket === true) {
            p.fv(t.imports, `${at}.trade.imports`, { min: 0 });
            p.fv(t.exports, `${at}.trade.exports`, { min: 0 });
            for (const side of ['sources', 'markets'] as const) {
              const xs = t[side];
              if (!Array.isArray(xs)) {
                p.add(`${at}.trade.${side}: not an array`);
                continue;
              }
              let total = 0;
              xs.forEach((r, j) => {
                if (!isObj(r)) return p.add(`${at}.trade.${side}[${j}]: not an object`);
                p.str(r.iso3, `${at}.trade.${side}[${j}].iso3`);
                p.str(r.name, `${at}.trade.${side}[${j}].name`);
                p.oneOf(r.group, PARTNER_GROUPS, `${at}.trade.${side}[${j}].group`);
                p.oneOf(r.status, ['importer', 'mirror'], `${at}.trade.${side}[${j}].status`);
                p.fv(r.share, `${at}.trade.${side}[${j}].share`, { min: 0, max: 1 });
                if (isFv(r.share) && r.share.v !== null) total += r.share.v;
              });
              if (total > 1 + 1e-9) p.add(`${at}.trade.${side}: shares sum to ${total}`);
            }
          }
        }
      }
      const dr = c.drivers;
      if (!isObj(dr)) p.add(`${at}.drivers: not an object`);
      else {
        checkDrivers(p, dr.es, `${at}.drivers.es`);
        checkDrivers(p, dr.eo, `${at}.drivers.eo`);
      }
    });
  }

  // summary
  const sm = x.summary;
  const list = Array.isArray(cells) ? (cells.filter(isObj) as unknown as CellFacts[]) : [];
  if (!isObj(sm)) p.add('summary: not an object');
  else if (list.length) {
    const placed = list.filter((c) => c.placed);
    const scored = list.filter((c) => c.es && c.eo && c.es.v !== null && c.eo.v !== null);
    const expect = (fv: unknown, n: number, path: string) => {
      if (!isFv(fv)) p.add(`${path}: not an Fv`);
      else if (fv.v !== n) p.add(`${path}: ${String(fv.v)} but the cells give ${n}`);
    };
    expect(sm.n_cells, list.length, 'summary.n_cells');
    expect(sm.n_scored, scored.length, 'summary.n_scored');
    expect(sm.n_placed, placed.length, 'summary.n_placed');
    const qc: Record<Quadrant, number> = { grow: 0, upgrade: 0, secure: 0, watch: 0 };
    for (const c of placed) if (c.quadrant) qc[c.quadrant]++;
    if (!isObj(sm.quadrants)) p.add('summary.quadrants: not an object');
    else for (const q of QUADRANT_IDS) expect(sm.quadrants[q], qc[q], `summary.quadrants.${q}`);
    expect(sm.n_insecure, qc.upgrade + qc.secure, 'summary.n_insecure');
    expect(sm.n_high_eo, qc.grow + qc.upgrade, 'summary.n_high_eo');
    const placedKeys = new Set(placed.map((c) => c.key));
    const orderCheck = (name: 'es_order' | 'eo_order', pillar: 'es' | 'eo', dir: 1 | -1) => {
      const xs = sm[name];
      if (!ordinalOk(xs, keys, `summary.${name}`, p)) return;
      if (xs.length !== placedKeys.size || !xs.every((k) => placedKeys.has(k))) p.add(`summary.${name}: not the placed cells`);
      for (let i = 1; i < xs.length; i++) {
        const a = byKey.get(xs[i - 1]) as unknown as CellFacts | undefined;
        const b = byKey.get(xs[i]) as unknown as CellFacts | undefined;
        const va = a?.[pillar]?.pct.v;
        const vb = b?.[pillar]?.pct.v;
        if (va === null || va === undefined || vb === null || vb === undefined) continue;
        if (dir * (vb - va) < -1e-9) p.add(`summary.${name}: ${xs[i]} is out of order`);
        else if (Math.abs(vb - va) <= 1e-9 && cellOrder(xs[i]) < cellOrder(xs[i - 1])) p.add(`summary.${name}: tie between ${xs[i - 1]} and ${xs[i]} not in cell order`);
      }
      xs.forEach((k, i) => {
        const c = byKey.get(k) as unknown as CellFacts | undefined;
        const o = c?.[pillar]?.order;
        if (o && o.v !== i + 1) p.add(`cells.${k}.${pillar}.order: ${String(o.v)} but position ${i + 1} in summary.${name}`);
      });
    };
    orderCheck('es_order', 'es', 1);
    orderCheck('eo_order', 'eo', -1);
    const sio = sm.si_order;
    if (ordinalOk(sio, keys, 'summary.si_order', p)) {
      const ins = list.filter((c) => c.si?.in_sample);
      if (sio.length !== ins.length) p.add('summary.si_order: not the in-sample cells');
      sio.forEach((k, i) => {
        const c = byKey.get(k) as unknown as CellFacts | undefined;
        if (!c?.si?.in_sample) p.add(`summary.si_order: ${k} is not in the Strategic Index sample`);
        else if (i > 0) {
          const prev = byKey.get(sio[i - 1]) as unknown as CellFacts | undefined;
          if (prev?.si?.rank && (c.si.rank.v ?? 0) < (prev.si.rank.v ?? 0)) p.add(`summary.si_order: ${k} is out of rank order`);
        }
      });
      const n = ins.length;
      for (const c of ins) if (c.si.n.v !== n) p.add(`cells.${c.key}.si.n: ${String(c.si.n.v)} but ${n} sectors are in sample`);
    }
    if (sm.top_si !== null && sm.top_si !== undefined) {
      const t = sm.top_si as Obj;
      if (!isObj(t) || !isCellKey(t.cell)) p.add('summary.top_si.cell: not a sector key');
      else if (isStrArr(sio) && sio[0] !== t.cell) p.add('summary.top_si.cell: not the first of si_order');
    }
  }

  // priority sectors
  if (!isStrArr(x.priority_sectors)) p.add('priority_sectors: not an array of strings');
  else for (const k of x.priority_sectors) if (!keys.has(k)) p.add(`priority_sectors: '${k}' is not a cell of the sheet`);
  p.oneOf(x.priority_rule, PRIORITY_RULES, 'priority_rule');

  // policy
  const pol = x.policy;
  if (!isObj(pol)) p.add('policy: not an object');
  else {
    for (const k of ['pams_total', 'pams_tech_specific', 'pams_no_tech', 'pams_critical_minerals']) p.fv(pol[k], `policy.${k}`, { min: 0, int: true });
    if (pol.cat !== null) {
      if (!isObj(pol.cat)) p.add('policy.cat: not an object');
      else {
        p.str(pol.cat.overall, 'policy.cat.overall');
        p.oneOf(pol.cat.scope, ['national', 'eu_bloc'], 'policy.cat.scope');
      }
    }
    const cov = pol.coverage;
    if (!isObj(cov) || !Array.isArray(cov.specific) || !isStrArr(cov.rows)) p.add('policy.coverage: malformed');
    else if (cov.specific.length !== cov.rows.length) p.add('policy.coverage.specific: one row per technology expected');
    const ag = pol.agreement;
    if (ag !== null && ag !== undefined) {
      if (!isObj(ag)) p.add('policy.agreement: not an object');
      else {
        p.bool(ag.variation, 'policy.agreement.variation');
        if (isObj(ag.matrix)) {
          for (const k of Object.keys(ag.matrix)) {
            if (!MATRIX_KEYS.includes(k)) p.add(`policy.agreement.matrix: unknown key ${k}`);
            const v = ag.matrix[k];
            if (isStrArr(v)) for (const c of v) if (!keys.has(c)) p.add(`policy.agreement.matrix.${k}: '${c}' is not a cell`);
          }
        } else p.add('policy.agreement.matrix: not an object');
        if (isObj(ag.by_class)) {
          for (const k of Object.keys(ag.by_class)) if (!AGREEMENT_CLASSES.includes(k as AgreementClass)) p.add(`policy.agreement.by_class: unknown class ${k}`);
        } else p.add('policy.agreement.by_class: not an object');
      }
    }
  }

  // partners
  const pt = x.partners;
  if (!isObj(pt)) p.add('partners: not an object');
  else {
    p.fv(pt.n_scored, 'partners.n_scored');
    p.fv(pt.n_ranked, 'partners.n_ranked');
    if (!isStrArr(pt.psi_order)) p.add('partners.psi_order: not an array of strings');
    if (!isStrArr(pt.tie_order)) p.add('partners.tie_order: not an array of strings');
    const rows = pt.rows;
    if (!Array.isArray(rows)) p.add('partners.rows: not an array');
    else {
      const ranks = new Set<number>();
      const isos = new Set<string>();
      const rowByIso = new Map<string, Obj>();
      rows.forEach((r, i) => {
        const at = `partners.rows[${i}]`;
        if (!isObj(r)) return p.add(`${at}: not an object`);
        if (!isStr(r.iso3)) p.add(`${at}.iso3: not a string`);
        else if (isos.has(r.iso3)) p.add(`${at}.iso3: duplicate ${r.iso3}`);
        else {
          isos.add(r.iso3);
          rowByIso.set(r.iso3, r);
        }
        if (r.iso3 === x.iso3) p.add(`${at}: the country is not its own partner`);
        p.str(r.name, `${at}.name`);
        p.oneOf(r.group, PARTNER_GROUPS, `${at}.group`);
        p.oneOf(r.tier, TIERS, `${at}.tier`);
        p.bool(r.rankable, `${at}.rankable`);
        for (const k of ['rank', 'psi', 'fs', 'po', 'dp']) p.fv(r[k], `${at}.${k}`);
        const rk = r.rank as Fv;
        if (isFv(rk) && rk.v !== null) {
          if (ranks.has(rk.v)) p.add(`${at}.rank: duplicate rank ${rk.v}`);
          ranks.add(rk.v);
          if (r.rankable !== true) p.add(`${at}.rank: a rank on a partner that is not rankable`);
          if (isFv(pt.n_ranked) && pt.n_ranked.v !== null && (rk.v < 1 || rk.v > pt.n_ranked.v)) p.add(`${at}.rank: outside 1..${pt.n_ranked.v}`);
        }
        p.oneOf(r.reason, PARTNER_REASONS, `${at}.reason`, true);
        if (r.reason) wordOf('reason', String(r.reason), `${at}.reason`);
        p.oneOf(r.match, PARTNER_MATCHES, `${at}.match`, true);
        if (r.match) wordOf('match', String(r.match), `${at}.match`);
        for (const k of ['fs_best5', 'po_best5']) {
          const b = r[k];
          if (!isStrArr(b)) p.add(`${at}.${k}: not an array of strings`);
          else for (const c of b) if (!isCellKey(c)) p.add(`${at}.${k}: '${c}' is not a sector key`);
        }
        const t = r.ties;
        if (!isObj(t)) p.add(`${at}.ties: not an object`);
        else {
          for (const k of ['imp_mean', 'exp_mean', 'tie']) p.fv(t[k], `${at}.ties.${k}`, { min: 0, max: 1 });
          for (const k of ['tie_rank', 'top3_import_cells', 'n_import_cells', 'fdi_in', 'fdi_out', 'alignment']) p.fv(t[k], `${at}.ties.${k}`);
          if (!isStrArr(t.agreements)) p.add(`${at}.ties.agreements: not an array of strings`);
          else for (const a of t.agreements) wordOf('agreement', a, `${at}.ties.agreements`);
          p.str(t.alignment_d, `${at}.ties.alignment_d`);
        }
      });
      if (isStrArr(pt.psi_order))
        pt.psi_order.forEach((iso, i) => {
          const r = rowByIso.get(iso);
          if (r && isFv(r.rank) && r.rank.v !== i + 1) p.add(`partners.psi_order: ${iso} at position ${i + 1} has rank ${String(r.rank.v)}`);
        });
      if (isStrArr(pt.tie_order))
        pt.tie_order.forEach((iso, i) => {
          const r = rowByIso.get(iso);
          const tr = r && isObj(r.ties) ? (r.ties.tie_rank as Fv) : null;
          if (tr && isFv(tr) && tr.v !== null && tr.v !== i + 1) p.add(`partners.tie_order: ${iso} at position ${i + 1} has tie rank ${tr.v}`);
        });
    }
    const sm2 = pt.supplier_matrix;
    if (!isObj(sm2) || !isStrArr(sm2.partners) || !isStrArr(sm2.cells) || !Array.isArray(sm2.share)) p.add('partners.supplier_matrix: malformed');
    else if (sm2.share.length !== sm2.partners.length) p.add('partners.supplier_matrix.share: one row per partner expected');
  }

  // network, caveats, sources, checks
  if (!isObj(x.network)) p.add('network: not an object');
  else p.bool(x.network.member, 'network.member');
  if (!Array.isArray(x.caveats)) p.add('caveats: not an array');
  else
    x.caveats.forEach((c, i) => {
      if (!isObj(c) || !isStr(c.id) || !isStr(c.text) || !c.text) p.add(`caveats[${i}]: expected { id, text }`);
      else if (!(CAVEAT_IDS as readonly string[]).includes(c.id)) p.add(`caveats[${i}].id: unknown caveat '${c.id}'`);
    });
  if (!Array.isArray(x.sources)) p.add('sources: not an array');
  else x.sources.forEach((s, i) => checkSource(p, s, `sources[${i}]`));
  if (!isObj(x.checks) || !isStr(x.checks.facts_sha256)) p.add('checks.facts_sha256: missing');
  return p.list;
}

/** A fact sheet, or CountryContractError. */
export function validateFactSheet(x: unknown, entry?: CountryEntry): FactSheet {
  const problems = checkFactSheet(x, entry);
  if (problems.length) throw new CountryContractError(`Fact sheet${entry ? ` ${entry.iso3}` : ''}`, problems);
  return x as FactSheet;
}

/** A manifest, or CountryContractError. */
export function validateCountryManifest(x: unknown): CountryManifest {
  const problems = checkCountryManifest(x);
  if (problems.length) throw new CountryContractError('Country manifest', problems);
  return x as CountryManifest;
}

function checkInlines(p: Problems, xs: unknown, path: string): void {
  if (!Array.isArray(xs)) return p.add(`${path}: not an array`);
  xs.forEach((n, i) => {
    if (!isObj(n)) return p.add(`${path}[${i}]: not an object`);
    p.oneOf(n.t, ['text', 'strong', 'em', 'value'], `${path}[${i}].t`);
    p.str(n.v, `${path}[${i}].v`);
    if (n.t === 'value') {
      p.str(n.path, `${path}[${i}].path`);
      p.str(n.claim, `${path}[${i}].claim`);
    }
  });
}

function checkItem(p: Problems, x: unknown, path: string): void {
  if (!isObj(x)) return p.add(`${path}: not an object`);
  checkInlines(p, x.inlines, `${path}.inlines`);
  if (!Array.isArray(x.refs) || !x.refs.every((r) => Number.isInteger(r))) p.add(`${path}.refs: not an array of integers`);
}

/** Problems with a memo document (empty when it is valid). */
export function checkMemoDoc(x: unknown, iso3?: string): string[] {
  const p = new Problems();
  if (!isObj(x)) return ['memo: not an object'];
  if (x.schema !== COUNTRY_MEMO_SCHEMA) p.add(`memo.schema: expected '${COUNTRY_MEMO_SCHEMA}'`);
  if (!isNumber(x.schema_version) || Math.floor(x.schema_version) > COUNTRY_SCHEMA_VERSION) p.add('memo.schema_version: unsupported');
  if (iso3 && x.iso3 !== iso3) p.add(`memo.iso3: ${String(x.iso3)} is not ${iso3}`);
  for (const k of ['title', 'subtitle', 'label', 'as_of', 'facts_sha256']) p.str(x[k], `memo.${k}`);
  if (isStr(x.label) && !x.label.startsWith(MEMO_LABEL_PREFIX)) p.add('memo.label: not the verbatim label');
  if (!Array.isArray(x.summary)) p.add('memo.summary: not an array');
  else x.summary.forEach((it, i) => checkItem(p, it, `memo.summary[${i}]`));
  if (!Array.isArray(x.sections)) p.add('memo.sections: not an array');
  else {
    if (x.sections.length !== MEMO_SECTIONS.length) p.add(`memo.sections: ${x.sections.length} sections, ${MEMO_SECTIONS.length} expected`);
    x.sections.forEach((s, i) => {
      const at = `memo.sections[${i}]`;
      if (!isObj(s)) return p.add(`${at}: not an object`);
      if (s.title !== MEMO_SECTIONS[i]) p.add(`${at}.title: not '${MEMO_SECTIONS[i]}'`);
      if (!Array.isArray(s.blocks)) return p.add(`${at}.blocks: not an array`);
      s.blocks.forEach((b, j) => {
        if (!isObj(b)) return p.add(`${at}.blocks[${j}]: not an object`);
        if (b.type === 'p') checkItem(p, b, `${at}.blocks[${j}]`);
        else if (b.type === 'ul') {
          if (!Array.isArray(b.items)) p.add(`${at}.blocks[${j}].items: not an array`);
          else b.items.forEach((it, k) => checkItem(p, it, `${at}.blocks[${j}].items[${k}]`));
        } else p.add(`${at}.blocks[${j}].type: '${String(b.type)}'`);
      });
    });
  }
  if (!Array.isArray(x.sources)) p.add('memo.sources: not an array');
  return p.list;
}

/** A memo document, or CountryContractError. */
export function validateMemoDoc(x: unknown, iso3?: string): MemoDoc {
  const problems = checkMemoDoc(x, iso3);
  if (problems.length) throw new CountryContractError(`Memo${iso3 ? ` ${iso3}` : ''}`, problems);
  return x as MemoDoc;
}

// ---- Small shared helpers -----------------------------------------------------------------------------

/** The display string of an Fv (en dash when missing). */
export const dOf = (x: Fv | null | undefined): string => (x && x.d ? x.d : '–');

/** The cell of a sheet by key. */
export function cellOf(sheet: FactSheet, key: string | null | undefined): CellFacts | null {
  if (!key) return null;
  return sheet.cells.find((c) => c.key === key) ?? null;
}

/** The partner row of a sheet by ISO3. */
export function partnerOf(sheet: FactSheet, iso3: string | null | undefined): PartnerRow | null {
  if (!iso3) return null;
  return sheet.partners.rows.find((r) => r.iso3 === iso3) ?? null;
}
