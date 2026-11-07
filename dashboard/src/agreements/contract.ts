// The partnership agreements data contract (docs/partnership_agreements.md section 9; written by
// scripts/37_export_agreement_data.R into public/data/agreements/): the manifest (vocabularies, names,
// counts, caveats and the file names), the columnar global file (one entry per pair with an agreement,
// and per-economy totals) and one file per economy (its agreements, its partners' pair profiles, its
// agreement gaps and execution gaps). Arrays that R exports are wrapped in I(), so a one-item array stays
// an array: the checks below name a collapsed one ("a one-item array collapsed to a string?").
// Pure: types, constants and check functions; no IO.

export const AGREEMENTS_DATA_DIR = 'data/agreements/';
export const AGREEMENTS_MANIFEST_FILE = 'manifest.json';
/** The manifest version this app reads; a newer one asks for a reload. */
export const AGREEMENTS_VERSION = 1;
export const AGR_GLOBAL_FILE_RE = /^agreements-global\.[0-9a-f]{8}\.json$/;
export const AGR_COUNTRY_FILE_RE = /^agreements-([A-Z0-9]{3})\.[0-9a-f]{8}\.json$/;
export const ISO3_RE = /^[A-Z0-9]{3}$/;

/** Depth classes (section 7): 0 none found ... 5 integrated. */
export const DEPTH_CLASSES = [0, 1, 2, 3, 4, 5] as const;
export type Depth = (typeof DEPTH_CLASSES)[number];
export const DATE_PRECISIONS = ['day', 'month', 'year', 'none'] as const;
export type DatePrecision = (typeof DATE_PRECISIONS)[number];
export const CODE_GRADES = ['specific', 'text'] as const;
export type CodeGrade = (typeof CODE_GRADES)[number];
export const COVERED_GRADES = ['specific', 'text', 'none'] as const;
export type CoveredGrade = (typeof COVERED_GRADES)[number];
export const QUADRANTS = ['opportunity', 'established', 'legacy', 'none'] as const;
export type Quadrant = (typeof QUADRANTS)[number];
/** A gap row's complementarity: `not_assessed` when need or capability data are missing for the pair (quadrant none). */
export const COMPLEMENTARITIES = ['high', 'low', 'not_assessed'] as const;
export type Complementarity = (typeof COMPLEMENTARITIES)[number];
export const GAP_SIDES = ['needs', 'supplies'] as const;
export type GapSide = (typeof GAP_SIDES)[number];
/** The six stages plus cross_stage (R AGR_SEGMENT_CROSS); legacy stage names are never stored. */
export const SEGMENTS = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery', 'cross_stage'] as const;
/**
 * The coding bases that make a code "specific" (established) rather than "text" (mentioned): config
 * `specific_bases`. The manifest may carry them as vocab.specific_bases (an additive field); else these.
 */
export const DEFAULT_SPECIFIC_BASES = ['source_coded', 'curated', 'type_implied', 'text_title'] as const;
/**
 * The title-keyword basis: it establishes a sector, never an INSTRUMENT. Instruments are graded with
 * vocab.established_instrument_bases when the manifest carries it; else the specific bases without this one.
 */
export const TITLE_BASIS = 'text_title';
/** The delivery statuses (config delivery_statuses), when the manifest does not list them. */
export const DEFAULT_DELIVERY_STATUSES = ['implementation_underway', 'operational'] as const;

// ---- manifest ---------------------------------------------------------------------------------------------

export interface AgrSectorInfo {
  id: string;
  label: string;
  /** electro-industrial: an agreement covering it is in the electro-industrial view */
  ei: boolean;
  /** dashboard technologies it maps to (empty: shown, not joined to capability data) */
  techs: string[];
}

export interface AgrInstrumentInfo {
  id: string;
  label: string;
  group: string;
  /** a concrete delivery mechanism (counts toward depth class 4 when established) */
  operational: boolean;
}

export interface AgrSourceInfo {
  id: string;
  label: string;
  short: string;
  citation: string;
  url: string;
  licence: string;
  n_records: number;
  last_updated: string | null;
}

export interface AgrVocab {
  agreement_types: Record<string, string>;
  partnership_labels: Record<string, string>;
  legal_statuses: Record<string, string>;
  implementation_statuses: Record<string, string>;
  delivery_statuses: string[];
  coding_bases: Record<string, string>;
  confidence_levels: Record<string, string>;
  depth_classes: Record<string, string>;
  instrument_groups: Record<string, string>;
  sectors: AgrSectorInfo[];
  instruments: AgrInstrumentInfo[];
  sources: AgrSourceInfo[];
  /** additive (not in the section 9 contract yet): config specific_bases; DEFAULT_SPECIFIC_BASES when absent */
  specific_bases?: string[];
  /** additive: the bases at which an instrument counts as established (no text_title); else specific_bases minus text_title */
  established_instrument_bases?: string[];
  /** additive: strategic compact id ("SC-BATTERIES") -> its name ("Battery Materials & Cells"), for the gap rows' shared compacts */
  compacts?: Record<string, string>;
}

export interface AgreementsManifest {
  version: number;
  created: string;
  as_of: string;
  git_commit: string | null;
  files: { global: string; countries: Record<string, string> };
  vocab: AgrVocab;
  names: Record<string, string>;
  /** current EU member states (EU-level agreements are attributed to them via the EU) */
  eu_members?: string[];
  counts: { records: number; canonical: number; pairs: number; countries: number; by_source: Record<string, number> };
  caveats: string[];
}

// ---- global file ------------------------------------------------------------------------------------------

export interface AgreementsGlobal {
  pairs: {
    a: string[];
    b: string[];
    n: number[];
    n_recent: number[];
    newest: (string | null)[];
    depth_all: number[];
    depth_ei: number[];
    has_sp: boolean[];
    has_trade: boolean[];
    has_sectoral: boolean[];
    n_delivery: number[];
    /** "|"-joined sector ids */
    ei_sectors: (string | null)[];
    /** "|"-joined agreement types */
    types: (string | null)[];
    /** "|"-joined source ids */
    sources: (string | null)[];
    // additive (the all-economies filters; a pair passes when any of its agreements has the value): "|"-joined, "" or null for none
    /** distinct years of all the pair's agreements ("2008|2014|2023") */
    years?: (string | null)[];
    /** distinct years of its electro-industrial agreements */
    years_ei?: (string | null)[];
    /** instrument groups established in any of its agreements ("trade|finance") */
    igroups?: (string | null)[];
    /** legal statuses of its agreements ("binding|non_binding|unknown") */
    legal?: (string | null)[];
    /** implementation statuses of its agreements ("in_force|signed|announced") */
    status?: (string | null)[];
    /** stages its agreements cover at an established grade ("Extraction|Refining") */
    stages?: (string | null)[];
  };
  countries: {
    iso3: string[];
    n_partners: number[];
    n_partners_d3: number[];
    n_agreements: number[];
    n_ei_agreements: number[];
    newest: (string | null)[];
  };
}

// ---- country file -----------------------------------------------------------------------------------------

/** [code, basis] */
export type AgrCode = [string, string];

export interface AgrSourceRef {
  dataset: string;
  record_id: string;
  url: string | null;
}

export interface AgrDelivery {
  text: string;
  status: string;
  url: string | null;
}

export interface AgrAgreement {
  id: string;
  title: string;
  parties: string[];
  type: string;
  label: string | null;
  legal: string;
  legal_basis: string | null;
  date: string | null;
  date_precision: DatePrecision;
  signature_date: string | null;
  effective_date: string | null;
  status: string;
  status_raw: string | null;
  category: string | null;
  sectors: AgrCode[];
  instruments: AgrCode[];
  techs: string[];
  segments: string[];
  ei: boolean;
  via_eu: boolean;
  sources: AgrSourceRef[];
  primary_url: string | null;
  primary_available: boolean;
  confidence: string;
  source_count: number;
  delivery: AgrDelivery[];
  trade_depth_index: number | null;
}

/** 0 none, 1 mentioned in text, 2 established */
export type InstLevel = 0 | 1 | 2;

export interface AgrPartner {
  partner: string;
  n: number;
  n_bilateral: number;
  n_plurilateral: number;
  n_via_eu: number;
  n_active: number;
  n_recent: number;
  oldest_active: string | null;
  newest: string | null;
  has_sp: boolean;
  has_trade: boolean;
  has_sectoral: boolean;
  /** [sector id, grade] */
  sectors: [string, CodeGrade][];
  ei_sectors: string[];
  techs: string[];
  segments: string[];
  inst: Record<string, InstLevel>;
  n_announced: number;
  n_signed: number;
  n_in_force: number;
  n_delivery: number;
  share_delivery: number | null;
  trade_depth_index: number | null;
  depth_all: Depth;
  depth_all_rule: string | null;
  depth_ei: Depth;
  depth_ei_rule: string | null;
  sources: string[];
  max_confidence: string | null;
  agreement_ids: string[];
}

export interface AgrGap {
  partner: string;
  tech: string;
  /** the focal country's side: `needs` has it as need_iso3 */
  side: GapSide;
  need: number | null;
  capability: number | null;
  recommended: number;
  friendshore: number | null;
  aib_shared: string[];
  /** `not_assessed`: need or capability data missing for the pair (such rows have quadrant none) */
  complementarity: Complementarity;
  covered_grade: CoveredGrade;
  architecture: 'strong' | 'weak';
  quadrant: Quadrant;
  /** UN-vote alignment of the two economies (1 same bloc, 0.5 one side non-aligned, 0 opposite blocs) */
  alignment?: number | null;
  /** need and capability match, but the economies are in opposite UN-vote blocs, so it does not count */
  alignment_blocked?: boolean;
  /** the stages where complementarity is high (supply-chain order) */
  stages_high?: string[];
  /** the stages an agreement of the pair covers at an established (specific) grade */
  stages_covered?: string[];
}

export interface AgrExecutionGap {
  tech: string;
  /** the priority stages without coverage */
  stages?: string[];
  priority_basis: string;
  priority: number | null;
  suppliers_without_coverage: string[];
}

export interface AgreementsCountry {
  iso3: string;
  agreements: AgrAgreement[];
  partners: AgrPartner[];
  gaps: AgrGap[];
  execution_gaps: AgrExecutionGap[];
}

// ---- checks -----------------------------------------------------------------------------------------------

export class AgreementsContractError extends Error {
  readonly problems: string[];
  constructor(what: string, problems: string[]) {
    super(`${what}: ${problems.slice(0, 5).join('; ')}${problems.length > 5 ? ` (+${problems.length - 5} more)` : ''}`);
    this.name = 'AgreementsContractError';
    this.problems = problems;
  }
}

type Obj = Record<string, unknown>;
const isObj = (x: unknown): x is Obj => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string';
const isNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);
const MAX_PROBLEMS = 200;

class Problems {
  readonly list: string[] = [];
  add(msg: string): void {
    if (this.list.length < MAX_PROBLEMS) this.list.push(msg);
  }
  full(): boolean {
    return this.list.length >= MAX_PROBLEMS;
  }
  str(x: unknown, path: string, nullable = false): void {
    if (!(isStr(x) || (nullable && x === null))) this.add(`${path}: expected a string${nullable ? ' or null' : ''}${Array.isArray(x) ? ' (got an array)' : ''}`);
  }
  num(x: unknown, path: string, opts: { nullable?: boolean; min?: number; max?: number; int?: boolean } = {}): void {
    if (x === null && opts.nullable) return;
    if (!isNum(x)) {
      this.add(`${path}: expected a number${opts.nullable ? ' or null' : ''}${Array.isArray(x) ? ' (got an array)' : ''}`);
      return;
    }
    if (opts.min !== undefined && x < opts.min - 1e-9) this.add(`${path}: ${x} below ${opts.min}`);
    if (opts.max !== undefined && x > opts.max + 1e-9) this.add(`${path}: ${x} above ${opts.max}`);
    if (opts.int && !Number.isInteger(x)) this.add(`${path}: ${x} is not an integer`);
  }
  bool(x: unknown, path: string): void {
    if (typeof x !== 'boolean') this.add(`${path}: expected true or false`);
  }
  arr(x: unknown, path: string): x is unknown[] {
    if (!Array.isArray(x)) {
      this.add(`${path}: expected an array${isStr(x) ? ' (a one-item array collapsed to a string?)' : isObj(x) ? ' (a one-item array collapsed to an object?)' : ''}`);
      return false;
    }
    return true;
  }
  strArr(x: unknown, path: string, vals?: ReadonlySet<string> | readonly string[]): x is string[] {
    if (!this.arr(x, path)) return false;
    let ok = true;
    x.forEach((v, i) => {
      if (!isStr(v)) {
        this.add(`${path}[${i}]: expected a string`);
        ok = false;
      } else if (vals && !has(vals, v)) this.add(`${path}[${i}]: '${v}' is not in the vocabulary`);
    });
    return ok;
  }
  oneOf(x: unknown, vals: ReadonlySet<string> | readonly (string | number)[], path: string): void {
    const ok = Array.isArray(vals) ? (vals as readonly unknown[]).includes(x) : isStr(x) && (vals as ReadonlySet<string>).has(x);
    if (!ok) this.add(`${path}: '${String(x)}' is not one of ${Array.isArray(vals) ? vals.join(', ') : [...(vals as ReadonlySet<string>)].join(', ')}`);
  }
  /** a "|"-joined string field (null or "" for none) whose parts are in `vals`; `graded`: a part may be `id:grade` (R's pair profile form) */
  joined(x: unknown, path: string, vals?: ReadonlySet<string>, graded = false): void {
    if (x === null || x === '') return;
    if (!isStr(x)) return this.add(`${path}: expected a "|"-joined string or null`);
    for (const part of x.split('|').filter(Boolean)) {
      const [v, g] = graded ? splitGraded(part) : [part, null];
      if (vals && !vals.has(v)) this.add(`${path}: '${v}' is not in the vocabulary`);
      if (g !== null && !(CODE_GRADES as readonly string[]).includes(g)) this.add(`${path}: '${part}' has no grade specific or text`);
    }
  }
  /** a named list: an object of string -> string ([] is jsonlite's empty named list) */
  labels(x: unknown, path: string): x is Record<string, string> {
    if (Array.isArray(x) && x.length === 0) return true;
    if (!isObj(x)) {
      this.add(`${path}: expected an object of id -> label`);
      return false;
    }
    for (const [k, v] of Object.entries(x)) if (!isStr(v)) this.add(`${path}.${k}: expected a string`);
    return true;
  }
}

/**
 * A sector of an `ei_sectors` field: section 9 lists ids, R's pair profile (agr_pairs.R) writes `id:grade`; both
 * are read. Returns [id, grade or null].
 */
export function splitGraded(s: string): [string, string | null] {
  const i = s.lastIndexOf(':');
  return i > 0 ? [s.slice(0, i), s.slice(i + 1)] : [s, null];
}

const has = (vals: ReadonlySet<string> | readonly string[], v: string): boolean => (Array.isArray(vals) ? (vals as readonly string[]).includes(v) : (vals as ReadonlySet<string>).has(v));
const keysOf = (x: unknown): Set<string> => new Set(isObj(x) ? Object.keys(x) : []);
const idsOf = (x: unknown): Set<string> => new Set(Array.isArray(x) ? x.flatMap((r) => (isObj(r) && isStr(r.id) ? [r.id] : [])) : []);
const NULLABLE_DATE = /^\d{4}(-\d{2}(-\d{2})?)?$/;

function date(p: Problems, x: unknown, path: string): void {
  if (x === null || x === '') return;
  if (!isStr(x) || !NULLABLE_DATE.test(x)) p.add(`${path}: '${String(x)}' is not a YYYY, YYYY-MM or YYYY-MM-DD date`);
}

/** The vocabularies a file check reads from a manifest (any of them may be missing: the check is then looser). */
interface VocabSets {
  types: Set<string>;
  labels: Set<string>;
  legal: Set<string>;
  statuses: Set<string>;
  bases: Set<string>;
  confidence: Set<string>;
  groups: Set<string>;
  sectors: Set<string>;
  instruments: Set<string>;
  sources: Set<string>;
}

function vocabSets(m: unknown): VocabSets | null {
  if (!isObj(m) || !isObj(m.vocab)) return null;
  const v = m.vocab;
  return {
    types: keysOf(v.agreement_types),
    labels: keysOf(v.partnership_labels),
    legal: keysOf(v.legal_statuses),
    statuses: keysOf(v.implementation_statuses),
    bases: keysOf(v.coding_bases),
    confidence: keysOf(v.confidence_levels),
    groups: keysOf(v.instrument_groups),
    sectors: idsOf(v.sectors),
    instruments: idsOf(v.instruments),
    sources: idsOf(v.sources),
  };
}

/** Problems with data/agreements/manifest.json (empty when it is valid). */
export function checkAgreementsManifest(x: unknown): string[] {
  if (!isObj(x)) return ['manifest: not an object'];
  const p = new Problems();
  if (!isNum(x.version)) p.add('manifest.version: not a number');
  else if (Math.floor(x.version) > AGREEMENTS_VERSION) p.add(`manifest: version ${x.version} needs a newer app`);
  p.str(x.created, 'manifest.created');
  p.str(x.as_of, 'manifest.as_of');
  if (isStr(x.as_of) && !NULLABLE_DATE.test(x.as_of)) p.add(`manifest.as_of: '${x.as_of}' is not a date`);
  p.str(x.git_commit, 'manifest.git_commit', true);
  if (!isObj(x.files)) p.add('manifest.files: not an object');
  else {
    if (!isStr(x.files.global) || !AGR_GLOBAL_FILE_RE.test(x.files.global)) p.add('manifest.files.global: not an agreements-global.<8 hex>.json file name');
    const c = x.files.countries;
    if (Array.isArray(c) && c.length === 0) {
      // an empty named list
    } else if (!isObj(c)) p.add('manifest.files.countries: expected an object of ISO3 -> file name');
    else
      for (const [iso, f] of Object.entries(c)) {
        if (!ISO3_RE.test(iso)) p.add(`manifest.files.countries: '${iso}' is not an ISO3 code`);
        const m = isStr(f) ? AGR_COUNTRY_FILE_RE.exec(f) : null;
        if (!m) p.add(`manifest.files.countries.${iso}: not an agreements-<ISO3>.<8 hex>.json file name`);
        else if (m[1] !== iso) p.add(`manifest.files.countries.${iso}: the file names ${m[1]}`);
      }
  }
  if (!isObj(x.vocab)) p.add('manifest.vocab: not an object');
  else {
    const v = x.vocab;
    for (const k of ['agreement_types', 'partnership_labels', 'legal_statuses', 'implementation_statuses', 'coding_bases', 'confidence_levels', 'depth_classes', 'instrument_groups']) p.labels(v[k], `manifest.vocab.${k}`);
    const statuses = keysOf(v.implementation_statuses);
    if (p.strArr(v.delivery_statuses, 'manifest.vocab.delivery_statuses')) for (const s of v.delivery_statuses) if (!statuses.has(s)) p.add(`manifest.vocab.delivery_statuses: '${s}' is not an implementation status`);
    if (isObj(v.depth_classes)) {
      const k = Object.keys(v.depth_classes).sort();
      if (k.join(',') !== '0,1,2,3,4,5') p.add(`manifest.vocab.depth_classes: expected the keys 0 to 5, got ${k.join(', ') || 'none'}`);
    }
    for (const k of ['specific_bases', 'established_instrument_bases'] as const) {
      const xs = v[k];
      if (xs !== undefined && p.strArr(xs, `manifest.vocab.${k}`)) {
        const bases = keysOf(v.coding_bases);
        for (const b of xs) if (!bases.has(b)) p.add(`manifest.vocab.${k}: '${b}' is not a coding basis`);
      }
    }
    if (v.compacts !== undefined) p.labels(v.compacts, 'manifest.vocab.compacts');
    if (p.arr(v.sectors, 'manifest.vocab.sectors'))
      v.sectors.forEach((s, i) => {
        const at = `manifest.vocab.sectors[${i}]`;
        if (!isObj(s)) return p.add(`${at}: not an object`);
        p.str(s.id, `${at}.id`);
        p.str(s.label, `${at}.label`);
        p.bool(s.ei, `${at}.ei`);
        p.strArr(s.techs, `${at}.techs`);
      });
    const groups = keysOf(v.instrument_groups);
    if (p.arr(v.instruments, 'manifest.vocab.instruments'))
      v.instruments.forEach((s, i) => {
        const at = `manifest.vocab.instruments[${i}]`;
        if (!isObj(s)) return p.add(`${at}: not an object`);
        p.str(s.id, `${at}.id`);
        p.str(s.label, `${at}.label`);
        if (!isStr(s.group) || !groups.has(s.group)) p.add(`${at}.group: '${String(s.group)}' is not an instrument group`);
        p.bool(s.operational, `${at}.operational`);
      });
    if (p.arr(v.sources, 'manifest.vocab.sources'))
      v.sources.forEach((s, i) => {
        const at = `manifest.vocab.sources[${i}]`;
        if (!isObj(s)) return p.add(`${at}: not an object`);
        for (const k of ['id', 'label', 'short', 'citation', 'url', 'licence']) p.str(s[k], `${at}.${k}`);
        p.num(s.n_records, `${at}.n_records`, { min: 0, int: true });
        p.str(s.last_updated, `${at}.last_updated`, true);
      });
  }
  if (Array.isArray(x.names) ? x.names.length > 0 : !isObj(x.names)) p.add('manifest.names: expected an object of ISO3 -> name');
  else if (isObj(x.names)) for (const [k, v] of Object.entries(x.names)) if (!isStr(v)) p.add(`manifest.names.${k}: expected a string`);
  if (!isObj(x.counts)) p.add('manifest.counts: not an object');
  else {
    for (const k of ['records', 'canonical', 'pairs', 'countries']) p.num(x.counts[k], `manifest.counts.${k}`, { min: 0, int: true });
    const bs = x.counts.by_source;
    if (!(isObj(bs) || (Array.isArray(bs) && bs.length === 0))) p.add('manifest.counts.by_source: expected an object of source -> count');
    else if (isObj(bs)) for (const [k, v] of Object.entries(bs)) p.num(v, `manifest.counts.by_source.${k}`, { min: 0, int: true });
  }
  p.strArr(x.caveats, 'manifest.caveats');
  if (x.eu_members !== undefined && p.strArr(x.eu_members, 'manifest.eu_members'))
    for (const c of x.eu_members) if (!ISO3_RE.test(c)) p.add(`manifest.eu_members: '${c}' is not an ISO3 code`);
  return p.list;
}

/** Columns of one table: every column an array, all of one length; returns the length (or -1). */
function columns(p: Problems, x: unknown, path: string, need: readonly string[]): number {
  if (!isObj(x)) {
    p.add(`${path}: expected an object of columns`);
    return -1;
  }
  let n = -1;
  for (const k of need) {
    const col = x[k];
    if (col === undefined) {
      p.add(`${path}.${k}: missing`);
      continue;
    }
    if (!p.arr(col, `${path}.${k}`)) continue;
    if (n < 0) n = col.length;
    else if (col.length !== n) p.add(`${path}.${k}: ${col.length} values, expected ${n} (columns of different lengths)`);
  }
  return n;
}

const PAIR_COLS = ['a', 'b', 'n', 'n_recent', 'newest', 'depth_all', 'depth_ei', 'has_sp', 'has_trade', 'has_sectoral', 'n_delivery', 'ei_sectors', 'types', 'sources'] as const;
/** Additive pair columns ("|"-joined per pair): checked when present. */
const PAIR_OPTIONAL_COLS = ['years', 'years_ei', 'igroups', 'legal', 'status', 'stages'] as const;
const YEAR_RE = /^\d{4}$/;
const COUNTRY_COLS = ['iso3', 'n_partners', 'n_partners_d3', 'n_agreements', 'n_ei_agreements', 'newest'] as const;

/** Problems with the global file (the manifest's vocabularies check its codes when given). */
export function checkAgreementsGlobal(x: unknown, manifest?: unknown): string[] {
  if (!isObj(x)) return ['global: not an object'];
  const p = new Problems();
  const vs = vocabSets(manifest);
  const n = columns(p, x.pairs, 'global.pairs', PAIR_COLS);
  // the additive columns: each, when present, an array of the pairs' length
  const extra = isObj(x.pairs)
    ? PAIR_OPTIONAL_COLS.filter((k) => {
        const col = (x.pairs as Obj)[k];
        if (col === undefined) return false;
        if (!p.arr(col, `global.pairs.${k}`)) return false;
        if (n >= 0 && col.length !== n) {
          p.add(`global.pairs.${k}: ${col.length} values, expected ${n} (columns of different lengths)`);
          return false;
        }
        return true;
      })
    : [];
  if (n > 0 && isObj(x.pairs)) {
    const c = x.pairs as Record<string, unknown[]>;
    for (let i = 0; i < n && !p.full(); i++) {
      const at = `global.pairs[${i}]`;
      const a = c.a[i];
      const b = c.b[i];
      if (!isStr(a) || !ISO3_RE.test(a)) p.add(`${at}.a: not an ISO3 code`);
      if (!isStr(b) || !ISO3_RE.test(b)) p.add(`${at}.b: not an ISO3 code`);
      if (isStr(a) && isStr(b) && !(a < b)) p.add(`${at}: the pair is not ordered a < b (${a}|${b})`);
      for (const k of ['n', 'n_recent', 'n_delivery'] as const) p.num(c[k][i], `${at}.${k}`, { min: 0, int: true });
      for (const k of ['depth_all', 'depth_ei'] as const) p.oneOf(c[k][i], DEPTH_CLASSES, `${at}.${k}`);
      for (const k of ['has_sp', 'has_trade', 'has_sectoral'] as const) p.bool(c[k][i], `${at}.${k}`);
      date(p, c.newest[i], `${at}.newest`);
      p.joined(c.ei_sectors[i], `${at}.ei_sectors`, set(vs?.sectors), true);
      p.joined(c.types[i], `${at}.types`, vs?.types);
      p.joined(c.sources[i], `${at}.sources`, vs?.sources);
      for (const k of extra) {
        const val = c[k][i];
        if (k === 'years' || k === 'years_ei') {
          p.joined(val, `${at}.${k}`);
          if (isStr(val)) for (const y of val.split('|').filter(Boolean)) if (!YEAR_RE.test(y)) p.add(`${at}.${k}: '${y}' is not a year`);
        } else if (k === 'igroups') p.joined(val, `${at}.${k}`, set(vs?.groups));
        else if (k === 'legal') p.joined(val, `${at}.${k}`, set(vs?.legal));
        else if (k === 'status') p.joined(val, `${at}.${k}`, set(vs?.statuses));
        else p.joined(val, `${at}.${k}`, new Set<string>(SEGMENTS));
      }
    }
  }
  const m = columns(p, x.countries, 'global.countries', COUNTRY_COLS);
  if (m > 0 && isObj(x.countries)) {
    const c = x.countries as Record<string, unknown[]>;
    for (let i = 0; i < m && !p.full(); i++) {
      const at = `global.countries[${i}]`;
      if (!isStr(c.iso3[i]) || !ISO3_RE.test(c.iso3[i] as string)) p.add(`${at}.iso3: not an ISO3 code`);
      for (const k of ['n_partners', 'n_partners_d3', 'n_agreements', 'n_ei_agreements'] as const) p.num(c[k][i], `${at}.${k}`, { min: 0, int: true });
      date(p, c.newest[i], `${at}.newest`);
    }
  }
  return p.list;
}

function codes(p: Problems, x: unknown, path: string, ids: Set<string> | undefined, second: Set<string> | readonly string[] | undefined, secondWord: string): void {
  if (!p.arr(x, path)) return;
  x.forEach((t, i) => {
    const at = `${path}[${i}]`;
    if (!Array.isArray(t) || t.length !== 2 || !isStr(t[0]) || !isStr(t[1])) return p.add(`${at}: expected [code, ${secondWord}]`);
    if (ids && ids.size && !ids.has(t[0])) p.add(`${at}: '${t[0]}' is not in the vocabulary`);
    if (second && (Array.isArray(second) ? second.length : (second as Set<string>).size) && !has(second as Set<string>, t[1])) p.add(`${at}: '${t[1]}' is not a ${secondWord}`);
  });
}

const set = (s: Set<string> | undefined): Set<string> | undefined => (s && s.size ? s : undefined);

/** Problems with an economy's file (`iso3`: the economy it must be; the manifest's vocabularies check its codes when given). */
export function checkAgreementsCountry(x: unknown, iso3?: string, manifest?: unknown): string[] {
  if (!isObj(x)) return ['country: not an object'];
  const p = new Problems();
  const vs = vocabSets(manifest);
  if (!isStr(x.iso3) || !ISO3_RE.test(x.iso3)) p.add('country.iso3: not an ISO3 code');
  else if (iso3 && x.iso3 !== iso3) p.add(`country.iso3: ${x.iso3} is not ${iso3}`);
  const ids = new Set<string>();
  if (p.arr(x.agreements, 'country.agreements')) {
    x.agreements.forEach((a, i) => {
      if (p.full()) return;
      const at = `country.agreements[${i}]`;
      if (!isObj(a)) return p.add(`${at}: not an object`);
      p.str(a.id, `${at}.id`);
      if (isStr(a.id)) {
        if (ids.has(a.id)) p.add(`${at}.id: '${a.id}' is listed twice`);
        ids.add(a.id);
      }
      p.str(a.title, `${at}.title`);
      if (p.strArr(a.parties, `${at}.parties`)) {
        if (a.parties.length < 2) p.add(`${at}.parties: fewer than two parties`);
        if (isStr(x.iso3) && !a.parties.includes(x.iso3) && !(a.via_eu === true && a.parties.includes('EUU'))) p.add(`${at}.parties: ${x.iso3} is not a party (nor the EU, via_eu)`);
      }
      if (vs && set(vs.types)) p.oneOf(a.type, vs.types, `${at}.type`);
      else p.str(a.type, `${at}.type`);
      if (a.label !== null && a.label !== '' && vs && set(vs.labels)) p.oneOf(a.label, vs.labels, `${at}.label`);
      else p.str(a.label, `${at}.label`, true);
      if (vs && set(vs.legal)) p.oneOf(a.legal, vs.legal, `${at}.legal`);
      else p.str(a.legal, `${at}.legal`);
      p.str(a.legal_basis, `${at}.legal_basis`, true);
      for (const k of ['date', 'signature_date', 'effective_date']) date(p, a[k], `${at}.${k}`);
      p.oneOf(a.date_precision, DATE_PRECISIONS, `${at}.date_precision`);
      if (vs && set(vs.statuses)) p.oneOf(a.status, vs.statuses, `${at}.status`);
      else p.str(a.status, `${at}.status`);
      p.str(a.status_raw, `${at}.status_raw`, true);
      p.str(a.category, `${at}.category`, true);
      codes(p, a.sectors, `${at}.sectors`, set(vs?.sectors), set(vs?.bases), 'coding basis');
      codes(p, a.instruments, `${at}.instruments`, set(vs?.instruments), set(vs?.bases), 'coding basis');
      p.strArr(a.techs, `${at}.techs`);
      p.strArr(a.segments, `${at}.segments`, SEGMENTS);
      p.bool(a.ei, `${at}.ei`);
      p.bool(a.via_eu, `${at}.via_eu`);
      if (p.arr(a.sources, `${at}.sources`)) {
        if (a.sources.length === 0) p.add(`${at}.sources: no source record (every agreement keeps its provenance)`);
        a.sources.forEach((s, j) => {
          const sa = `${at}.sources[${j}]`;
          if (!isObj(s)) return p.add(`${sa}: not an object`);
          if (vs && set(vs.sources)) p.oneOf(s.dataset, vs.sources, `${sa}.dataset`);
          else p.str(s.dataset, `${sa}.dataset`);
          p.str(s.record_id, `${sa}.record_id`);
          p.str(s.url, `${sa}.url`, true);
        });
      }
      p.str(a.primary_url, `${at}.primary_url`, true);
      p.bool(a.primary_available, `${at}.primary_available`);
      if (vs && set(vs.confidence)) p.oneOf(a.confidence, vs.confidence, `${at}.confidence`);
      else p.str(a.confidence, `${at}.confidence`);
      p.num(a.source_count, `${at}.source_count`, { min: 1, int: true });
      if (p.arr(a.delivery, `${at}.delivery`))
        a.delivery.forEach((d, j) => {
          const da = `${at}.delivery[${j}]`;
          if (!isObj(d)) return p.add(`${da}: not an object`);
          p.str(d.text, `${da}.text`);
          p.str(d.status, `${da}.status`);
          p.str(d.url, `${da}.url`, true);
        });
      p.num(a.trade_depth_index, `${at}.trade_depth_index`, { nullable: true, min: 0, max: 7 });
    });
  }
  const partners = new Set<string>();
  if (p.arr(x.partners, 'country.partners')) {
    x.partners.forEach((r, i) => {
      if (p.full()) return;
      const at = `country.partners[${i}]`;
      if (!isObj(r)) return p.add(`${at}: not an object`);
      if (!isStr(r.partner) || !ISO3_RE.test(r.partner)) p.add(`${at}.partner: not an ISO3 code`);
      else {
        if (partners.has(r.partner)) p.add(`${at}.partner: '${r.partner}' is listed twice`);
        partners.add(r.partner);
        if (r.partner === x.iso3) p.add(`${at}.partner: the economy itself`);
      }
      for (const k of ['n', 'n_bilateral', 'n_plurilateral', 'n_via_eu', 'n_active', 'n_recent', 'n_announced', 'n_signed', 'n_in_force', 'n_delivery']) p.num(r[k], `${at}.${k}`, { min: 0, int: true });
      p.num(r.share_delivery, `${at}.share_delivery`, { nullable: true, min: 0, max: 1 });
      p.num(r.trade_depth_index, `${at}.trade_depth_index`, { nullable: true, min: 0, max: 7 });
      for (const k of ['oldest_active', 'newest']) date(p, r[k], `${at}.${k}`);
      for (const k of ['has_sp', 'has_trade', 'has_sectoral']) p.bool(r[k], `${at}.${k}`);
      codes(p, r.sectors, `${at}.sectors`, set(vs?.sectors), CODE_GRADES, 'grade');
      if (p.strArr(r.ei_sectors, `${at}.ei_sectors`)) p.joined(r.ei_sectors.join('|'), `${at}.ei_sectors`, set(vs?.sectors), true);
      p.strArr(r.techs, `${at}.techs`);
      p.strArr(r.segments, `${at}.segments`, SEGMENTS);
      const inst = r.inst;
      if (Array.isArray(inst) && inst.length === 0) {
        // an empty named list
      } else if (!isObj(inst)) p.add(`${at}.inst: expected an object of instrument group -> 0, 1 or 2`);
      else
        for (const [g, v] of Object.entries(inst)) {
          if (vs && set(vs.groups) && !vs.groups.has(g)) p.add(`${at}.inst.${g}: not an instrument group`);
          p.oneOf(v, [0, 1, 2], `${at}.inst.${g}`);
        }
      for (const k of ['depth_all', 'depth_ei']) p.oneOf(r[k], DEPTH_CLASSES, `${at}.${k}`);
      for (const k of ['depth_all_rule', 'depth_ei_rule', 'max_confidence']) p.str(r[k], `${at}.${k}`, true);
      p.strArr(r.sources, `${at}.sources`, set(vs?.sources));
      if (p.strArr(r.agreement_ids, `${at}.agreement_ids`)) for (const id of r.agreement_ids) if (ids.size && !ids.has(id)) p.add(`${at}.agreement_ids: '${id}' is not in agreements`);
    });
  }
  if (p.arr(x.gaps, 'country.gaps')) {
    x.gaps.forEach((g, i) => {
      if (p.full()) return;
      const at = `country.gaps[${i}]`;
      if (!isObj(g)) return p.add(`${at}: not an object`);
      if (!isStr(g.partner) || !ISO3_RE.test(g.partner)) p.add(`${at}.partner: not an ISO3 code`);
      p.str(g.tech, `${at}.tech`);
      p.oneOf(g.side, GAP_SIDES, `${at}.side`);
      p.num(g.need, `${at}.need`, { nullable: true, min: 0, max: 1 });
      p.num(g.capability, `${at}.capability`, { nullable: true, min: 0, max: 1 });
      p.num(g.recommended, `${at}.recommended`, { min: 0, int: true });
      p.num(g.friendshore, `${at}.friendshore`, { nullable: true });
      p.strArr(g.aib_shared, `${at}.aib_shared`);
      p.oneOf(g.complementarity, COMPLEMENTARITIES, `${at}.complementarity`);
      p.oneOf(g.covered_grade, COVERED_GRADES, `${at}.covered_grade`);
      p.oneOf(g.architecture, ['strong', 'weak'], `${at}.architecture`);
      p.oneOf(g.quadrant, QUADRANTS, `${at}.quadrant`);
      if (g.alignment !== undefined) p.num(g.alignment, `${at}.alignment`, { nullable: true, min: 0, max: 1 });
      if (g.alignment_blocked !== undefined && typeof g.alignment_blocked !== 'boolean') p.add(`${at}.alignment_blocked: not a boolean`);
      if (g.stages_high !== undefined) p.strArr(g.stages_high, `${at}.stages_high`);
      if (g.stages_covered !== undefined) p.strArr(g.stages_covered, `${at}.stages_covered`);
    });
  }
  if (p.arr(x.execution_gaps, 'country.execution_gaps')) {
    x.execution_gaps.forEach((g, i) => {
      const at = `country.execution_gaps[${i}]`;
      if (!isObj(g)) return p.add(`${at}: not an object`);
      p.str(g.tech, `${at}.tech`);
      p.str(g.priority_basis, `${at}.priority_basis`);
      p.num(g.priority, `${at}.priority`, { nullable: true, min: 0, max: 1 });
      p.strArr(g.suppliers_without_coverage, `${at}.suppliers_without_coverage`);
      if (g.stages !== undefined) p.strArr(g.stages, `${at}.stages`);
    });
  }
  return p.list;
}
