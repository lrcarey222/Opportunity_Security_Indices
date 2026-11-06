// Development partnerships (scripts/93_export_development_partnerships.R; rules R/dashboard/country/country_devpart.R
// and config/development_partnerships.yml): the development arm of the Allied Industrial Base, at donor x
// recipient x technology x supply-chain step. One file per economy (devpart-<ISO3>.<h8>.json, the Country
// manifest entry's `devpart`) in one of two perspectives:
//   donor      a high-income economy in the AIB demand scope: its steps with a strategic reason for more allied
//              capacity, the developing partners whose same step is a credible Catch Up, the commercial-route and
//              review cases, and its current aid against those pairs;
//   recipient  a non-high-income economy: its Catch Up steps with the recipient test, and the developed partners
//              whose same step needs the capability.
// No composite score: the file carries the reasons (donor signals, recipient anchors, relationship ties, tools)
// and a fixed order. Columns arrive as arrays (NA as null); ";"-joined lists are split here. Pure.

export const DEVPART_SCHEMA = 'opsi.country.devpart';
export const DEVPART_FILE_RE = /^devpart-[A-Z0-9]{3}\.[0-9a-f]{8}\.json$/;

export type DevpartPerspective = 'donor' | 'recipient' | 'none';
export type DevpartRoute = 'development' | 'commercial' | 'review' | 'excluded';
type Cell = string | number | boolean | null;
export type Columns = Record<string, Cell[]>;

export interface DevpartWords {
  signal: Record<string, string>;
  signal_short?: Record<string, string>;
  anchor: Record<string, string>;
  support: Record<string, string>;
  route: Record<string, string>;
  aid_status: Record<string, string>;
  aid_gap: Record<string, string>;
  constraint: Record<string, string>;
  benefit_donor: Record<string, string>;
  benefit_recipient: Record<string, string>;
  fail: Record<string, string>;
  /** an input the engine did not observe, by the constraint it would have raised */
  unobserved?: Record<string, string>;
  /** strategic compact id -> name */
  compact?: Record<string, string>;
}

export interface DevpartDoc {
  schema: typeof DEVPART_SCHEMA;
  schema_version: number;
  iso3: string;
  country: string;
  perspective: DevpartPerspective;
  perspective_reason: string | null;
  meta: { created: string; as_of_label: string | null; aid_years: number[]; aid_flow: string; dip_years: number[]; need_bar: number; sources: string[] };
  words: DevpartWords;
  steps?: Columns;
  pairs?: Columns;
  aid?: { reporting: boolean; gap: Columns; top: Columns; no_aib: Columns };
  preserve?: string[] | string;
  /** recipient: its Preserve steps with the commercial and review counts */
  preserve_steps?: Columns;
}

/** A pair as the Country file carries it (fields marked optional are in the full export only). */
export interface DevpartPair {
  donor_iso3: string;
  recipient_iso3: string;
  technology: string;
  stage: string;
  key: string;
  capability: string;
  aib_build_nodes: string[];
  donor_choice: string;
  donor_fence?: string | null;
  donor_need: number | null;
  donor_confidence?: string | null;
  donor_signals: string[];
  donor_tier: number | null;
  aib_gap_status: string | null;
  recipient_choice: string;
  recipient_siv?: number | null;
  recipient_competitive_position: number | null;
  recipient_confidence: string | null;
  recipient_evidence: string | null;
  recipient_income_group: string | null;
  catch_up_anchor_type: string[];
  pathway_signals: string[];
  thin_anchor: boolean;
  recipient_constraints: string[];
  /** pathway inputs the data do not cover (never stated as constraints) */
  recipient_unobserved: string[];
  recipient_fail: string | null;
  donor_national_dropped?: boolean;
  capability_note?: string | null;
  donor_deployment_choice?: string | null;
  demand_anchor?: string | null;
  donor_review: boolean;
  capability_unverified?: boolean;
  aib_capacity_host: boolean;
  development_potential_legacy?: number | null;
  oda_status: string | null;
  current_oda: number | null;
  other_official_finance?: number | null;
  oda_specificity: string | null;
  oda_named?: number | null;
  oda_shared?: number | null;
  oda_proxy?: number | null;
  oda_pooled?: number | null;
  cm_fdi_capital_disclosed?: number | null;
  oda_recovery_proxy: number | null;
  oda_pair_total: number | null;
  oda_stage: string[];
  oda_components_assembly_shared: boolean;
  oda_proxy_flag?: boolean;
  oci_finance: number | null;
  imf_dip_tie: number | null;
  imf_dip_basis: string | null;
  imf_dip_conduit: string | null;
  cm_fdi_projects: number | null;
  cm_fdi_usd_m?: number | null;
  aib_pairing: boolean;
  compact: string[];
  donor_compact_role: string | null;
  recipient_compact_role: string | null;
  route: DevpartRoute;
  recommended_tool_family: string;
  evidence_quality: string;
  caveats: string[];
  rationale?: string;
}

export interface DevpartDonorStep {
  key: string;
  tech: string;
  stage: string;
  step_name: string;
  choice: string;
  fence: string | null;
  need: number | null;
  conf: string | null;
  signals: string[];
  donor_tier: number | null;
  aib_status: string | null;
  has_development: boolean;
  n_considered: number;
  /** "test:count" of the candidates that fail (review and excluded), most frequent first */
  fails: string[];
  /** the partners already on the commercial route in the step */
  commercial: string[];
}

export interface DevpartRecipientStep {
  key: string;
  tech: string;
  stage: string;
  step_name: string;
  choice: string;
  prize: number | null;
  cp: number | null;
  conf: string | null;
  ev: string | null;
  anchors: string[];
  supports: string[];
  thin_anchor: boolean;
  constraints: string[];
  unobserved: string[];
  capability_note?: string | null;
  recipient_fail: string | null;
  recipient_ok: boolean;
  n_donors: number;
}

const LIST_FIELDS = new Set(['aib_build_nodes', 'donor_signals', 'catch_up_anchor_type', 'pathway_signals', 'recipient_constraints', 'recipient_unobserved', 'oda_stage', 'compact', 'caveats',
  'signals', 'fails', 'commercial', 'anchors', 'supports', 'constraints', 'unobserved']);

/** A recipient's Preserve step (the commercial route: trade and investment, not aid; or a review case). */
export interface DevpartPreserveStep {
  key: string;
  tech: string;
  stage: string;
  step_name: string;
  conf: string | null;
  ev: string | null;
  preserve_fail: string | null;
  n_commercial: number;
  n_review: number;
}

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);

/** Column arrays -> rows; ";"-joined list fields split into arrays (null -> []). */
export function devpartRows<T>(cols: Columns | undefined): T[] {
  if (!cols) return [];
  const keys = Object.keys(cols);
  const n = keys.length ? (cols[keys[0]]?.length ?? 0) : 0;
  const out: T[] = [];
  for (let i = 0; i < n; i++) {
    const r: Record<string, unknown> = {};
    for (const k of keys) {
      const v = cols[k][i] ?? null;
      r[k] = LIST_FIELDS.has(k) ? (typeof v === 'string' && v ? v.split(';') : []) : v;
    }
    out.push(r as T);
  }
  return out;
}

/** The problems of a development-partnership file (empty when it matches the contract). */
export function checkDevpartDoc(x: unknown, iso3?: string): string[] {
  const p: string[] = [];
  if (!isObj(x)) return ['devpart: not an object'];
  if (x.schema !== DEVPART_SCHEMA) p.push(`devpart.schema: expected '${DEVPART_SCHEMA}'`);
  if (typeof x.schema_version !== 'number') p.push('devpart.schema_version: not a number');
  else if (Math.floor(x.schema_version) > 1) p.push(`devpart: schema_version ${x.schema_version} needs a newer app`);
  if (typeof x.iso3 !== 'string' || !/^[A-Z0-9]{3}$/.test(x.iso3)) p.push('devpart.iso3: not an ISO3 code');
  else if (iso3 && x.iso3 !== iso3) p.push(`devpart.iso3: ${x.iso3} is not ${iso3}`);
  if (!['donor', 'recipient', 'none'].includes(x.perspective as string)) p.push(`devpart.perspective: '${String(x.perspective)}'`);
  if (!isObj(x.words)) p.push('devpart.words: not an object');
  if (!isObj(x.meta)) p.push('devpart.meta: not an object');
  const cols = (c: unknown, what: string, need: readonly string[]) => {
    if (!isObj(c)) return p.push(`devpart.${what}: not a column object`);
    const lens = Object.values(c).map((v) => (Array.isArray(v) ? v.length : -1));
    if (lens.some((l) => l < 0)) p.push(`devpart.${what}: a column is not an array`);
    else if (new Set(lens).size > 1) p.push(`devpart.${what}: columns of different lengths`);
    for (const k of need) if (!(k in c)) p.push(`devpart.${what}.${k}: missing`);
    return 0;
  };
  if (x.perspective === 'donor' || x.perspective === 'recipient') {
    cols(x.steps, 'steps', ['key', 'tech', 'stage', 'step_name', 'choice']);
    cols(x.pairs, 'pairs', ['donor_iso3', 'recipient_iso3', 'key', 'route', 'oda_status', 'current_oda']);
    if (isObj(x.pairs) && Array.isArray(x.pairs.route) && !x.pairs.route.every((r) => ['development', 'commercial', 'review', 'excluded'].includes(r as string))) {
      p.push('devpart.pairs.route: unknown route');
    }
  }
  if (x.perspective === 'donor') {
    if (!isObj(x.aid)) p.push('devpart.aid: not an object');
    else for (const k of ['gap', 'top', 'no_aib']) cols(x.aid[k], `aid.${k}`, []);
  }
  return p;
}

// ---- derivations --------------------------------------------------------------------------------------------

const PAIR_LISTS = ['aib_build_nodes', 'donor_signals', 'catch_up_anchor_type', 'pathway_signals', 'recipient_constraints', 'recipient_unobserved', 'oda_stage', 'compact', 'caveats'] as const;

/** The economy's pairs in its perspective (the donor's or the recipient's), in the file's order; a list column the file leaves out reads as empty lists. */
export function devpartPairs(doc: DevpartDoc): DevpartPair[] {
  return devpartRows<DevpartPair>(doc.pairs).map((p) => {
    const r = p as unknown as Record<string, unknown>;
    for (const k of PAIR_LISTS) if (!Array.isArray(r[k])) r[k] = [];
    if (typeof r.rationale !== 'string') r.rationale = '';
    if (typeof r.recommended_tool_family !== 'string') r.recommended_tool_family = '';
    return p;
  });
}

export const ROUTE_ORDER: readonly DevpartRoute[] = ['development', 'commercial', 'review'];

/** Pairs by route. */
export function byRoute(pairs: readonly DevpartPair[]): Record<DevpartRoute, DevpartPair[]> {
  const out: Record<DevpartRoute, DevpartPair[]> = { development: [], commercial: [], review: [], excluded: [] };
  for (const p of pairs) out[p.route].push(p);
  return out;
}

/** A list of codes in the file's words (unknown codes as they are). */
export function wordsOf(codes: readonly string[] | undefined, words: Record<string, string> | undefined): string[] {
  return (codes ?? []).map((c) => words?.[c] ?? c.replace(/_/g, ' '));
}

/** USD millions in words: "$1.3 million", "<$0.1 million", "$536 million"; null -> null. */
export function fmtUsdM(v: number | null | undefined): string | null {
  if (v === null || v === undefined || !Number.isFinite(v)) return null;
  if (v === 0) return '$0';
  if (Math.abs(v) < 0.1) return '<$0.1 million';
  if (Math.abs(v) < 10) return `$${(Math.round(v * 10) / 10).toFixed(1)} million`;
  if (Math.abs(v) >= 1000) return `$${(Math.round(v / 100) / 10).toFixed(1)} billion`;
  return `$${Math.round(v)} million`;
}

/** The pair's existing aid relationship in words (missing data stay missing, never "$0"). */
export function aidWords(p: DevpartPair, words: DevpartWords): string {
  const s = p.oda_status;
  if (s === 'not_reporting') return words.aid_status.not_reporting;
  if (s === 'none' || s === null) return words.aid_status.none;
  if (s === 'proxy') return `no recycling purpose code; ${fmtUsdM(p.oda_recovery_proxy) ?? 'no'} at Extraction and Refining (proxy)`;
  if (s === 'other_stage') return `${fmtUsdM(p.oda_pair_total)} in energy, industry and mining aid, none at this step`;
  // the label says where most of the money is (config aid.technology_share_min); the technology-coded part is named
  const spec =
    p.oda_specificity === 'technology' ? 'mostly coded to the technology'
      : p.oda_specificity === 'shared_technology' ? 'mostly under codes naming several technologies'
        : p.oda_specificity === 'proxy' ? 'mostly under a code that only approximates the technology'
          : 'stage-wide, technology not identified';
  const named = p.oda_named ?? 0;
  const part = named > 0 && p.oda_specificity !== 'technology' ? `; ${fmtUsdM(named)} coded to the technology` : '';
  return `${fmtUsdM(p.current_oda)} at this step (${spec}${part})`;
}

/** Where the pair's aid sits, and the Components/Assembly shared-code note of the pair itself. */
export function aidStageWords(p: DevpartPair): string {
  if (!p.oda_stage.length) return '';
  return ` (aid recorded at: ${p.oda_stage.join(', ')}${p.oda_components_assembly_shared && p.oda_status === 'recorded' ? '; Components and Assembly share aid codes' : ''})`;
}

/** Compact roles in words: each "SC-ID: role" as "role in the <name> compact"; only the compacts in `only` when given. */
export function compactRoleWords(roles: string | null | undefined, words: DevpartWords, only?: readonly string[]): string[] {
  return (roles ?? '')
    .split(';')
    .map((x) => x.trim())
    .filter(Boolean)
    .map((x) => {
      const i = x.indexOf(':');
      return i < 0 ? { id: x, role: '' } : { id: x.slice(0, i).trim(), role: x.slice(i + 1).trim() };
    })
    .filter((x) => !only || only.includes(x.id))
    .map((x) => `${x.role || 'a role'} in the ${words.compact?.[x.id] ?? x.id} compact`);
}

/** A recipient's constraints in words: the observed ones, then the inputs the data do not cover. */
export function constraintWords(cons: readonly string[], unobserved: readonly string[] | undefined, words: DevpartWords): string {
  const out = [...wordsOf(cons, words.constraint), ...new Set(wordsOf(unobserved ?? [], words.unobserved))];
  return out.length ? out.join('; ') : 'none observed';
}

/**
 * The pair's investment and finance ties in words. An observed zero reads "none recorded"; a source that does
 * not cover the pair (null) is left out, and with nothing observed at all the cell says so.
 */
export function tieWords(p: DevpartPair, words?: DevpartWords): string {
  const out: string[] = [];
  if (p.imf_dip_tie !== null) {
    const by = p.imf_dip_basis === 'derived' ? ', partner-reported' : '';
    out.push(p.imf_dip_tie > 0 ? `direct investment ${fmtUsdM(p.imf_dip_tie)} (economy-wide${by})` : p.imf_dip_tie < 0 ? `net direct investment position below zero (economy-wide${by})` : 'no direct investment position recorded');
  }
  if (p.cm_fdi_projects !== null) {
    out.push(p.cm_fdi_projects > 0 ? `${p.cm_fdi_projects} factory project${p.cm_fdi_projects === 1 ? '' : 's'} by the donor's firms` : "no factory projects by the donor's firms recorded");
  }
  if (p.oci_finance !== null) {
    out.push(p.oci_finance > 0 ? `export and lender finance ${fmtUsdM(p.oci_finance)}` : 'no export or lender finance recorded at the step');
  }
  if (p.aib_pairing) out.push('a simulated AIB investment pairing (a model result, not an existing tie)');
  if (words) out.push(...compactRoleWords(p.recipient_compact_role, words, p.compact));
  return out.length ? out.join('; ') : 'not covered by the data';
}

/** The Catch Up anchor and pathway in words. */
export function anchorWords(p: { catch_up_anchor_type: string[]; pathway_signals: string[] }, words: DevpartWords): string {
  const a = wordsOf(p.catch_up_anchor_type, words.anchor);
  const s = wordsOf(p.pathway_signals, words.support);
  return a.length ? `${a.join(', ')}${s.length ? `; corroborated by ${s.join(', ')}` : ''}` : 'no capability anchor';
}

/** The donor's reasons in words (the Partner signal left out where the choice already says Partner). */
export function donorReasonWords(p: { donor_signals: string[]; donor_choice?: string }, words: DevpartWords): string {
  return wordsOf(p.donor_signals.filter((sg) => !(sg === 'partner' && p.donor_choice === 'partner')), words.signal).join('; ');
}

/**
 * The donor's current-aid comparison grouped (the file's aid.gap rows by class), with the largest aid
 * recipients that have no pair. The comparison is against this energy-industrial objective only.
 */
export interface AidGapRow {
  recipient_iso3: string;
  recipient: string;
  technology: string;
  stage: string;
  capability: string;
  route: string;
  current_oda: number | null;
  oda_pair_total: number | null;
  oda_stage: string[];
  gap: string;
}
export interface AidTopRow {
  recipient_iso3: string;
  recipient: string;
  oda_all: number | null;
  oda_mapped: number | null;
  paired?: boolean;
}
export function aidGap(doc: DevpartDoc): { reporting: boolean; groups: { gap: string; rows: AidGapRow[] }[]; noAib: AidTopRow[]; top: AidTopRow[] } {
  const rows = devpartRows<AidGapRow>(doc.aid?.gap);
  const order = ['aligned', 'stage_wide', 'wrong_stage', 'no_support', 'proxy', 'graduate', 'stage_wide_commercial'];
  const groups = order.map((g) => ({ gap: g, rows: rows.filter((r) => r.gap === g) })).filter((g) => g.rows.length);
  return { reporting: !!doc.aid?.reporting, groups, noAib: devpartRows<AidTopRow>(doc.aid?.no_aib), top: devpartRows<AidTopRow>(doc.aid?.top) };
}

/** Recipient view: donors grouped by whether they need the step themselves (tiers 1-4) or only for the allied gap (tier 5). */
export function recipientDonorGroups(pairs: readonly DevpartPair[]): { own: DevpartPair[]; allied: DevpartPair[] } {
  const live = pairs.filter((p) => p.route === 'development' || p.route === 'commercial');
  return { own: live.filter((p) => (p.donor_tier ?? 9) <= 4), allied: live.filter((p) => p.donor_tier === 5) };
}
