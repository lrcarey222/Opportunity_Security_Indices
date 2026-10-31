// The six-stage posture bundle, schema 2 (data/posture, written by scripts/91_export_posture_data.R from the
// strategic position framework, docs/strategic_position.md; design section 3, 2026-10-29). Canonical logic:
// Need -> Prize -> Position -> Choice -> Policy alignment. Every country x technology x six-stage node has a
// choice: Secure (with its rationale), Compete (Preserve, Catch Up or Partner), Deploy (Accelerate or Watch),
// or Not assessed (need or prize missing; never Watch). The core document is columnar; decodeCore() turns it
// into rows and checks it. A v1 core (schema_version 1: Need x Opportunity quadrants, no Deploy mode) is
// refused, as is a newer one. Checks throw PostureContractError with every problem found. The stages are
// the six of src/lib/stages.ts (SUPPLY_STAGES, in chain order).
//
// Values are normalised here, once: evidence high / medium / low / none (as R writes it) becomes the
// grammar's strong / moderate / weak (the raw word is kept in evidenceRaw); posture confidence "medium"
// becomes "moderate" and alignment confidence "moderate" becomes "medium". The detail document's cells are
// normalised by detail.ts (nodeDetail), so the drawer and the sections read one typed shape whatever the
// exporter's spelling of a field (flat or nested, see detail.ts).
//
// Yards & Fences (2026-10-30, additive within schema 2; R posture_export.R): the core may carry the prize the
// choice read under meta.prize_measure (strategic_value: cells.prize is the Strategic Industrial Value, and
// cells.io the market prize, Industrial Opportunity; market or absent: cells.prize is the market prize), the
// node's yard roll-up (cells.yd, coded through meta.yard_codes), the ten large-yard signal names
// (meta.large_yard_signals) and the node-level evidence (meta.yf_nodes, columnar, one row per producing-stage
// node and sensitive layer). Each is optional: an older core decodes with prize_measure "market", no yards and
// no nodes. A roll-up always holds the node's own yard (the yard of its choice).

import { DEPLOYMENT_STAGE, STAGE_DISPLAY, SUPPLY_STAGES, type SupplyStage } from '../lib/stages.ts';
import { nodeDetail, type NodeDetail } from './detail.ts';

export const POSTURE_SCHEMA = 'osi-posture';
export const POSTURE_SCHEMA_VERSION = 2;
export const POSTURE_DATA_DIR = 'data/posture/';
/** The six stages, in chain order (the shared SUPPLY_STAGES). */
export const POSTURE_STAGES = SUPPLY_STAGES;
export type PostureStage = SupplyStage;

/** S Secure, C Compete, D Deploy, N not assessed (need or prize missing: never Watch, never Deploy). */
export type PostureCode = 'S' | 'C' | 'D' | 'N';
/** The three postures that carry a background colour. */
export type AssessedPosture = Exclude<PostureCode, 'N'>;
/** The detail document's posture words. */
export type PostureWord = 'secure' | 'compete' | 'deploy' | 'not_assessed';
export type CompeteMode = 'preserve' | 'catch_up' | 'partner';
export type DeployMode = 'accelerate' | 'watch';
/** The six verbs of the counts line, the Choices groups and the markers: Secure and the five modes. */
export type Verb = 'secure' | CompeteMode | DeployMode;
export type NeedType = 'supply_chain' | 'deployment';
export type SecurityRationale = 'operational_control' | 'coercive_chokepoint' | 'systemic_resilience';
/** The credible-pathway test of Catch Up (design D3); unknown counts as not credible. */
export type Pathway = 'yes' | 'no' | 'unknown';
/** Policy alignment (design D5). deployment_gap is shown as under-prioritized, "under-response (deployment)". */
export type Alignment = 'aligned' | 'under_prioritized' | 'over_prioritized' | 'misaligned' | 'deployment_gap' | 'mixed' | 'insufficient_data';
export type AlignmentConfidence = 'high' | 'medium' | 'low';
export type Confidence = 'high' | 'moderate' | 'low';
/** Evidence quality, the grammar's three words (spec XX). */
export type Evidence = 'strong' | 'moderate' | 'weak';
export type Momentum = 'up' | 'flat' | 'down';
/** The allied division's gap per node (the last two only where the division data support them, design 5b). */
export type AibGap =
  | 'none' | 'no_trusted_supplier' | 'single_point_of_failure' | 'insufficient_redundancy' | 'insufficient_allied_capacity' | 'excessive_concentration' | 'weak_evidence';
/** The three yards of the Yards & Fences framing (R yard_rollup.R SP_YARDS): small (high fence: Secure), medium (Preserve, Catch Up, Partner; Accelerate at Deployment & Use), large (low fence: Watch). */
export type Yard = 'small' | 'medium' | 'large';
/** A node's yard roll-up (R SP_YARD_ROLLUPS): its own yard where its layers agree, else the yards it spans. */
export type YardRollup = Yard | 'mixed_high_medium' | 'mixed_medium_low' | 'mixed_high_low' | 'mixed_high_medium_low';
/** What cells.prize holds (R decision_engine.R choice_prize): the Strategic Industrial Value, or the market prize (Industrial Opportunity). */
export type PrizeMeasure = 'strategic_value' | 'market';
/** Where a Deployment Need family's value comes from (design D12). */
export type DataBasis = 'national' | 'regional_proxy' | 'observed_trend' | 'global_context' | 'not_assessed';

export class PostureContractError extends Error {
  readonly problems: string[];
  constructor(problems: string[]) {
    super(`Posture data does not match the contract: ${problems.slice(0, 3).join('; ')}`);
    this.name = 'PostureContractError';
    this.problems = problems;
  }
}

export interface PostureManifest {
  schema: string;
  schema_version: number;
  created: string;
  core: string;
  details: Record<string, string>;
  counts: { countries: number; cells: number; division_nodes: number };
}

/** The decision thresholds (config/strategic_position.yml), for the chart lines, borderline words and reasons. */
export interface PostureThresholds {
  /** supply_chain_need at or above: high need */
  need: number;
  /** the prize the choice read (cells.prize: the Strategic Industrial Value, or the market prize) at or above: high prize */
  prize: number;
  /** Competitive Position percentile among contenders at or above: strong position (Preserve bar, top quarter) */
  cp: number;
  /** deployment_need at or above: Deploy · Accelerate */
  deployment: number;
  /** a value within this of its threshold is borderline */
  borderline_band: number;
}

/** Which scenario each Deployment Need family reads (design D12; a family without one is null). */
export interface FamilyScenarios {
  demand: string | null;
  policy_gap: string | null;
  system: string | null;
}

export interface PostureMeta {
  techs: string[];
  stages: string[];
  /** stage id -> display name (meta.stage_display, else STAGE_DISPLAY) */
  stage_display: Record<string, string>;
  benchmark_period: string | null;
  momentum_period: string | null;
  momentum_previous_period: string | null;
  momentum_through: string | null;
  momentum_band: number;
  thresholds: PostureThresholds;
  /** scenario id -> its label ("current_policy" -> "Current policy") */
  scenario_labels: Record<string, string>;
  family_scenarios: FamilyScenarios;
  /** cell key "Tech|Stage" -> a short noun phrase for the strategy sentence ("battery materials refining") */
  phrases: Record<string, string>;
  core_members: number;
  partner_members: number;
  /** what cells.prize holds (meta.prize_measure; "market" when the core does not say) */
  prize_measure: PrizeMeasure;
  /** the roll-up of each code of cells.yd (meta.yard_codes inverted; the R codes S, M, L, SM, ML, SL, SML by default) */
  yard_codes: Record<string, YardRollup>;
  /** the large-yard signals in R's fixed order (meta.large_yard_signals); the nodes' indexes point into it */
  large_yard_signals: string[];
  /** the node-level Yards & Fences evidence, keyed "tech|stage|layer" (layer "stage" for the whole node); empty when the core has none */
  yf_nodes: Record<string, YfNode>;
}

/**
 * The Yards & Fences evidence of one producing-stage node or sensitive layer (meta.yf_nodes; country-invariant,
 * the modal value across economies: where an economy's own large-yard case differs, its detail says so).
 */
export interface YfNode {
  /** "tech|stage|layer" */
  key: string;
  tech: string;
  stage: string;
  /** the layer id, "stage" for the whole node */
  layer: string;
  /** the layer's name (null for the whole node) */
  label: string | null;
  /** the large-yard case: true holds, false does not, null undecided */
  largeYard: boolean | null;
  /** how many of the non-required (commodity) signals hold */
  n: number | null;
  /** the signals that hold, by name */
  signals: string[];
  /** the signals not observed, by name */
  unknown: string[];
  /** a large yard that keeps a capability floor (moderate industrial learning) */
  floor: boolean | null;
  /** industrial-commons value, 0-1 */
  commons: number | null;
  /** future-option value, 0-1 */
  option: number | null;
  /** how many of the three commons attributes were observed */
  commonsN: number | null;
  /** how many of the four leap tests were observed */
  optionN: number | null;
  /** "market_only": neither commons nor option reaches the gate, so the value is capped below the prize bar */
  cap: string | null;
}

export interface PostureCell {
  iso3: string;
  tech: string;
  stage: PostureStage;
  posture: PostureCode;
  /** the engine variant, the key of the policy design text (with the delivery mode); null when absent */
  variant: string | null;
  /** Compete only */
  mode: CompeteMode | null;
  /** Deploy only: Accelerate (Deployment & Use, high deployment need) or Watch */
  deployMode: DeployMode | null;
  /** Secure only */
  rationale: SecurityRationale | null;
  /** deployment at Deployment & Use, supply_chain elsewhere (the core's column when written, else by stage) */
  needType: NeedType;
  /** the relevant need (supply_chain_need or deployment_need), 0 to 1 */
  need: number | null;
  /** the prize the choice read, 0 to 1 (producing stages): the Strategic Industrial Value, or the market prize where meta.prize_measure is "market" */
  prize: number | null;
  /** the market prize (industrial_opportunity, cells.io), 0 to 1; where the core has no io, the prize itself when it is the market measure, else null */
  market: number | null;
  /** the node's yard roll-up (cells.yd); null when not assessed or not written */
  yards: YardRollup | null;
  /** Competitive Position percentile among the node's contenders, 0 to 1 */
  cp: number | null;
  /** the economy's share of world supply */
  share: number | null;
  pathway: Pathway | null;
  /** the inputs within the borderline band of their threshold, as written ("need", "prize;cp") */
  borderline: string | null;
  confidence: Confidence | null;
  /** the consistency flag (e.g. review_strong_position_low_prize) */
  flag: string | null;
  evidence: Evidence | null;
  /** the evidence word as R wrote it (high, medium, low, none) */
  evidenceRaw: string | null;
  alignment: Alignment | null;
  alignmentConfidence: AlignmentConfidence | null;
  alignmentFlag: string | null;
  partner1: string | null;
  partnerGap: boolean | null;
  momentum: Momentum | null;
  momentumQuality: string | null;
}

export interface DivisionNode {
  tech: string;
  stage: PostureStage;
  primary: string | null;
  primaryShare: number | null;
  backups: { iso3: string; share: number | null }[];
  redundancy: number;
  alliedShare: number | null;
  coerciveShare: number | null;
  leader: string | null;
  leaderShare: number | null;
  gap: AibGap;
  gaps: AibGap[];
  source: string | null;
  aibNodes: number | null;
  partnerDemand: number;
  secureDemand: number;
  partnerGaps: number;
  memberPartnerDemand: number;
}

export interface PostureCore {
  /** when scripts/91 wrote the core (ISO 8601), for the CSVs' as-of column; null when the document has none */
  created: string | null;
  /** the commit the export ran on, for the CSVs' provenance; null when the document has none */
  gitCommit: string | null;
  meta: PostureMeta;
  countries: { iso3: string; name: string; tier: string }[];
  cells: PostureCell[];
  division: DivisionNode[];
  aibHeader: { core_members: number; members: number; priority_nodes: number; covered_share: number | null; unresolved_gaps: number };
  /** policy design text (the mitigation of a Secure node, the fence / gate / build of any node) keyed variant|delivery */
  design: Record<string, { fence?: string; fence_level?: string; gate?: string; build?: string; benchmark?: string; exit?: string }>;
}

export interface PostureDetail {
  iso3: string;
  country: string;
  created: string | null;
  /** the 66 nodes, normalised (detail.ts) */
  cells: NodeDetail[];
}

const isObj = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null && !Array.isArray(v);
const asArr = (v: unknown): unknown[] => (Array.isArray(v) ? v : []);
const str = (v: unknown): string | null => (typeof v === 'string' && v !== '' ? v : null);
const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

/** Evidence as the grammar's three words: high / strong -> strong, medium / moderate -> moderate, low / weak / none -> weak; undefined for a word it does not know. */
export function normalizeEvidence(raw: unknown): Evidence | null | undefined {
  const s = str(raw);
  if (s === null) return null;
  const v = s.toLowerCase();
  if (v === 'high' || v === 'strong') return 'strong';
  if (v === 'medium' || v === 'moderate') return 'moderate';
  if (v === 'low' || v === 'weak' || v === 'none') return 'weak';
  return undefined;
}

/** Posture confidence: high / moderate (or medium) / low; undefined for a word it does not know. */
export function normalizeConfidence(raw: unknown): Confidence | null | undefined {
  const s = str(raw);
  if (s === null) return null;
  return s === 'high' ? 'high' : s === 'moderate' || s === 'medium' ? 'moderate' : s === 'low' ? 'low' : undefined;
}

/** Alignment confidence: high / medium (or moderate) / low; undefined for a word it does not know. */
export function normalizeAlignmentConfidence(raw: unknown): AlignmentConfidence | null | undefined {
  const s = str(raw);
  if (s === null) return null;
  return s === 'high' ? 'high' : s === 'medium' || s === 'moderate' ? 'medium' : s === 'low' ? 'low' : undefined;
}

export const ALIGNMENTS: readonly Alignment[] = ['aligned', 'under_prioritized', 'over_prioritized', 'misaligned', 'deployment_gap', 'mixed', 'insufficient_data'];
export const AIB_GAPS: readonly AibGap[] = [
  'none', 'no_trusted_supplier', 'single_point_of_failure', 'insufficient_redundancy', 'insufficient_allied_capacity', 'excessive_concentration', 'weak_evidence',
];
const POSTURE_CODES = new Set<string>(['S', 'C', 'D', 'N']);
const MODES = new Set<string>(['catch_up', 'preserve', 'partner']);
const DEPLOY_MODES = new Set<string>(['accelerate', 'watch']);
const RATIONALES = new Set<string>(['operational_control', 'coercive_chokepoint', 'systemic_resilience']);
const NEED_TYPES = new Set<string>(['supply_chain', 'deployment']);
const PATHWAYS = new Set<string>(['yes', 'no', 'unknown']);
const ALIGNMENT_SET = new Set<string>(ALIGNMENTS);
const MOMENTA = new Set<string>(['up', 'flat', 'down']);
const GAPS = new Set<string>(AIB_GAPS);
const WORDS = new Set<string>(['secure', 'compete', 'deploy', 'not_assessed']);

/** The need type of a stage: Deployment & Use reads deployment need, every other stage supply-chain need. */
export const needTypeOfStage = (stage: string): NeedType => (stage === DEPLOYMENT_STAGE ? 'deployment' : 'supply_chain');

function checkHeader(doc: unknown, what: string, problems: string[]): doc is Record<string, unknown> {
  if (!isObj(doc)) {
    problems.push(`${what} is not an object`);
    return false;
  }
  if (doc.schema !== POSTURE_SCHEMA) problems.push(`${what}: schema is not ${POSTURE_SCHEMA}`);
  if (typeof doc.schema_version !== 'number') problems.push(`${what}: no schema_version`);
  else if (doc.schema_version > POSTURE_SCHEMA_VERSION) problems.push(`${what}: schema_version ${doc.schema_version} needs a newer app`);
  else if (doc.schema_version < POSTURE_SCHEMA_VERSION) {
    problems.push(`${what}: schema_version ${doc.schema_version} is the retired Need x Opportunity posture; this app reads schema ${POSTURE_SCHEMA_VERSION} (Secure, Preserve, Catch Up, Partner, Accelerate, Watch)`);
  }
  return true;
}

export function checkPostureManifest(doc: unknown): PostureManifest {
  const p: string[] = [];
  if (checkHeader(doc, 'manifest', p)) {
    if (typeof doc.core !== 'string') p.push('manifest: no core file');
    if (!isObj(doc.details)) p.push('manifest: no details map');
  }
  if (p.length) throw new PostureContractError(p);
  return doc as unknown as PostureManifest;
}

/** R POSTURE_YARD_CODE: the code of each roll-up in cells.yd (the default of meta.yard_codes). */
export const YARD_CODES: Readonly<Record<YardRollup, string>> = {
  small: 'S', medium: 'M', large: 'L', mixed_high_medium: 'SM', mixed_medium_low: 'ML', mixed_high_low: 'SL', mixed_high_medium_low: 'SML',
};
export const YARD_ROLLUPS = Object.keys(YARD_CODES) as YardRollup[];
/** R POSTURE_LY_SIGNALS (strategic_value.R SP_LY_SIGNALS), in order: the default of meta.large_yard_signals. */
export const LARGE_YARD_SIGNALS: readonly string[] = [
  'mature', 'passive', 'standardized', 'substitutable', 'easy_qualification', 'diversified', 'low_learning', 'low_spillover', 'low_differentiation', 'commoditized',
];
/** The yards a roll-up spans, small to large. */
export const ROLLUP_YARDS: Readonly<Record<YardRollup, readonly Yard[]>> = {
  small: ['small'], medium: ['medium'], large: ['large'],
  mixed_high_medium: ['small', 'medium'], mixed_medium_low: ['medium', 'large'], mixed_high_low: ['small', 'large'], mixed_high_medium_low: ['small', 'medium', 'large'],
};
/** The yard of a choice (R sp_yard_of_choice): Secure small; Preserve, Catch Up, Partner and Accelerate medium; Watch large. */
export const yardOfVerb = (v: Verb | null): Yard | null => (v === null ? null : v === 'secure' ? 'small' : v === 'watch' ? 'large' : 'medium');

const DEFAULT_THRESHOLDS: PostureThresholds = { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.67, borderline_band: 0.05 };
const DEFAULT_SCENARIO_LABELS: Record<string, string> = {
  current_policy: 'Current policy',
  national_commitments: 'National commitments',
  climate_aligned: 'Climate-aligned',
};

/** meta.thresholds {need, prize, cp, deployment, borderline_band}; flat need_threshold ... keys are read too. */
function decodeThresholds(meta: Record<string, unknown>, p: string[]): PostureThresholds {
  const t = isObj(meta.thresholds) ? meta.thresholds : {};
  const pick = (k: keyof PostureThresholds, ...flat: string[]): number => {
    const v = num(t[k]) ?? flat.map((f) => num(meta[f])).find((x) => x !== null) ?? null;
    if (v === null) {
      p.push(`core: meta.thresholds.${k} is missing`);
      return DEFAULT_THRESHOLDS[k];
    }
    return v;
  };
  return {
    need: pick('need', 'need_threshold'),
    prize: pick('prize', 'prize_threshold', 'opportunity_threshold'),
    cp: pick('cp', 'cp_threshold'),
    deployment: pick('deployment', 'deployment_threshold'),
    borderline_band: pick('borderline_band', 'borderline_band'),
  };
}

function decodeMeta(meta: Record<string, unknown>, techs: string[], stages: string[], p: string[]): PostureMeta {
  const sd = isObj(meta.stage_display) ? meta.stage_display : {};
  const stage_display = Object.fromEntries(stages.map((s) => [s, str(sd[s]) ?? STAGE_DISPLAY[s as SupplyStage] ?? s]));
  const sl = isObj(meta.scenario_labels) ? meta.scenario_labels : isObj(meta.scenarios) ? meta.scenarios : {};
  const scenario_labels = { ...DEFAULT_SCENARIO_LABELS, ...Object.fromEntries(Object.entries(sl).filter(([, v]) => typeof v === 'string')) } as Record<string, string>;
  const fs = isObj(meta.family_scenarios) ? meta.family_scenarios : {};
  const family_scenarios: FamilyScenarios = {
    demand: str(fs.demand) ?? 'current_policy',
    policy_gap: str(fs.policy_gap) ?? 'national_commitments',
    system: str(fs.system),
  };
  const ph = isObj(meta.phrases) ? meta.phrases : {};
  const phrases = Object.fromEntries(Object.entries(ph).filter(([, v]) => typeof v === 'string')) as Record<string, string>;
  const pm = str(meta.prize_measure);
  if (pm !== null && pm !== 'strategic_value' && pm !== 'market') p.push(`core: meta.prize_measure '${pm}' is not known`);
  const yc = isObj(meta.yard_codes) ? meta.yard_codes : {};
  const yard_codes: Record<string, YardRollup> = Object.fromEntries(YARD_ROLLUPS.map((r) => [YARD_CODES[r], r]));
  for (const [r, code] of Object.entries(yc)) {
    if (!(YARD_ROLLUPS as string[]).includes(r)) p.push(`core: meta.yard_codes names an unknown roll-up '${r}'`);
    else if (typeof code === 'string' && code !== '') yard_codes[code] = r as YardRollup;
  }
  const lys = Array.isArray(meta.large_yard_signals) ? meta.large_yard_signals.map(String) : [...LARGE_YARD_SIGNALS];
  return {
    techs, stages, stage_display,
    benchmark_period: str(meta.benchmark_period),
    momentum_period: str(meta.momentum_period),
    momentum_previous_period: str(meta.momentum_previous_period),
    momentum_through: str(meta.momentum_through),
    momentum_band: num(meta.momentum_band) ?? 0.1,
    thresholds: decodeThresholds(meta, p),
    scenario_labels,
    family_scenarios,
    phrases,
    core_members: num(meta.core_members) ?? 0,
    partner_members: num(meta.partner_members) ?? 0,
    prize_measure: pm === 'strategic_value' ? 'strategic_value' : 'market',
    yard_codes,
    large_yard_signals: lys,
    yf_nodes: decodeYfNodes(meta.yf_nodes, lys, techs, stages, p),
  };
}

/**
 * meta.yf_nodes (columnar: key "tech|stage|layer", label, case, n, s, u, floor, commons, option, commons_n,
 * option_n, cap; s and u 0-based indexes into the signal names) as nodes by key; empty when absent.
 */
function decodeYfNodes(raw: unknown, signals: readonly string[], techs: readonly string[], stages: readonly string[], p: string[]): Record<string, YfNode> {
  if (!isObj(raw)) return {};
  const keys = asArr(raw.key);
  const col = (n: string) => asArr(raw[n]);
  for (const n of ['label', 'case', 'n', 's', 'u', 'floor', 'commons', 'option', 'commons_n', 'option_n', 'cap']) {
    if (Array.isArray(raw[n]) && col(n).length !== keys.length) p.push(`core: meta.yf_nodes.${n} has ${col(n).length} values, not ${keys.length}`);
  }
  const out: Record<string, YfNode> = {};
  let badKey = 0;
  let badIdx = 0;
  const names = (v: unknown): string[] =>
    asArr(v).map((i) => {
      const k = num(i);
      const s = k !== null && Number.isInteger(k) ? signals[k] : undefined;
      if (s === undefined) badIdx++;
      return s ?? '?';
    });
  keys.forEach((k, i) => {
    const parts = typeof k === 'string' ? k.split('|') : [];
    if (parts.length !== 3 || !techs.includes(parts[0]) || !stages.includes(parts[1]) || !parts[2]) {
      badKey++;
      return;
    }
    const cs = col('case')[i];
    const fl = col('floor')[i];
    out[k as string] = {
      key: k as string, tech: parts[0], stage: parts[1], layer: parts[2], label: str(col('label')[i]),
      largeYard: typeof cs === 'boolean' ? cs : null, n: num(col('n')[i]), signals: names(col('s')[i]), unknown: names(col('u')[i]),
      floor: typeof fl === 'boolean' ? fl : null, commons: num(col('commons')[i]), option: num(col('option')[i]),
      commonsN: num(col('commons_n')[i]), optionN: num(col('option_n')[i]), cap: str(col('cap')[i]),
    };
  });
  if (badKey) p.push(`core: ${badKey} meta.yf_nodes ${badKey === 1 ? 'key is' : 'keys are'} not a technology|stage|layer of the core`);
  if (badIdx) p.push(`core: ${badIdx} meta.yf_nodes signal ${badIdx === 1 ? 'index points' : 'indexes point'} outside meta.large_yard_signals`);
  return out;
}

/** The columns every core must carry (others are optional; a column that is present has one value per cell). */
const REQUIRED_COLUMNS = ['c', 't', 's', 'p', 'm', 'dm', 'need', 'prize', 'cp'] as const;
const OPTIONAL_COLUMNS = ['v', 'sec', 'nt', 'share', 'pw', 'bd', 'conf', 'fl', 'ev', 'al', 'alc', 'alf', 'p1', 'gap', 'mom', 'mq', 'io', 'yd'] as const;

/** Check and decode the columnar core document (schema 2). */
export function decodeCore(doc: unknown): PostureCore {
  const p: string[] = [];
  if (!checkHeader(doc, 'core', p)) throw new PostureContractError(p);
  if (p.length) throw new PostureContractError(p);
  const metaRaw = isObj(doc.meta) ? doc.meta : null;
  if (!metaRaw) p.push('core: no meta');
  const techs = asArr(metaRaw?.techs).map(String);
  const stages = asArr(metaRaw?.stages).map(String);
  if (stages.join('|') !== POSTURE_STAGES.join('|')) p.push(`core: stages are not the six stages (${stages.join(', ')})`);
  const meta = decodeMeta(metaRaw ?? {}, techs, stages, p);
  const c = isObj(doc.countries) ? doc.countries : {};
  const iso = asArr(c.iso3).map(String);
  const names = asArr(c.name);
  const tiers = asArr(c.tier);
  const countries = iso.map((i, k) => ({ iso3: i, name: str(names[k]) ?? i, tier: str(tiers[k]) ?? 'none' }));
  const k = isObj(doc.cells) ? doc.cells : {};
  const has = (name: string) => Array.isArray(k[name]);
  const n = asArr(k.p).length;
  for (const name of REQUIRED_COLUMNS) {
    if (!has(name)) p.push(`core: cells.${name} is missing`);
    else if (asArr(k[name]).length !== n) p.push(`core: cells.${name} has ${asArr(k[name]).length} values, not ${n}`);
  }
  for (const name of OPTIONAL_COLUMNS) if (has(name) && asArr(k[name]).length !== n) p.push(`core: cells.${name} has ${asArr(k[name]).length} values, not ${n}`);
  // an absent optional column reads as all null
  const col = (name: string): unknown[] => (has(name) ? asArr(k[name]) : new Array<unknown>(n).fill(null));
  const cells: PostureCell[] = [];
  if (!p.length) {
    const [cc, tt, ss, pp, vv, mm, dmm, secc, ntt, need, prize, cp, share, pw, bd, conf, fl, ev, al, alc, alf, p1, gap, mom, mq, io, yd] = [
      'c', 't', 's', 'p', 'v', 'm', 'dm', 'sec', 'nt', 'need', 'prize', 'cp', 'share', 'pw', 'bd', 'conf', 'fl', 'ev', 'al', 'alc', 'alf', 'p1', 'gap', 'mom', 'mq', 'io', 'yd',
    ].map(col);
    // without a market column, the prize is the market prize only where the core says it is
    const marketIsPrize = !has('io') && meta.prize_measure === 'market';
    const bad = new Map<string, number>();
    const count = (why: string) => bad.set(why, (bad.get(why) ?? 0) + 1);
    for (let i = 0; i < n; i++) {
      const code = str(pp[i]);
      const ci = num(cc[i]);
      const ti = num(tt[i]);
      const si = num(ss[i]);
      if (!code || !POSTURE_CODES.has(code) || ci === null || ti === null || si === null || !iso[ci] || !techs[ti] || !stages[si]) {
        count('an unknown posture or index');
        continue;
      }
      const stage = stages[si] as PostureStage;
      const m = str(mm[i]);
      const dm = str(dmm[i]);
      const sec = str(secc[i]);
      const nt = str(ntt[i]);
      const pwv = str(pw[i]);
      const alv = str(al[i]);
      const momv = str(mom[i]);
      if (m !== null && !MODES.has(m)) count('an unknown compete mode');
      if (dm !== null && !DEPLOY_MODES.has(dm)) count('an unknown deploy mode');
      if (sec !== null && !RATIONALES.has(sec)) count('an unknown Secure rationale');
      if (nt !== null && !NEED_TYPES.has(nt)) count('an unknown need type');
      if (pwv !== null && !PATHWAYS.has(pwv)) count('an unknown pathway value');
      if (alv !== null && !ALIGNMENT_SET.has(alv)) count('an unknown policy alignment');
      if (momv !== null && !MOMENTA.has(momv)) count('an unknown momentum');
      // the framework's invariants (spec VIII): a Compete node has a mode and a Deploy node a deploy mode, and
      // only those; Deployment & Use never becomes Compete (no Partner there), and Accelerate is a Deployment choice
      if (code === 'C' && (m === null || dm !== null)) count('a Compete node without its mode alone');
      if (code === 'D' && (dm === null || m !== null)) count('a Deploy node without its deploy mode alone');
      if ((code === 'S' || code === 'N') && (m !== null || dm !== null)) count('a Secure or not-assessed node with a mode');
      if (code === 'C' && stage === DEPLOYMENT_STAGE) count('a Compete node at Deployment');
      if (dm === 'accelerate' && stage !== DEPLOYMENT_STAGE) count('Accelerate outside Deployment');
      const evidence = normalizeEvidence(ev[i]);
      if (evidence === undefined) count('an unknown evidence word');
      const confidence = normalizeConfidence(conf[i]);
      if (confidence === undefined) count('an unknown posture confidence');
      const alignmentConfidence = normalizeAlignmentConfidence(alc[i]);
      if (alignmentConfidence === undefined) count('an unknown alignment confidence');
      const ydv = str(yd[i]);
      const yards = ydv !== null ? (meta.yard_codes[ydv] ?? null) : null;
      if (ydv !== null && yards === null) count('an unknown yard roll-up code');
      // a roll-up holds the node's own yard (R yard_rollup.R: the stage row is one of its units)
      const own = yardOfVerb(code === 'S' ? 'secure' : code === 'C' && m !== null && MODES.has(m) ? (m as Verb) : code === 'D' && dm !== null && DEPLOY_MODES.has(dm) ? (dm as Verb) : null);
      if (yards !== null && own !== null && !ROLLUP_YARDS[yards].includes(own)) count("a yard roll-up without the node's own yard");
      cells.push({
        iso3: iso[ci], tech: techs[ti], stage, posture: code as PostureCode, variant: str(vv[i]),
        mode: m !== null && MODES.has(m) ? (m as CompeteMode) : null,
        deployMode: dm !== null && DEPLOY_MODES.has(dm) ? (dm as DeployMode) : null,
        rationale: sec !== null && RATIONALES.has(sec) ? (sec as SecurityRationale) : null,
        needType: nt !== null && NEED_TYPES.has(nt) ? (nt as NeedType) : needTypeOfStage(stage),
        need: num(need[i]), prize: num(prize[i]), market: marketIsPrize ? num(prize[i]) : num(io[i]), yards, cp: num(cp[i]), share: num(share[i]),
        pathway: pwv !== null && PATHWAYS.has(pwv) ? (pwv as Pathway) : null,
        borderline: str(bd[i]), confidence: confidence ?? null, flag: str(fl[i]),
        evidence: evidence ?? null, evidenceRaw: str(ev[i]),
        alignment: alv !== null && ALIGNMENT_SET.has(alv) ? (alv as Alignment) : null,
        alignmentConfidence: alignmentConfidence ?? null, alignmentFlag: str(alf[i]),
        partner1: str(p1[i]), partnerGap: typeof gap[i] === 'boolean' ? (gap[i] as boolean) : null,
        momentum: momv !== null && MOMENTA.has(momv) ? (momv as Momentum) : null, momentumQuality: str(mq[i]),
      });
    }
    for (const [why, k2] of bad) p.push(`core: ${k2} ${k2 === 1 ? 'cell has' : 'cells have'} ${why}`);
  }
  const d = isObj(doc.division) ? doc.division : {};
  const dc = (n2: string) => asArr(d[n2]);
  const division: DivisionNode[] = dc('t').map((t, i) => {
    const gapsStr = str(dc('gaps')[i]);
    const gap = str(dc('gap')[i]) ?? 'none';
    if (!GAPS.has(gap)) p.push(`core: division gap '${gap}' is not known`);
    const gaps = gapsStr ? gapsStr.split(';') : [];
    for (const g of gaps) if (!GAPS.has(g)) p.push(`core: division gap '${g}' is not known`);
    const backups = [1, 2]
      .map((b) => ({ iso3: str(dc(`backup_${b}`)[i]), share: num(dc(`backup_${b}_share`)[i]) }))
      .filter((b): b is { iso3: string; share: number | null } => b.iso3 !== null);
    return {
      tech: techs[num(t) ?? -1] ?? '?', stage: (stages[num(dc('s')[i]) ?? -1] ?? 'Extraction') as PostureStage,
      primary: str(dc('primary')[i]), primaryShare: num(dc('primary_share')[i]), backups,
      redundancy: num(dc('redundancy')[i]) ?? 0, alliedShare: num(dc('allied_share')[i]), coerciveShare: num(dc('coercive_share')[i]),
      leader: str(dc('leader')[i]), leaderShare: num(dc('leader_share')[i]), gap: gap as AibGap,
      gaps: gaps as AibGap[], source: str(dc('source')[i]), aibNodes: num(dc('aib_nodes')[i]),
      partnerDemand: num(dc('partner_demand')[i]) ?? 0, secureDemand: num(dc('secure_demand')[i]) ?? 0,
      partnerGaps: num(dc('partner_gaps')[i]) ?? 0, memberPartnerDemand: num(dc('member_partner_demand')[i]) ?? 0,
    };
  });
  if (p.length) throw new PostureContractError(p);
  const h = (isObj(doc.aib_header) ? doc.aib_header : {}) as PostureCore['aibHeader'];
  const design = (isObj(doc.design) ? doc.design : {}) as PostureCore['design'];
  return { created: str(doc.created), gitCommit: str(doc.git_commit), meta, countries, cells, division, aibHeader: h, design };
}

/** Check the detail document (one economy, schema 2) and normalise its cells (detail.ts). */
export function checkPostureDetail(doc: unknown): PostureDetail {
  const p: string[] = [];
  const cells: NodeDetail[] = [];
  if (checkHeader(doc, 'detail', p) && !p.length) {
    if (typeof doc.iso3 !== 'string') p.push('detail: no iso3');
    if (!Array.isArray(doc.cells)) p.push('detail: no cells');
    else
      for (const c of doc.cells) {
        if (!isObj(c) || typeof c.tech !== 'string' || !(POSTURE_STAGES as readonly unknown[]).includes(c.stage) || !WORDS.has(String(c.posture))) {
          p.push('detail: a cell without tech, six-stage stage or posture (secure, compete, deploy or not_assessed)');
          break;
        }
        cells.push(nodeDetail(c));
      }
  }
  if (p.length) throw new PostureContractError(p);
  const d = doc as Record<string, unknown>;
  return { iso3: d.iso3 as string, country: str(d.country) ?? (d.iso3 as string), created: str(d.created), cells };
}
