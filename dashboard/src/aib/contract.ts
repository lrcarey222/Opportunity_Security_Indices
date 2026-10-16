// Sectoral Compacts data contract (schema osi-aib v1). The interface between
// scripts/84_export_aib_data.R (R/dashboard/aib/aib_export.R, which writes these files) and the views
// in src/aib/. Pure: no IO, no React.
//
// Files under <base>data/aib/:
//   manifest.json            AibManifest (no-cache; written last, names the current document)
//   compacts.<h8>.json       AibDoc: meta, country names, compacts and every classified node (immutable)
//
// Shares are 0-1 (the UI prints them as %); USD values are dollars (the UI prints $m / $bn). Arrays are
// always arrays (the exporter wraps every array field in I()). Numbers may be null where R had NA.
// Strings the exporter can write as NA (a node's data quality, its baseline largest supplier, ...) are
// typed `string | null` here and tolerated by the checks.
//
// Runtime checks: checkAibManifest() and checkCompactsDoc() return problem strings (empty = valid);
// the store throws AibContractError with them.
//
// Additive fields (2026-10-15, after the R review; still v1): Compact.metrics.globalOptimum,
// anchorFragmented, anchorCompact, compactGain; Compact.classMix, secondaryClass; Node.tightestCostless,
// minRequiredDiversification; frontier[].secondShock, specialization; and the compact type "compact
// unnecessary: ...". They are optional here: an earlier bundle without them still validates. From the
// same review, metrics.trade is "the trade compact": every economy keeps its existing-relationship
// response and the coalition's members may also open or expand relationships, so it is at least
// metrics.existing; metrics.investment adds the standard
// increment at up to two member hosts per BUILD node.

// ---- Schema ----------------------------------------------------------------------------------------

export const AIB_SCHEMA = 'osi-aib';
export const AIB_VERSION = 1;
/** Relative to the app's base URL. Written only by scripts/84. */
export const AIB_DATA_DIR = 'data/aib/';
export const AIB_MANIFEST_FILE = 'manifest.json';
/** The document file name scripts/84 writes (and the only kind it deletes). */
export const AIB_DOC_FILE_RE = /^compacts\.[0-9a-f]{8}\.json$/;
/** Path segments under /compacts that are views, not compact ids. */
export const AIB_RESERVED = ['about'] as const;

// ---- Enums -----------------------------------------------------------------------------------------

/** The node classes in the order the views list them (meta.classes carries the same list). */
export const AIB_CLASSES = ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA'] as const;
export type AibClass = (typeof AIB_CLASSES)[number];

export const AIB_ARCHETYPES = ['Trade / Coordination Compact', 'Investment / Capacity Compact', 'Specialization Compact', 'Integrated Compact'] as const;
export type AibArchetype = (typeof AIB_ARCHETYPES)[number];

/** The compact types; "compact unnecessary": the coalition barely improves the demand anchors' coverage
 *  over what their existing relationships give them (fragmented action). */
export const AIB_COMPACT_TYPES = ['candidate compact', 'bilateral arrangement sufficient', 'compact unnecessary: existing relationships do nearly as well'] as const;
export type AibCompactType = (typeof AIB_COMPACT_TYPES)[number];

export const AIB_RELATIONSHIPS = ['new', 'expanded'] as const;
export type AibRelationship = (typeof AIB_RELATIONSHIPS)[number];

// ---- Manifest ----------------------------------------------------------------------------------------

export interface AibManifest {
  schema: typeof AIB_SCHEMA;
  version: number;
  created: string;
  universe: string;
  headlineShock: string;
  headlineScenario: string;
  files: { compacts: string };
}

// ---- Document ----------------------------------------------------------------------------------------

export interface AibShock {
  id: string;
  label: string;
}

export interface AibMeta {
  universe: string;
  headlineShock: string;
  headlineScenario: string;
  tradeYear: number;
  built: string;
  shocks: AibShock[];
  scenarios: string[];
  /** the five classes, in order (AIB_CLASSES) */
  classes: string[];
  /** coverage targets of the minimum sufficient coalition, e.g. [0.9, 0.95, 0.99] */
  targets: number[];
  /** the classification settings (config/aib.yml `classification`) */
  thresholds: Record<string, number>;
}

export interface AibCoalition {
  /** the minimum sufficient coalition (the middle target) */
  members: string[];
  t90: string[];
  t95: string[];
  t99: string[];
  reached99: boolean;
  maxCoverage: number | null;
}

export interface AibRole {
  iso3: string;
  role: string;
  basis: string;
  /** in the minimum coalition (false: an associate that joins only for the highest target) */
  member: boolean;
}

export interface AibMetrics {
  demandUsd: number | null;
  lostUsd: number | null;
  exposure: number | null;
  static: number | null;
  existing: number | null;
  trade: number | null;
  investment: number | null;
  dividend: number | null;
  residual: number | null;
  residualAfterInvestment: number | null;
  /** coverage if every economy coordinated (the global optimum; additive) */
  globalOptimum?: number | null;
  /** the demand anchors' coverage through existing relationships: fragmented action (additive) */
  anchorFragmented?: number | null;
  /** the demand anchors' coverage under the compact's minimum coalition (additive) */
  anchorCompact?: number | null;
  /** anchorCompact - anchorFragmented (additive) */
  compactGain?: number | null;
}

export interface AibPathStep {
  step: number;
  /** the economy added at this step (null at step 0: the demand anchors alone) */
  added: string | null;
  members: string[];
  minCoverage: number | null;
  meanCoverage: number | null;
  /** node id -> coverage of the anchors' demand (the exporter writes [] when empty) */
  nodeCoverage: Record<string, number>;
}

export interface AibTradeEdge {
  from: string;
  to: string;
  node: string;
  usd: number | null;
  share: number | null;
  relationship: AibRelationship;
  opportunity: number | null;
}

export interface AibInvestmentEdge {
  investor: string;
  host: string;
  node: string;
  usd: number | null;
  share: number | null;
  friendshore: number | null;
}

export interface AibCompact {
  /** "AIB-C01" */
  id: string;
  name: string;
  technologies: string[];
  stages: string[];
  archetype: AibArchetype;
  type: AibCompactType;
  /** node ids */
  nodes: string[];
  coalition: AibCoalition;
  anchors: string[];
  suppliers: string[];
  hosts: string[];
  technologyPartners: string[];
  bridges: string[];
  associates: string[];
  roles: AibRole[];
  metrics: AibMetrics;
  /** lost-supply shares of the intervention classes, "BUILD 78%; COORDINATE 22%" (additive) */
  classMix?: string | null;
  /** the second class when it holds enough of the lost supply to keep its instruments (additive) */
  secondaryClass?: string | null;
  /** the instruments of every archetype whose class holds enough of the lost supply */
  policyNeeds: string[];
  path: AibPathStep[];
  trade: AibTradeEdge[];
  investment: AibInvestmentEdge[];
}

export interface AibFrontierPoint {
  /** the supplier-share limit: no candidate exporter may provide more than this share */
  cap: number;
  coverage: number | null;
  largestShare: number | null;
  largest: string | null;
  /** coverage if the optimized allocation then also lost its largest supplier (additive) */
  secondShock?: number | null;
  /** supply-weighted specialization score of the optimized supply, 0-1 (additive) */
  specialization?: number | null;
}

export interface AibNode {
  id: string;
  label: string;
  techs: string[];
  stage: string;
  cls: string;
  reason: string | null;
  quality: string | null;
  flags: string[];
  exposure: number | null;
  static: number | null;
  existing: number | null;
  optimized: number | null;
  dividend: number | null;
  residual: number | null;
  demandUsd: number | null;
  lostUsd: number | null;
  baseLargest: string | null;
  baseLargestShare: number | null;
  largest: string | null;
  largestShare: number | null;
  hhi: number | null;
  hosts: string[];
  bestGain: number | null;
  partners: string | null;
  treatment: string | null;
  compact: string | null;
  yfCells: string[];
  yfSubcomponents: string[];
  /** the tightest supplier-share limit that costs at most 2 points of coverage (additive) */
  tightestCostless?: number | null;
  /** the loosest limit at which coverage stays >= 80% after a second shock; null when none does (additive) */
  minRequiredDiversification?: number | null;
  frontier: AibFrontierPoint[];
}

export interface AibDoc {
  schema: typeof AIB_SCHEMA;
  version: number;
  meta: AibMeta;
  /** ISO3 -> name for every code used (the exporter writes [] when there is none) */
  countries: Record<string, string>;
  /** ordered by lost supply, largest first */
  compacts: AibCompact[];
  /** every classified sub-sector node, in a compact or not */
  nodes: AibNode[];
}

// ---- Runtime checks ----------------------------------------------------------------------------------

export class AibContractError extends Error {
  readonly problems: string[];
  constructor(what: string, problems: string[]) {
    super(`${what}: ${problems.slice(0, 5).join('; ')}${problems.length > 5 ? ` (+${problems.length - 5} more)` : ''}`);
    this.name = 'AibContractError';
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
  str(x: unknown, path: string, nullable = false): void {
    if (!(isStr(x) || (nullable && x === null))) this.add(`${path}: expected a string${nullable ? ' or null' : ''}`);
  }
  /** a finite number, or null when nullable; optional bounds */
  num(x: unknown, path: string, opts: { nullable?: boolean; min?: number; max?: number; int?: boolean } = {}): void {
    if (x === null && opts.nullable) return;
    if (!isNum(x)) {
      this.add(`${path}: expected a number${opts.nullable ? ' or null' : ''}`);
      return;
    }
    if (opts.min !== undefined && x < opts.min - 1e-9) this.add(`${path}: ${x} below ${opts.min}`);
    if (opts.max !== undefined && x > opts.max + 1e-9) this.add(`${path}: ${x} above ${opts.max}`);
    if (opts.int && !Number.isInteger(x)) this.add(`${path}: ${x} is not an integer`);
  }
  /** an additive field: absent is fine, else a number or null with the bounds */
  optNum(x: unknown, path: string, opts: { min?: number; max?: number } = {}): void {
    if (x === undefined) return;
    this.num(x, path, { ...opts, nullable: true });
  }
  /** an additive field: absent is fine, else a string or null */
  optStr(x: unknown, path: string): void {
    if (x === undefined) return;
    this.str(x, path, true);
  }
  bool(x: unknown, path: string): void {
    if (typeof x !== 'boolean') this.add(`${path}: expected true or false`);
  }
  strArr(x: unknown, path: string): x is string[] {
    if (!Array.isArray(x)) {
      this.add(`${path}: expected an array${isStr(x) ? ' (a one-item array collapsed to a string?)' : ''}`);
      return false;
    }
    const bad = x.findIndex((v) => !isStr(v));
    if (bad >= 0) {
      this.add(`${path}[${bad}]: expected a string`);
      return false;
    }
    return true;
  }
  arr(x: unknown, path: string): x is unknown[] {
    if (!Array.isArray(x)) {
      this.add(`${path}: expected an array${isObj(x) ? ' (a one-item array collapsed to an object?)' : ''}`);
      return false;
    }
    return true;
  }
  oneOf<T>(x: unknown, vals: readonly T[], path: string): void {
    if (!vals.includes(x as T)) this.add(`${path}: '${String(x)}' is not one of ${vals.join(', ')}`);
  }
}

/** A share in [0, 1] (or null); a small tolerance for rounding. */
const SHARE = { nullable: true, min: 0, max: 1 } as const;

/** Problems with data/aib/manifest.json (empty when it is valid). */
export function checkAibManifest(x: unknown): string[] {
  if (!isObj(x)) return ['manifest: not an object'];
  const p = new Problems();
  if (x.schema !== AIB_SCHEMA) p.add(`manifest.schema: expected '${AIB_SCHEMA}'`);
  if (!isNum(x.version)) p.add('manifest.version: not a number');
  else if (Math.floor(x.version) > AIB_VERSION) p.add(`manifest: version ${x.version} needs a newer app`);
  for (const k of ['created', 'universe', 'headlineShock', 'headlineScenario']) p.str(x[k], `manifest.${k}`);
  if (!isObj(x.files)) p.add('manifest.files: not an object');
  else if (!isStr(x.files.compacts) || !AIB_DOC_FILE_RE.test(x.files.compacts)) p.add('manifest.files.compacts: not a compacts.<8 hex>.json file name');
  return p.list;
}

/** True when a manifest's version is newer than this app reads (the module asks for a reload). */
export const needsNewerApp = (problems: readonly string[]): boolean => problems.some((q) => /needs a newer app/.test(q));

function checkMeta(p: Problems, m: unknown): void {
  if (!isObj(m)) return p.add('meta: not an object');
  for (const k of ['universe', 'headlineShock', 'headlineScenario', 'built']) p.str(m[k], `meta.${k}`);
  p.num(m.tradeYear, 'meta.tradeYear', { int: true });
  if (p.arr(m.shocks, 'meta.shocks')) {
    m.shocks.forEach((s, i) => {
      if (!isObj(s)) return p.add(`meta.shocks[${i}]: not an object`);
      p.str(s.id, `meta.shocks[${i}].id`);
      p.str(s.label, `meta.shocks[${i}].label`);
    });
  }
  p.strArr(m.scenarios, 'meta.scenarios');
  if (p.strArr(m.classes, 'meta.classes')) {
    for (const c of m.classes) if (!(AIB_CLASSES as readonly string[]).includes(c)) p.add(`meta.classes: '${c}' is not one of ${AIB_CLASSES.join(', ')}`);
  }
  if (p.arr(m.targets, 'meta.targets')) m.targets.forEach((t, i) => p.num(t, `meta.targets[${i}]`, { min: 0, max: 1 }));
  if (!isObj(m.thresholds)) p.add('meta.thresholds: not an object');
  else for (const [k, v] of Object.entries(m.thresholds)) p.num(v, `meta.thresholds.${k}`);
}

function checkNode(p: Problems, n: unknown, at: string, classes: readonly string[]): void {
  if (!isObj(n)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'label', 'stage']) p.str(n[k], `${at}.${k}`);
  if (!isStr(n.cls)) p.add(`${at}.cls: expected a string`);
  else if (!classes.includes(n.cls)) p.add(`${at}.cls: '${n.cls}' is not one of meta.classes`);
  for (const k of ['reason', 'quality', 'baseLargest', 'largest', 'partners', 'treatment', 'compact']) p.str(n[k], `${at}.${k}`, true);
  for (const k of ['techs', 'flags', 'hosts', 'yfCells', 'yfSubcomponents']) p.strArr(n[k], `${at}.${k}`);
  for (const k of ['exposure', 'static', 'existing', 'optimized', 'residual', 'baseLargestShare', 'largestShare', 'hhi']) p.num(n[k], `${at}.${k}`, SHARE);
  // the dividend and the best marginal gain are differences of shares
  for (const k of ['dividend', 'bestGain']) p.num(n[k], `${at}.${k}`, { nullable: true, min: -1, max: 1 });
  for (const k of ['demandUsd', 'lostUsd']) p.num(n[k], `${at}.${k}`, { nullable: true, min: 0 });
  for (const k of ['tightestCostless', 'minRequiredDiversification']) p.optNum(n[k], `${at}.${k}`, { min: 0, max: 1 });
  if (p.arr(n.frontier, `${at}.frontier`)) {
    n.frontier.forEach((f, i) => {
      const fa = `${at}.frontier[${i}]`;
      if (!isObj(f)) return p.add(`${fa}: not an object`);
      p.num(f.cap, `${fa}.cap`, { min: 0, max: 1 });
      p.num(f.coverage, `${fa}.coverage`, SHARE);
      p.num(f.largestShare, `${fa}.largestShare`, SHARE);
      p.str(f.largest, `${fa}.largest`, true);
      p.optNum(f.secondShock, `${fa}.secondShock`, { min: 0, max: 1 });
      p.optNum(f.specialization, `${fa}.specialization`, { min: 0, max: 1 });
    });
  }
}

function checkCompact(p: Problems, c: unknown, at: string, nodeIds: ReadonlySet<string>): void {
  if (!isObj(c)) return p.add(`${at}: not an object`);
  for (const k of ['id', 'name']) p.str(c[k], `${at}.${k}`);
  p.oneOf(c.archetype, AIB_ARCHETYPES, `${at}.archetype`);
  p.oneOf(c.type, AIB_COMPACT_TYPES, `${at}.type`);
  for (const k of ['technologies', 'stages', 'anchors', 'suppliers', 'hosts', 'technologyPartners', 'bridges', 'associates', 'policyNeeds']) p.strArr(c[k], `${at}.${k}`);
  if (p.strArr(c.nodes, `${at}.nodes`)) {
    if (!c.nodes.length) p.add(`${at}.nodes: a compact without nodes`);
    for (const id of c.nodes) if (!nodeIds.has(id)) p.add(`${at}.nodes: '${id}' is not a node of the document`);
  }
  const co = c.coalition;
  if (!isObj(co)) p.add(`${at}.coalition: not an object`);
  else {
    for (const k of ['members', 't90', 't95', 't99']) p.strArr(co[k], `${at}.coalition.${k}`);
    p.bool(co.reached99, `${at}.coalition.reached99`);
    p.num(co.maxCoverage, `${at}.coalition.maxCoverage`, SHARE);
  }
  if (p.arr(c.roles, `${at}.roles`)) {
    c.roles.forEach((r, i) => {
      const ra = `${at}.roles[${i}]`;
      if (!isObj(r)) return p.add(`${ra}: not an object`);
      for (const k of ['iso3', 'role', 'basis']) p.str(r[k], `${ra}.${k}`);
      p.bool(r.member, `${ra}.member`);
    });
  }
  const m = c.metrics;
  if (!isObj(m)) p.add(`${at}.metrics: not an object`);
  else {
    for (const k of ['demandUsd', 'lostUsd']) p.num(m[k], `${at}.metrics.${k}`, { nullable: true, min: 0 });
    for (const k of ['exposure', 'static', 'existing', 'trade', 'investment', 'residual', 'residualAfterInvestment']) p.num(m[k], `${at}.metrics.${k}`, SHARE);
    p.num(m.dividend, `${at}.metrics.dividend`, { nullable: true, min: -1, max: 1 });
    for (const k of ['globalOptimum', 'anchorFragmented', 'anchorCompact']) p.optNum(m[k], `${at}.metrics.${k}`, { min: 0, max: 1 });
    p.optNum(m.compactGain, `${at}.metrics.compactGain`, { min: -1, max: 1 });
  }
  p.optStr(c.classMix, `${at}.classMix`);
  p.optStr(c.secondaryClass, `${at}.secondaryClass`);
  if (p.arr(c.path, `${at}.path`)) {
    c.path.forEach((s, i) => {
      const sa = `${at}.path[${i}]`;
      if (!isObj(s)) return p.add(`${sa}: not an object`);
      p.num(s.step, `${sa}.step`, { min: 0, int: true });
      p.str(s.added, `${sa}.added`, true);
      p.strArr(s.members, `${sa}.members`);
      p.num(s.minCoverage, `${sa}.minCoverage`, SHARE);
      p.num(s.meanCoverage, `${sa}.meanCoverage`, SHARE);
      const nc = s.nodeCoverage;
      if (Array.isArray(nc) ? nc.length > 0 : !isObj(nc)) p.add(`${sa}.nodeCoverage: expected an object of node coverages`);
      else if (isObj(nc)) for (const [k, v] of Object.entries(nc)) p.num(v, `${sa}.nodeCoverage.${k}`, SHARE);
    });
  }
  if (p.arr(c.trade, `${at}.trade`)) {
    c.trade.forEach((e, i) => {
      const ea = `${at}.trade[${i}]`;
      if (!isObj(e)) return p.add(`${ea}: not an object`);
      for (const k of ['from', 'to', 'node']) p.str(e[k], `${ea}.${k}`);
      p.num(e.usd, `${ea}.usd`, { nullable: true });
      p.num(e.share, `${ea}.share`, { nullable: true });
      p.oneOf(e.relationship, AIB_RELATIONSHIPS, `${ea}.relationship`);
      p.num(e.opportunity, `${ea}.opportunity`, { nullable: true });
    });
  }
  if (p.arr(c.investment, `${at}.investment`)) {
    c.investment.forEach((e, i) => {
      const ea = `${at}.investment[${i}]`;
      if (!isObj(e)) return p.add(`${ea}: not an object`);
      for (const k of ['investor', 'host', 'node']) p.str(e[k], `${ea}.${k}`);
      p.num(e.usd, `${ea}.usd`, { nullable: true });
      p.num(e.share, `${ea}.share`, { nullable: true });
      p.num(e.friendshore, `${ea}.friendshore`, { nullable: true });
    });
  }
}

/** Problems with a compacts.<h8>.json document (empty when it is valid). */
export function checkCompactsDoc(x: unknown): string[] {
  if (!isObj(x)) return ['document: not an object'];
  const p = new Problems();
  if (x.schema !== AIB_SCHEMA) p.add(`document.schema: expected '${AIB_SCHEMA}'`);
  if (!isNum(x.version)) p.add('document.version: not a number');
  else if (Math.floor(x.version) > AIB_VERSION) p.add(`document: version ${x.version} needs a newer app`);
  checkMeta(p, x.meta);
  const classes = isObj(x.meta) && Array.isArray(x.meta.classes) ? (x.meta.classes as unknown[]).filter(isStr) : [...AIB_CLASSES];
  // countries: an object of names ([] when empty, as jsonlite writes an empty named list)
  if (Array.isArray(x.countries) ? x.countries.length > 0 : !isObj(x.countries)) p.add('countries: expected an object of ISO3 -> name');
  else if (isObj(x.countries)) for (const [k, v] of Object.entries(x.countries)) p.str(v, `countries.${k}`);
  const nodeIds = new Set<string>();
  if (p.arr(x.nodes, 'nodes')) {
    x.nodes.forEach((n, i) => {
      checkNode(p, n, `nodes[${i}]`, classes);
      if (isObj(n) && isStr(n.id)) {
        if (nodeIds.has(n.id)) p.add(`nodes[${i}].id: duplicate '${n.id}'`);
        nodeIds.add(n.id);
      }
    });
  }
  const compactIds = new Set<string>();
  if (p.arr(x.compacts, 'compacts')) {
    x.compacts.forEach((c, i) => {
      checkCompact(p, c, `compacts[${i}]`, nodeIds);
      if (isObj(c) && isStr(c.id)) {
        if (compactIds.has(c.id)) p.add(`compacts[${i}].id: duplicate '${c.id}'`);
        else if ((AIB_RESERVED as readonly string[]).includes(c.id.toLowerCase())) p.add(`compacts[${i}].id: '${c.id}' is a reserved path segment`);
        compactIds.add(c.id);
      }
    });
  }
  // a node's compact must be one of the document's compacts (when there is a list to check against)
  if (Array.isArray(x.nodes) && Array.isArray(x.compacts)) {
    x.nodes.forEach((n, i) => {
      if (isObj(n) && isStr(n.compact) && !compactIds.has(n.compact)) p.add(`nodes[${i}].compact: '${n.compact}' is not a compact of the document`);
    });
  }
  return p.list;
}

// ---- Accessors ---------------------------------------------------------------------------------------

/** A path step's node coverages as an object (the exporter writes [] for an empty one). */
export const nodeCoverageOf = (s: AibPathStep): Record<string, number> => (Array.isArray(s.nodeCoverage) ? {} : s.nodeCoverage);

/** The country names as an object (the exporter writes [] for an empty one). */
export const countryNamesOf = (doc: AibDoc): Record<string, string> => (Array.isArray(doc.countries) ? {} : doc.countries);
