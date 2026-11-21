// The shipped-bundle checks (docs/QC_SYSTEM_DESIGN.md 6.1): schema, completeness, allowed codes, numeric
// validity, posture invariants, the decision trace ending in the shipped choice, the choice-rule reproduction,
// knife-edges (prize at the 0.59 cap, values exactly on a threshold, whole nodes sharing one prize near the bar),
// provenance stamps and release completeness, the AIB coverage path and class rule, zero-gain rank-1 hosts, memo
// verification flags and the TypeScript fallback thresholds of posture/contract.ts against the core's. Each check
// returns one row {check_id, layer 'bundle', area, status, n, detail, examples}; a passing check is a statement
// about the bundle, not evidence that the analysis is right. Pure: no IO (the Node CLI reads the files and the
// contract source text for parseDefaultThresholds).

import { AIB_CLASSES, type AibDoc, type AibInvestmentHost, type AibNode } from '../../aib/contract.ts';
import { SUPPLY_STAGES, DEPLOYMENT_STAGE } from '../../lib/stages.ts';
import { ALIGNMENTS, ROLLUP_YARDS, YARD_CODES, yardOfVerb, type PostureCell, type PostureThresholds } from '../../posture/contract.ts';
import type { NodeDetail } from '../../posture/detail.ts';
import { releaseCompleteness } from '../release.ts';
import { cellKey, isObj, type DecodedBundles, type RawBundles } from './bundles.ts';
import { cappedNodes, detailChoice, shippedChoice, type ReproductionRow, type ReproductionSummary } from './choiceRule.ts';

export type QcStatus = 'pass' | 'warn' | 'fail' | 'info';
export interface QcRow {
  check_id: string;
  layer: 'bundle';
  area: string;
  status: QcStatus;
  /** the number of cells / nodes / files the row counts (the offending ones for warn / fail) */
  n: number;
  detail: string;
  examples: string[];
}

export interface BundleQcInput {
  decoded: DecodedBundles;
  /** the parsed JSON before decoding (raw column checks); optional */
  raw?: RawBundles | null;
  /** problems the loader met (missing files) */
  loadProblems?: readonly string[];
  /** the choice-rule reproduction (qc/choiceRule.ts reproduceShippedChoices) */
  reproduction?: { rows: readonly ReproductionRow[]; summary: ReproductionSummary } | null;
  /** DEFAULT_THRESHOLDS of posture/contract.ts (parseDefaultThresholds on its source) */
  fallbackThresholds?: Partial<PostureThresholds> | null;
  /** the data release ID (release.ts dataReleaseId) */
  releaseId?: string | null;
  /** how many examples a row lists (default 8) */
  maxExamples?: number;
}

const EPS = 1e-9;
/** Half a unit of the 3rd decimal (the core and the trace round to 3 dp, the detail keeps 3 significant digits), plus float slack. */
const ROUND_TOL = 5e-4 + 1e-9;
const num = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
const r4 = (x: number): string => String(Math.round(x * 1e4) / 1e4);

/** DEFAULT_THRESHOLDS from the text of posture/contract.ts (it is module-private there); null when not found. */
export function parseDefaultThresholds(source: string): Partial<PostureThresholds> | null {
  const m = /DEFAULT_THRESHOLDS\s*:\s*PostureThresholds\s*=\s*\{([^}]*)\}/.exec(source);
  if (!m) return null;
  const out: Partial<PostureThresholds> = {};
  for (const part of m[1].split(',')) {
    const kv = /^\s*(\w+)\s*:\s*([0-9.eE+-]+)\s*$/.exec(part);
    if (kv && ['need', 'prize', 'cp', 'deployment', 'borderline_band'].includes(kv[1])) out[kv[1] as keyof PostureThresholds] = Number(kv[2]);
  }
  return Object.keys(out).length ? out : null;
}

class Rows {
  readonly rows: QcRow[] = [];
  private readonly max: number;
  // no parameter property: the CLIs load this file with node --experimental-strip-types
  constructor(max: number) {
    this.max = max;
  }
  add(check_id: string, area: string, status: QcStatus, n: number, detail: string, examples: readonly string[] = []): void {
    this.rows.push({ check_id, layer: 'bundle', area, status, n, detail, examples: examples.slice(0, this.max) });
  }
  /** pass when no offender, else `bad` */
  check(check_id: string, area: string, offenders: readonly string[], okText: string, badText: (n: number) => string, bad: QcStatus = 'fail', total?: number): void {
    if (!offenders.length) this.add(check_id, area, 'pass', total ?? 0, okText);
    else this.add(check_id, area, bad, offenders.length, badText(offenders.length), offenders);
  }
}

const COMPETE = new Set(['preserve', 'catch_up', 'partner']);
const DEPLOY = new Set(['accelerate', 'watch']);
const RATIONALES = new Set(['operational_control', 'coercive_chokepoint', 'systemic_resilience']);
const PATHWAYS = new Set(['yes', 'no', 'unknown']);
const CONF = new Set(['high', 'moderate', 'low']);

// ---- posture -------------------------------------------------------------------------------------------------------

function postureRawChecks(raw: RawBundles | null | undefined, R: Rows): void {
  const core = raw?.posture?.core;
  if (!isObj(core) || !isObj(core.cells)) return;
  const cols = core.cells as Record<string, unknown>;
  const n = Array.isArray(cols.c) ? cols.c.length : 0;
  const bad: string[] = [];
  for (const k of ['c', 't', 's', 'p', 'm', 'dm', 'need', 'prize', 'cp']) if (!Array.isArray(cols[k])) bad.push(`required column ${k} missing`);
  for (const [k, v] of Object.entries(cols)) if (Array.isArray(v) && v.length !== n) bad.push(`column ${k} has ${v.length} values, not ${n}`);
  R.check('BQ-POST-SCHEMA-02', 'posture.schema', bad, `posture core: ${Object.keys(cols).length} columns, ${n} rows, every column full length`, (k) => `${k} posture core column problems`);
  // codes
  const codes: string[] = [];
  const allowed: Record<string, (v: unknown) => boolean> = {
    p: (v) => v === 'S' || v === 'C' || v === 'D' || v === 'N',
    m: (v) => v === null || (typeof v === 'string' && COMPETE.has(v)),
    dm: (v) => v === null || (typeof v === 'string' && DEPLOY.has(v)),
    sec: (v) => v === null || (typeof v === 'string' && RATIONALES.has(v)),
    nt: (v) => v === 'supply_chain' || v === 'deployment',
    pw: (v) => v === null || (typeof v === 'string' && PATHWAYS.has(v)),
    conf: (v) => typeof v === 'string' && CONF.has(v),
    al: (v) => v === null || (typeof v === 'string' && (ALIGNMENTS as readonly string[]).includes(v)),
    yd: (v) => v === null || (typeof v === 'string' && Object.values(YARD_CODES).includes(v)),
  };
  for (const [k, ok] of Object.entries(allowed)) {
    const col = cols[k];
    if (!Array.isArray(col)) continue;
    const odd = new Map<string, number>();
    col.forEach((v) => {
      if (!ok(v)) odd.set(JSON.stringify(v), (odd.get(JSON.stringify(v)) ?? 0) + 1);
    });
    for (const [v, c] of odd) codes.push(`${k}=${v} (${c} cells)`);
  }
  R.check('BQ-POST-CODES-01', 'posture.codes', codes, 'posture codes (p, m, dm, sec, nt, pw, conf, al, yd) are all in their vocabularies', (k) => `${k} out-of-vocabulary code values in the posture core`);
  // numeric columns: a number in [0,1] or null; never a string (NaN, Inf, "NA" ...)
  const numBad: string[] = [];
  for (const k of ['need', 'prize', 'cp', 'share', 'io']) {
    const col = cols[k];
    if (!Array.isArray(col)) continue;
    col.forEach((v, i) => {
      if (v === null) return;
      if (typeof v !== 'number' || !Number.isFinite(v)) numBad.push(`${k}[${i}] = ${JSON.stringify(v)} (malformed)`);
      else if (v < -EPS || v > 1 + EPS) numBad.push(`${k}[${i}] = ${v} (outside [0,1])`);
    });
  }
  R.check('BQ-POST-NUM-01', 'posture.numeric', numBad, 'need, prize, cp, share and io are numbers in [0,1] or null in every row', (k) => `${k} malformed or out-of-range numeric values in the posture core`);
}

const traceNum = (v: unknown): number | null =>
  typeof v === 'number' && Number.isFinite(v) ? v : typeof v === 'string' && v !== '' && Number.isFinite(Number(v)) ? Number(v) : null;

/**
 * The trace's result words against the shipped numbers (R sp_choice_trace): lvl() for need / prize / deployment
 * need; the position word (high, not_top_quarter, capability_floor_not_met, not_a_contender, unknown) against cp
 * and the bar, and, for the contender words, the measured share against the contender line (0.5%,
 * decision_framework.contenders.min_share: config only); the large-yard word against its signal count and the
 * node record; the pathway word against the detail's pathway. One string per contradiction.
 */
export function traceWordIssues(
  cells: readonly PostureCell[],
  index: ReadonlyMap<string, NodeDetail>,
  th: PostureThresholds,
  yfNodes: Readonly<Record<string, { largeYard: boolean | null; n: number | null }>>,
  contenderMinShare = 0.005,
): string[] {
  const out: string[] = [];
  for (const c of cells) {
    const k = cellKey(c.iso3, c.tech, c.stage);
    const d = index.get(k);
    if (!d) continue;
    for (const t of d.trace) {
      const lv = (v: number | null, bar: number) => (v === null ? 'not_assessed' : v >= bar - ROUND_TOL && v < bar + ROUND_TOL ? null : v >= bar ? 'high' : 'low');
      if (t.test === 'supply_chain_need' || t.test === 'strategic_industrial_value' || t.test === 'industrial_opportunity' || t.test === 'deployment_need') {
        const v = t.test === 'supply_chain_need' || t.test === 'deployment_need' ? d.need : d.prize.value;
        const bar = t.test === 'supply_chain_need' ? th.need : t.test === 'deployment_need' ? th.deployment : th.prize;
        const want = lv(v, bar);
        // a value within rounding of the bar can read either way (the trace word, computed unrounded, decides)
        if (want !== null && t.result !== want) out.push(`${k} ${t.test}: value ${v} against ${bar} reads ${want}, trace says ${t.result}`);
      } else if (t.test === 'cp_contender_pct') {
        const v = d.cp;
        const r = t.result;
        const near = v !== null && Math.abs(v - th.cp) < ROUND_TOL;
        if (r === 'unknown' && v !== null) out.push(`${k} position: trace says unknown but cp ${v} is shipped`);
        if (r !== 'unknown' && v === null) out.push(`${k} position: trace says ${r} but no cp is shipped`);
        if (v !== null && !near) {
          if ((r === 'high' || r === 'capability_floor_not_met' || r === 'not_a_contender') && v < th.cp) out.push(`${k} position: ${r} with cp ${v} below the ${th.cp} bar`);
          if (r === 'not_top_quarter' && v >= th.cp) out.push(`${k} position: not_top_quarter with cp ${v} at or above the ${th.cp} bar`);
        }
        const sh = d.homeShare;
        if (sh !== null && (r === 'high' || r === 'capability_floor_not_met') && sh < contenderMinShare - ROUND_TOL / 10) out.push(`${k} position: ${r} (a contender) with a measured share ${sh} below the ${contenderMinShare} contender line`);
        if (sh !== null && r === 'not_a_contender' && sh >= contenderMinShare + ROUND_TOL / 10) out.push(`${k} position: not_a_contender with a measured share ${sh} at or above the ${contenderMinShare} contender line`);
      } else if (t.test === 'large_yard_case') {
        const n = traceNum(t.value);
        const min = traceNum(t.threshold);
        if (t.result === 'yes' && n !== null && min !== null && n < min) out.push(`${k} large yard: yes on ${n} signals, fewer than ${min}`);
        const yf = yfNodes[`${c.tech}|${c.stage}|stage`];
        const own = d.largeYard ? d.largeYard.case : yf ? yf.largeYard : undefined;
        const word = t.result === 'yes' ? true : t.result === 'no' ? false : t.result === 'unknown' ? null : undefined;
        if (own !== undefined && word !== undefined && own !== word) out.push(`${k} large yard: trace says ${t.result}, the ${d.largeYard ? "economy's own record" : 'node record (meta.yf_nodes)'} says ${String(own)}`);
        const ownN = d.largeYard ? d.largeYard.n : yf ? yf.n : null;
        if (ownN !== null && n !== null && ownN !== n) out.push(`${k} large yard: trace counts ${n} signals, the record ${ownN}`);
      } else if (t.test === 'pathway_credible') {
        if (t.result !== (d.pathway.credible ?? 'unknown')) out.push(`${k} pathway: trace says ${t.result}, the detail ${String(d.pathway.credible)}`);
      }
    }
  }
  return out;
}

/**
 * The borderline tags R writes for need, prize and deployment need (decision_engine.R nb_need, nb_prize, nb_dn:
 * a triage-read cell whose value is within the band of the bar; a capped prize is never borderline), recomputed
 * from the shipped values. A value within rounding of the band edge can go either way and is not counted.
 * Returns the contradictions and how many tags were checked.
 */
export function borderlineIssues(cells: readonly PostureCell[], index: ReadonlyMap<string, NodeDetail>, th: PostureThresholds, capped: ReadonlyMap<string, boolean>): { issues: string[]; checked: number } {
  const issues: string[] = [];
  let checked = 0;
  const band = th.borderline_band;
  const want = (v: number | null, bar: number): boolean | null => {
    if (v === null) return false;
    const d = Math.abs(v - bar);
    return d < band - ROUND_TOL ? true : d > band + ROUND_TOL ? false : null;
  };
  for (const c of cells) {
    const k = cellKey(c.iso3, c.tech, c.stage);
    const d = index.get(k);
    if (!d) continue;
    const tests = new Set(d.trace.map((t) => t.test));
    const dep = c.stage === DEPLOYMENT_STAGE;
    const tri = !dep && tests.has('supply_chain_need') && d.need !== null && d.prize.value !== null;
    const pairs: [string, boolean | null][] = [];
    if (tri) {
      pairs.push(['supply_chain_need', want(d.need, th.need)]);
      pairs.push(['strategic_industrial_value', capped.get(`${c.tech}|${c.stage}`) === true ? false : want(d.prize.value, th.prize)]);
    }
    if (dep && tests.has('deployment_need')) pairs.push(['deployment_need', want(d.need, th.deployment)]);
    for (const [tag, w] of pairs) {
      if (w === null) continue;
      checked++;
      const has = d.borderline.includes(tag);
      if (has !== w) issues.push(`${k} ${tag}: value ${tag === 'strategic_industrial_value' ? d.prize.value : d.need} ${w ? 'is' : 'is not'} within ${band} of the bar but the cell ${has ? 'carries' : 'lacks'} the borderline tag`);
    }
  }
  return { issues, checked };
}

function postureChecks(input: BundleQcInput, R: Rows): void {
  const p = input.decoded.posture;
  if (!p) {
    R.add('BQ-POST-SCHEMA-01', 'posture.schema', 'fail', 0, 'the posture bundle did not decode or is missing');
    return;
  }
  const man = p.manifest ?? {};
  const meta = p.core.meta;
  const cells = p.core.cells;
  R.check('BQ-POST-SCHEMA-01', 'posture.schema', [
    ...(man.schema !== 'osi-posture' ? [`manifest schema ${String(man.schema)}`] : []),
    ...(man.schema_version !== 2 ? [`manifest schema_version ${String(man.schema_version)}`] : []),
  ], 'posture manifest osi-posture v2; core and details decode with the app\'s own decoders (decodeCore, checkPostureDetail)', (k) => `${k} posture schema problems`);
  postureRawChecks(input.raw, R);

  // completeness
  const nCountries = p.core.countries.length;
  const perCountry = new Map<string, number>();
  const keys = new Set<string>();
  const dup: string[] = [];
  const vocab: string[] = [];
  for (const c of cells) {
    perCountry.set(c.iso3, (perCountry.get(c.iso3) ?? 0) + 1);
    const k = cellKey(c.iso3, c.tech, c.stage);
    if (keys.has(k)) dup.push(k);
    keys.add(k);
    if (!/^[A-Z]{3}$/.test(c.iso3)) vocab.push(`${k}: iso3 not 3 capitals`);
    if (!meta.techs.includes(c.tech)) vocab.push(`${k}: tech not in meta.techs`);
    if (!(SUPPLY_STAGES as readonly string[]).includes(c.stage)) vocab.push(`${k}: stage not one of the six`);
  }
  const nodes = meta.techs.length * SUPPLY_STAGES.length;
  const short = [...perCountry].filter(([, n]) => n !== nodes).map(([iso, n]) => `${iso}: ${n} cells`);
  const counts = isObj(man.counts) ? man.counts : {};
  const compl: string[] = [...short, ...dup.map((d) => `duplicate ${d}`), ...vocab];
  if (num(counts.countries) !== null && counts.countries !== nCountries) compl.push(`manifest counts.countries ${String(counts.countries)} vs core ${nCountries}`);
  if (num(counts.cells) !== null && counts.cells !== cells.length) compl.push(`manifest counts.cells ${String(counts.cells)} vs core ${cells.length}`);
  if (cells.length !== nCountries * nodes) compl.push(`${cells.length} cells, not ${nCountries} x ${nodes}`);
  R.check('BQ-POST-COMPLETE-01', 'posture.completeness', compl, `${nCountries} economies x ${meta.techs.length} technologies x ${SUPPLY_STAGES.length} stages = ${cells.length} cells, unique (country, tech, stage), ISO3 and vocabularies valid`, (k) => `${k} completeness problems in the posture core`, 'fail', cells.length);

  // details
  const detIssues: string[] = [];
  let detCells = 0;
  const index = new Map<string, NodeDetail>();
  for (const { iso3 } of p.core.countries) {
    const d = p.details[iso3];
    if (!d) {
      detIssues.push(`${iso3}: no detail document`);
      continue;
    }
    if (d.cells.length !== nodes) detIssues.push(`${iso3}: ${d.cells.length} detail cells`);
    for (const c of d.cells) {
      index.set(cellKey(iso3, c.tech, c.stage), c);
      detCells++;
    }
  }
  for (const c of cells) if (!index.has(cellKey(c.iso3, c.tech, c.stage))) detIssues.push(`${cellKey(c.iso3, c.tech, c.stage)}: no detail cell`);
  R.check('BQ-POST-COMPLETE-02', 'posture.completeness', detIssues, `${Object.keys(p.details).length} detail documents, ${detCells} detail cells, one per core cell`, (k) => `${k} detail completeness problems`, 'fail', detCells);

  // choice counts against the manifest
  const ch: Record<string, number> = {};
  for (const c of cells) {
    const v = shippedChoice(c) ?? 'none';
    ch[v] = (ch[v] ?? 0) + 1;
  }
  const mc = isObj(counts.choices) ? (counts.choices as Record<string, unknown>) : null;
  const cDiff = mc ? Object.keys({ ...mc, ...ch }).filter((k) => (num(mc[k]) ?? 0) !== (ch[k] ?? 0)).map((k) => `${k}: manifest ${String(mc[k] ?? 0)}, core ${ch[k] ?? 0}`) : ['manifest has no counts.choices'];
  R.check('BQ-POST-COUNTS-01', 'posture.completeness', cDiff, `manifest counts.choices equal the core: ${Object.entries(ch).map(([k, v]) => `${k} ${v}`).join(', ')}`, (k) => `${k} choice counts differ between the manifest and the core`, mc ? 'fail' : 'warn', cells.length);

  // invariants
  const inv: string[] = [];
  for (const c of cells) {
    const k = cellKey(c.iso3, c.tech, c.stage);
    const v = shippedChoice(c);
    const dep = c.stage === DEPLOYMENT_STAGE;
    if (c.posture === 'C' && !c.mode) inv.push(`${k}: Compete without a mode`);
    if (c.posture !== 'C' && c.mode) inv.push(`${k}: mode ${c.mode} outside Compete`);
    if (c.posture === 'D' && !c.deployMode) inv.push(`${k}: Deploy without a mode`);
    if (c.posture !== 'D' && c.deployMode) inv.push(`${k}: deploy mode outside Deploy`);
    if (c.posture === 'S' && !c.rationale) inv.push(`${k}: Secure without a rationale`);
    if (c.posture !== 'S' && c.rationale) inv.push(`${k}: rationale outside Secure`);
    if (dep && c.posture === 'C') inv.push(`${k}: Compete at Deployment & Use`);
    if (!dep && c.deployMode === 'accelerate') inv.push(`${k}: Accelerate outside Deployment & Use`);
    if (c.posture === 'N' && c.need !== null) inv.push(`${k}: not assessed with a need value`);
    if (c.posture !== 'N' && c.posture !== 'S' && c.need === null) inv.push(`${k}: assessed without a need value`);
    if (c.posture !== 'N' && c.posture !== 'S' && !dep && c.prize === null) inv.push(`${k}: assessed producing cell without a prize`);
    if (v && v !== 'not_assessed') {
      const y = yardOfVerb(v);
      if (c.yards === null) inv.push(`${k}: assessed without a yard code`);
      else if (y && !ROLLUP_YARDS[c.yards].includes(y)) inv.push(`${k}: yard ${c.yards} but the choice ${v} is ${y}`);
    } else if (c.yards !== null) inv.push(`${k}: not assessed with yard ${c.yards}`);
    const d = index.get(k);
    if (d && detailChoice(d) !== v) inv.push(`${k}: detail choice ${detailChoice(d)} vs core ${v}`);
  }
  R.check('BQ-POST-INV-01', 'posture.invariants', inv, 'Compete has a mode, Deploy a deploy mode, Secure a rationale; no Compete at Deployment, Accelerate only there; not assessed has no need; yards match the choice; core and detail choices agree', (k) => `${k} posture invariant violations`, 'fail', cells.length);

  // core vs detail values
  const agree: string[] = [];
  const close = (a: number | null, b: number | null) => (a === null && b === null) || (a !== null && b !== null && Math.abs(a - b) <= ROUND_TOL);
  for (const c of cells) {
    const d = index.get(cellKey(c.iso3, c.tech, c.stage));
    if (!d) continue;
    const k = cellKey(c.iso3, c.tech, c.stage);
    if (!close(c.need, d.need)) agree.push(`${k}: need core ${c.need} detail ${d.need}`);
    if (!close(c.cp, d.cp)) agree.push(`${k}: cp core ${c.cp} detail ${d.cp}`);
    if (c.stage !== DEPLOYMENT_STAGE && !close(c.prize, d.prize.value)) agree.push(`${k}: prize core ${c.prize} detail ${d.prize.value}`);
    if (!close(c.share, d.homeShare)) agree.push(`${k}: share core ${c.share} detail ${d.homeShare}`);
  }
  R.check('BQ-POST-NUM-03', 'posture.numeric', agree, 'core and detail agree on need, prize, cp and share (within half a unit of the 3rd decimal, the export rounding)', (k) => `${k} core / detail value disagreements`, 'fail', cells.length);

  // detail numeric validity
  const dnum: string[] = [];
  const unit = (k: string, name: string, v: number | null | undefined) => {
    if (v === null || v === undefined) return;
    if (!Number.isFinite(v)) dnum.push(`${k} ${name} = ${v}`);
    else if (v < -EPS || v > 1 + EPS) dnum.push(`${k} ${name} = ${v} (outside [0,1])`);
  };
  const nonneg = (k: string, name: string, v: number | null | undefined) => {
    if (v === null || v === undefined) return;
    if (!Number.isFinite(v) || v < -EPS) dnum.push(`${k} ${name} = ${v} (negative or malformed)`);
  };
  for (const [k, d] of index) {
    const s = d.supplyChain;
    unit(k, 'need', d.need);
    unit(k, 'cp', d.cp);
    unit(k, 'home_share', d.homeShare);
    unit(k, 'prize', d.prize.value);
    unit(k, 'prize.market', d.prize.market);
    if (s) {
      unit(k, 'import_dependence', s.importDependence);
      unit(k, 'import_hhi', s.supplierConcentration);
      unit(k, 'china_bloc', s.riskyExposure);
      unit(k, 'trusted_alternatives', s.trustedAlternatives);
      unit(k, 'top_supplier_share', s.topSupplierShare);
      unit(k, 'china_share', s.chinaShare);
      unit(k, 'opposite_bloc_share', s.oppositeBlocShare);
      unit(k, 'trusted_share', s.trustedShare);
      unit(k, 'coercive_top_share', s.coerciveTopShare);
      nonneg(k, 'imports_to_gdp', s.importsToGdp);
    }
    for (const f of d.cpFamilies) unit(k, `cp_families.${f.id}`, f.value);
    unit(k, 'trade.benchmark_share', num(d.trade?.benchmark_share));
    nonneg(k, 'trade.benchmark_rca', num(d.trade?.benchmark_rca));
    nonneg(k, 'trade.benchmark_exports', num(d.trade?.benchmark_exports));
  }
  R.check('BQ-POST-NUM-02', 'posture.numeric', dnum, 'detail families, shares and trade-benchmark values are in range (0-1 scores and shares, non-negative RCA, exports, imports/GDP)', (k) => `${k} out-of-range or malformed detail values`, 'fail', index.size);

  // trace
  const tr: string[] = [];
  const trv: string[] = [];
  const th = meta.thresholds;
  for (const c of cells) {
    const k = cellKey(c.iso3, c.tech, c.stage);
    const d = index.get(k);
    if (!d) continue;
    const last = d.trace[d.trace.length - 1];
    const v = shippedChoice(c);
    if (!last || last.test !== 'choice') tr.push(`${k}: trace does not end in a choice step`);
    else if (last.result !== v) tr.push(`${k}: trace ends in ${last.result}, core ships ${v}`);
    for (const t of d.trace) {
      const val = typeof t.value === 'number' ? t.value : typeof t.value === 'string' && t.value !== '' && Number.isFinite(Number(t.value)) ? Number(t.value) : null;
      const thr = typeof t.threshold === 'number' ? t.threshold : typeof t.threshold === 'string' && Number.isFinite(Number(t.threshold)) ? Number(t.threshold) : null;
      const want: Record<string, [number | null, number]> = {
        supply_chain_need: [d.need, th.need],
        strategic_industrial_value: [d.prize.value, th.prize],
        cp_contender_pct: [d.cp, th.cp],
        deployment_need: [d.need, th.deployment],
      };
      const w = want[t.test];
      if (!w) continue;
      if (thr !== null && Math.abs(thr - w[1]) > EPS) trv.push(`${k} ${t.test}: trace threshold ${thr} vs core meta ${w[1]}`);
      if (val !== null && w[0] !== null && Math.abs(val - w[0]) > ROUND_TOL) trv.push(`${k} ${t.test}: trace value ${val} vs detail ${w[0]}`);
    }
  }
  R.check('BQ-POST-TRACE-01', 'posture.trace', tr, 'every cell\'s decision trace ends in its shipped choice', (k) => `${k} cells whose trace does not end in the shipped choice`, 'fail', cells.length);
  R.check('BQ-POST-TRACE-02', 'posture.trace', trv, 'trace values and thresholds equal the detail values and the core thresholds', (k) => `${k} trace values or thresholds disagree with the detail or the core meta`, 'fail', cells.length);
  R.check('BQ-POST-TRACE-03', 'posture.trace', traceWordIssues(cells, index, th, meta.yf_nodes), 'every trace result word agrees with the shipped values: need / prize / deployment high iff at or above the bar; the position word with cp against the bar and the measured share against the 0.5% contender line; the large-yard word with its signal count and the node record (meta.yf_nodes, or the economy\'s own exception); the pathway word with the detail', (k) => `${k} trace words contradict the shipped values (the trace says one thing, the numbers another)`, 'fail', cells.length);

  // reproduction
  const rep = input.reproduction;
  if (rep) {
    const bad = rep.rows.filter((r) => !r.reproduced);
    const s = rep.summary;
    const paths = Object.entries(s.by_path).map(([k, v]) => `${k} ${v.reproduced}/${v.cells}`).join(', ');
    const scope = 'Secure (the engine override, 6 tests not shipped) and the large-yard case itself (its signals) are read from the trace, not recomputed; cp_high reads the trace\'s position word because the capability lower bound (t3_capability_lo) is not shipped. The test therefore verifies the rule\'s logic and the need / prize / deployment-need comparisons, not those three inputs.';
    // rounding and unresolved cases are a limit of the bundle's precision; any other mismatch is an error
    const hard = bad.filter((r) => r.category !== 'rounding_on_threshold' && r.category !== 'unresolved');
    if (!bad.length)
      R.add('BQ-POST-REPRO-01', 'posture.reproduction', 'pass', s.cells, `the TypeScript port of sp_choice_rule reproduces ${s.reproduced} of ${s.cells} shipped choices (${(s.rate * 100).toFixed(2)}%) from the shipped values; by path ${paths}. ${scope}`);
    else
      R.add('BQ-POST-REPRO-01', 'posture.reproduction', hard.length ? 'fail' : 'warn', bad.length, `${bad.length} of ${s.cells} shipped choices are not reproduced by the rule port (rate ${(s.rate * 100).toFixed(2)}%); categories ${Object.entries(s.by_category).map(([k, v]) => `${k} ${v}`).join(', ')}; by path ${paths}. ${scope}`, [...hard, ...bad.filter((r) => !hard.includes(r))].map((r) => `${r.iso3}|${r.tech}|${r.stage}: ${r.reason}`));
  } else R.add('BQ-POST-REPRO-01', 'posture.reproduction', 'warn', 0, 'the choice-rule reproduction was not run');

  // knife-edges
  const caps = cappedNodes(p.core);
  const bl = borderlineIssues(cells, index, th, caps);
  R.check('BQ-POST-BORDER-01', 'posture.trace', bl.issues, `the borderline tags of need, prize and deployment need (which lower the published confidence) reproduce from the shipped values on all ${bl.checked} checkable tags (values within rounding of the band edge skipped)`, (k) => `${k} borderline tags contradict the shipped values (confidence is computed from them)`, 'fail', bl.checked);
  const at59 = cells.filter((c) => c.prize !== null && Math.abs(c.prize - 0.59) < EPS);
  const capped = cells.filter((c) => caps.get(`${c.tech}|${c.stage}`) === true);
  R.add('BQ-POST-CAP-01', 'posture.knife_edge', 'info', at59.length, `${at59.length} cells carry a prize of exactly 0.59 (the market-only cap = prize threshold - cap_margin; by rule never high and never borderline, so margin views show a spurious -0.01 cluster); ${capped.length} cells sit in nodes capped market_only (meta.yf_nodes)`, at59.slice(0, 8).map((c) => cellKey(c.iso3, c.tech, c.stage)));
  const overCap = capped.filter((c) => c.prize !== null && c.prize > 0.59 + EPS).map((c) => `${cellKey(c.iso3, c.tech, c.stage)} prize ${c.prize}`);
  R.check('BQ-POST-CAP-02', 'posture.knife_edge', overCap, 'no cell of a market-only capped node carries a prize above 0.59', (k) => `${k} cells of market-only capped nodes carry a prize above the 0.59 cap`, 'fail', capped.length);
  // exactly on a threshold (where the rule reads the value)
  const on: string[] = [];
  const onCount: Record<string, number> = { need: 0, prize: 0, cp: 0, deployment: 0, large_yard: 0 };
  for (const c of cells) {
    const k = cellKey(c.iso3, c.tech, c.stage);
    const d = index.get(k);
    if (!d) continue;
    for (const t of d.trace) {
      const v = typeof t.value === 'number' ? t.value : typeof t.value === 'string' && t.value !== '' && Number.isFinite(Number(t.value)) ? Number(t.value) : null;
      const thr = typeof t.threshold === 'number' ? t.threshold : typeof t.threshold === 'string' && Number.isFinite(Number(t.threshold)) ? Number(t.threshold) : null;
      if (v === null || thr === null || Math.abs(v - thr) > EPS) continue;
      const id = t.test === 'supply_chain_need' ? 'need' : t.test === 'strategic_industrial_value' ? 'prize' : t.test === 'cp_contender_pct' ? 'cp' : t.test === 'deployment_need' ? 'deployment' : t.test === 'large_yard_case' ? 'large_yard' : null;
      if (!id) continue;
      // a large-yard case that fails on exactly min_signals fails on a required signal or a veto, not on the count
      if (id === 'large_yard' && t.result !== 'yes') continue;
      onCount[id]++;
      on.push(`${k} ${t.test} = ${t.threshold} -> ${t.result} (choice ${shippedChoice(c)})`);
    }
  }
  R.add('BQ-POST-EDGE-01', 'posture.knife_edge', on.length ? 'warn' : 'pass', on.length, `${on.length} trace steps sit exactly on their threshold at the shipped 3-decimal precision (the inclusive >= rule decides them): ${Object.entries(onCount).map(([k, v]) => `${k} ${v}`).join(', ')}`, [...on.filter((x) => !x.includes('large_yard_case')), ...on.filter((x) => x.includes('large_yard_case'))]);
  // whole nodes sharing one prize within the band of the bar
  const byNode = new Map<string, number[]>();
  for (const c of cells) if (c.stage !== DEPLOYMENT_STAGE && c.prize !== null && c.posture !== 'N') (byNode.get(`${c.tech}|${c.stage}`) ?? byNode.set(`${c.tech}|${c.stage}`, []).get(`${c.tech}|${c.stage}`)!).push(c.prize);
  const flat: string[] = [];
  for (const [node, ps] of byNode) {
    const lo = Math.min(...ps);
    const hi = Math.max(...ps);
    if (ps.length >= 20 && hi - lo < 1e-6 && Math.abs(lo - th.prize) < th.borderline_band && caps.get(node) !== true) flat.push(`${node}: prize ${r4(lo)} for all ${ps.length} assessed economies (bar ${th.prize})`);
  }
  R.add('BQ-POST-EDGE-02', 'posture.knife_edge', flat.length ? 'warn' : 'pass', flat.length, flat.length ? `${flat.length} nodes carry one prize for every assessed economy within the borderline band of the prize bar: the whole node flips together on a small threshold move` : 'no node carries one uniform prize within the band of the bar', flat);

  // TS fallback thresholds
  const fb = input.fallbackThresholds;
  if (fb) {
    const diff = (Object.keys(fb) as (keyof PostureThresholds)[]).filter((k) => fb[k] !== undefined && Math.abs((fb[k] as number) - th[k]) > EPS).map((k) => `${k}: DEFAULT_THRESHOLDS ${fb[k]} vs core meta ${th[k]}`);
    R.check('BQ-POST-FALLBACK-01', 'posture.thresholds', diff, `the TypeScript fallback thresholds of posture/contract.ts equal the core's meta.thresholds`, (k) => `${k} fallback thresholds in posture/contract.ts DEFAULT_THRESHOLDS differ from the shipped meta.thresholds (latent: decodeCore records a missing threshold as a contract problem and throws, so today the fallback never reaches a page; it would if that rule were relaxed)`, 'warn');
  } else R.add('BQ-POST-FALLBACK-01', 'posture.thresholds', 'info', 0, 'DEFAULT_THRESHOLDS of posture/contract.ts not supplied (not compared)');
}

// ---- AIB -------------------------------------------------------------------------------------------------------------

/** The node classification of config/aib.yml (aib_compacts.R) re-run on the exported fields; null when undecidable. */
export function aibClassFromExport(n: AibNode, t: Record<string, number | undefined>): { cls: string | null; why: string } {
  const exp = num(n.exposure);
  const res = num(n.residual);
  const ex = num(n.existing);
  const div = num(n.dividend);
  const opt = num(n.optimized);
  const ls = num(n.largestShare);
  const at = (k: string, d: number) => t[k] ?? d;
  if (exp === null) return { cls: null, why: 'exposure not exported' };
  if (exp < at('low_exposure', 0.15)) return { cls: 'LOW INTERVENTION', why: `exposure ${exp} < low_exposure` };
  if (res === null) return { cls: null, why: 'residual not exported' };
  if (res >= at('build_gap', 0.1)) return { cls: 'BUILD', why: `residual ${res} >= build_gap` };
  const capPt = (n.frontier ?? []).find((f) => Math.abs(f.cap - at('manage_cap', 0.4)) < 1e-9);
  const loss = opt !== null && capPt && num(capPt.coverage) !== null ? opt - (capPt.coverage as number) : null;
  const conc = (ls !== null && ls >= at('manage_largest_share', 0.4)) || (loss !== null && loss >= at('manage_coverage_loss', 0.05));
  if (conc) return { cls: 'MANAGE SPECIALIZATION', why: `largest share ${ls} / coverage loss at the cap ${loss === null ? 'n/a' : r4(loss)}` };
  // the optimized top-3 share is not exported: the concentration test cannot be ruled out on it
  const rest = ex !== null && ex >= at('adequate_coverage', 0.9) ? 'LOW INTERVENTION' : div !== null && div >= at('coordinate_dividend', 0.05) ? 'COORDINATE' : 'LOW INTERVENTION';
  return { cls: rest, why: `not concentrated on the exported fields (top-3 share after the shock not exported): ${rest}` };
}

function aibChecks(input: BundleQcInput, R: Rows): void {
  const a = input.decoded.aib;
  if (!a) {
    R.add('BQ-AIB-SCHEMA-01', 'aib.schema', 'fail', 0, 'the AIB bundle did not decode or is missing');
    return;
  }
  const doc: AibDoc = a.doc;
  R.check('BQ-AIB-SCHEMA-01', 'aib.schema', a.problems.slice(0, 50), `AIB document ${doc.schema} v${doc.version}: ${doc.nodes.length} nodes, ${doc.compacts.length} operational compacts, ${doc.strategic?.compacts.length ?? 0} strategic compacts; checkCompactsDoc reports no problem`, (k) => `${k} problems from the app's checkCompactsDoc`);
  const allowed = new Set<string>(AIB_CLASSES);
  const badCls: string[] = [];
  for (const n of doc.nodes) {
    if (!allowed.has(n.cls)) badCls.push(`${n.id}: cls ${n.cls}`);
    for (const [k, v] of Object.entries(n.variants ?? {})) if (v !== null && !allowed.has(v)) badCls.push(`${n.id}: variant ${k} ${v}`);
    for (const g of n.growth ?? []) if (g.cls !== null && !allowed.has(g.cls)) badCls.push(`${n.id}: growth ${g.label} ${g.cls}`);
    if (n.diversify?.cls && !allowed.has(n.diversify.cls)) badCls.push(`${n.id}: diversify ${n.diversify.cls}`);
  }
  const clsCount: Record<string, number> = {};
  for (const n of doc.nodes) clsCount[n.cls] = (clsCount[n.cls] ?? 0) + 1;
  R.check('BQ-AIB-CLASS-01', 'aib.codes', badCls, `node classes, variants and growth classes are AIB classes: ${Object.entries(clsCount).map(([k, v]) => `${k} ${v}`).join(', ')}`, (k) => `${k} class values outside AIB_CLASSES`, 'fail', doc.nodes.length);

  // coverage path and residual
  const path: string[] = [];
  const resid: string[] = [];
  const rng: string[] = [];
  const unit = (id: string, name: string, v: number | null | undefined, open = false) => {
    if (v === null || v === undefined) return;
    if (!Number.isFinite(v) || v < (open ? EPS : -EPS) || v > 1 + EPS) rng.push(`${id} ${name} = ${v}`);
  };
  for (const n of doc.nodes) {
    const s = num(n.static), e = num(n.existing), o = num(n.optimized), r = num(n.residual);
    if (s !== null && e !== null && s > e + EPS) path.push(`${n.id}: static ${s} > existing ${e}`);
    if (e !== null && o !== null && e > o + EPS) path.push(`${n.id}: existing ${e} > optimized ${o}`);
    if (o !== null && o > 1 + EPS) path.push(`${n.id}: optimized ${o} > 1`);
    if (o !== null && r !== null && Math.abs(r - Math.max(1 - o, 0)) > 1e-6) resid.push(`${n.id}: residual ${r} vs 1 - optimized ${r4(1 - o)}`);
    const dv = n.diversify;
    if (dv) {
      const ds = num(dv.static), de = num(dv.existing), dop = num(dv.optimized), dr = num(dv.residual);
      if (ds !== null && de !== null && ds > de + EPS) path.push(`${n.id} (managed): static ${ds} > existing ${de}`);
      if (de !== null && dop !== null && de > dop + EPS) path.push(`${n.id} (managed): existing ${de} > optimized ${dop}`);
      if (dop !== null && dr !== null && Math.abs(dr - Math.max(1 - dop, 0)) > 1e-6) resid.push(`${n.id} (managed): residual ${dr} vs 1 - optimized ${r4(1 - dop)}`);
      unit(n.id, 'diversify.hhi', num(dv.hhi), true);
      unit(n.id, 'diversify.largestShare', num(dv.largestShare));
    }
    for (const [k, v] of [['exposure', n.exposure], ['static', n.static], ['existing', n.existing], ['optimized', n.optimized], ['dividend', n.dividend], ['residual', n.residual], ['largestShare', n.largestShare], ['baseLargestShare', n.baseLargestShare], ['n1Reoptimized', n.n1Reoptimized], ['diversion', n.diversion]] as const)
      unit(n.id, k, num(v));
    unit(n.id, 'hhi', num(n.hhi), true);
    unit(n.id, 'baseline.hhi', num(n.baseline?.hhi), true);
    const sum = (n.baseline?.suppliers ?? []).reduce((acc, [, sh]) => acc + (num(sh) ?? 0), 0);
    if (sum > 1 + 1e-6) rng.push(`${n.id}: baseline supplier shares sum to ${r4(sum)}`);
    if (num(n.lostUsd) !== null && (n.lostUsd as number) < 0) rng.push(`${n.id}: lostUsd ${n.lostUsd}`);
  }
  R.check('BQ-AIB-PATH-01', 'aib.coverage', path, 'static <= existing <= optimized <= 1 for every node (Decoupling and Managed), within 1e-9', (k) => `${k} coverage-path violations`, 'fail', doc.nodes.length);
  R.check('BQ-AIB-RESID-01', 'aib.coverage', resid, 'residual = max(1 - optimized, 0) for every node (within 1e-6)', (k) => `${k} residuals do not equal 1 - optimized`, 'fail', doc.nodes.length);
  R.check('BQ-AIB-NUM-01', 'aib.numeric', rng, 'AIB shares and coverages are in [0,1], HHIs in (0,1], supplier shares sum <= 1, lost supply non-negative', (k) => `${k} out-of-range AIB values`, 'fail', doc.nodes.length);

  // the class rule on the exported fields
  const t = (doc.meta?.thresholds ?? {}) as unknown as Record<string, number | undefined>;
  const mism: string[] = [];
  let tested = 0;
  for (const n of doc.nodes) {
    if (n.cls === 'INSUFFICIENT DATA') continue;
    tested++;
    const r = aibClassFromExport(n, t);
    if (r.cls === null) mism.push(`${n.id}: undecidable (${r.why})`);
    else if (r.cls !== n.cls) mism.push(`${n.id}: shipped ${n.cls}, the rule on exported fields gives ${r.cls} (${r.why})`);
  }
  R.check('BQ-AIB-RULE-01', 'aib.reproduction', mism, `the AIB class rule (config/aib.yml classification, meta.thresholds) re-run on the exported exposure, residual, largest share, coverage loss at the cap, existing coverage and dividend reproduces all ${tested} classified nodes`, (k) => `${k} of ${tested} classified nodes are not reproduced by the class rule on the exported fields (the optimized top-3 share is not exported, so a MANAGE by top-3 cannot be seen)`, 'warn', tested);

  // zero-gain rank-1 hosts
  const zero = (hosts: readonly AibInvestmentHost[] | undefined, where: string) =>
    (hosts ?? []).filter((h) => h.rank === 1 && !((num(h.gain) ?? 0) > 0)).map((h) => `${where}: ${h.node} rank-1 host ${h.host} gain ${h.gain} (${h.rankBasis ?? 'no rank basis'})`);
  const z = [...zero(doc.investmentFlows?.hosts, 'decoupling'), ...zero(a.managed?.investmentFlows?.hosts, 'managed')];
  R.check('BQ-AIB-HOST-01', 'aib.hosts', z, 'every rank-1 investment host has a positive simulated gain (Decoupling and Managed)', (k) => `${k} rank-1 hosts with zero simulated gain (ranked by scale, not gain): never an intervention`, 'warn');

  // compacts
  const cp: string[] = [];
  for (const c of doc.compacts) {
    const m = c.metrics;
    const seq: [string, number | null][] = [['static', num(m.static)], ['existing', num(m.existing)], ['trade', num(m.trade)], ['investment', num(m.investment)]];
    for (let i = 1; i < seq.length; i++) {
      const [pa, pv] = seq[i - 1];
      const [na, nv] = seq[i];
      if (pv !== null && nv !== null && pv > nv + EPS) cp.push(`${c.id}: ${pa} ${pv} > ${na} ${nv}`);
    }
    if (num(m.investment) !== null && (m.investment as number) > 1 + EPS) cp.push(`${c.id}: investment coverage ${m.investment} > 1`);
  }
  for (const sc of doc.strategic?.compacts ?? []) {
    for (const [sid, rec] of Object.entries(sc.scenarios)) {
      const cv = rec?.coverage;
      if (!cv) continue;
      const seq: [string, number | null][] = [['static', num(cv.static)], ['existing', num(cv.existing)], ['coordinated', num(cv.coordinated)], ['postInvestment', num(cv.postInvestment)]];
      for (let i = 1; i < seq.length; i++) {
        const [pa, pv] = seq[i - 1];
        const [na, nv] = seq[i];
        if (pv !== null && nv !== null && pv > nv + EPS) cp.push(`${sc.id} ${sid}: ${pa} ${pv} > ${na} ${nv}`);
      }
    }
  }
  R.check('BQ-AIB-PATH-02', 'aib.coverage', cp, 'compact coverage paths are monotone (operational: static <= existing <= trade <= investment; strategic: static <= existing <= coordinated <= postInvestment)', (k) => `${k} compact coverage-path violations`, 'fail');
  const anchors = doc.compacts.filter((c) => num(c.metrics.anchorCompact) !== null && num(c.metrics.trade) !== null && (c.metrics.anchorCompact as number) - (c.metrics.trade as number) > 0.1);
  R.add('BQ-AIB-ANCHOR-01', 'aib.coverage', anchors.length ? 'info' : 'pass', anchors.length, `${anchors.length} operational compacts show anchor coverage more than 10 points above trade coverage (the "compact gain" is largely redistribution within the scope plus simulated capacity)`, anchors.map((c) => `${c.id}: anchorCompact ${c.metrics.anchorCompact} vs trade ${c.metrics.trade}`));
}

// ---- provenance and release ---------------------------------------------------------------------------------------

function provenanceChecks(input: BundleQcInput, R: Rows): void {
  const b = input.decoded;
  const st = b.stamps;
  const files = b.files;
  const pm = b.posture?.manifest ?? null;
  const posture: string[] = [];
  if (!st.posture?.created) posture.push('posture manifest: no created');
  if (!st.posture?.git_commit) posture.push('posture manifest: no git_commit');
  if (b.posture && b.posture.core.gitCommit !== (pm?.git_commit ?? null)) posture.push(`core git_commit ${b.posture.core.gitCommit} vs manifest ${String(pm?.git_commit)}`);
  if (b.posture && b.posture.core.created !== (pm?.created ?? null)) posture.push(`core created ${b.posture.core.created} vs manifest ${String(pm?.created)}`);
  R.check('BQ-PROV-POSTURE-01', 'provenance', posture, `posture manifest and core stamped ${st.posture?.created} @ ${st.posture?.git_commit}`, (k) => `${k} posture stamp problems`, 'warn');
  const det = files.filter((f) => /^posture\/posture-detail-/.test(f.file));
  const detNoCommit = det.filter((f) => !f.git_commit);
  const detNoCreated = det.filter((f) => !f.created);
  R.add('BQ-PROV-DETAIL-01', 'provenance', detNoCommit.length || detNoCreated.length ? 'warn' : 'pass', detNoCommit.length, `${det.length} posture detail documents: ${det.length - detNoCreated.length} carry created, ${det.length - detNoCommit.length} carry a git commit (the detail header has none: a detail file cannot be tied to a commit on its own)`, detNoCommit.slice(0, 3).map((f) => f.file));
  const aibFiles = files.filter((f) => f.file.startsWith('aib/'));
  R.add('BQ-PROV-AIB-01', 'provenance', aibFiles.some((f) => !f.git_commit) ? 'warn' : 'pass', aibFiles.filter((f) => !f.git_commit).length, `AIB bundle created ${st.aib?.created ?? 'n/a'}; ${aibFiles.filter((f) => !f.git_commit).length} of ${aibFiles.length} AIB files carry no git commit (manifest, compacts and scenario documents)`, aibFiles.filter((f) => !f.git_commit).map((f) => f.file));
  const memoFiles = files.filter((f) => f.file.startsWith('partnerships/'));
  R.add('BQ-PROV-MEMO-01', 'provenance', memoFiles.some((f) => !f.git_commit) ? 'warn' : 'pass', memoFiles.filter((f) => !f.git_commit).length, `${memoFiles.length} partnership files read; none carries a git commit (per-memo generated_at only)`, memoFiles.map((f) => f.file));
  const other: string[] = [];
  for (const [name, m] of Object.entries(b.manifests)) {
    if (!m) other.push(`${name}: manifest missing`);
    else {
      if (typeof m.created !== 'string') other.push(`${name}: no created`);
      if (typeof m.git_commit !== 'string') other.push(`${name}: no git_commit`);
    }
  }
  R.check('BQ-PROV-OTHER-01', 'provenance', other, `country ${String(b.manifests.country?.created)} @ ${String(b.manifests.country?.git_commit)}, sector ${String(b.manifests.sector?.created)} @ ${String(b.manifests.sector?.git_commit)}, strategic ${String(b.manifests.strategic?.created)} @ ${String(b.manifests.strategic?.git_commit)}: all stamped`, (k) => `${k} stamp problems in the country / sector / strategic manifests`, 'warn');
  const commits = new Set<string>([st.posture?.git_commit, b.manifests.country?.git_commit, b.manifests.sector?.git_commit, b.manifests.strategic?.git_commit].filter((x): x is string => typeof x === 'string'));
  const created = [st.posture?.created, b.manifests.country?.created, b.manifests.sector?.created, b.manifests.strategic?.created, st.aib?.created].filter((x): x is string => typeof x === 'string').map((x) => x.slice(0, 10));
  R.add('BQ-PROV-MIX-01', 'provenance', commits.size > 1 ? 'info' : 'pass', commits.size, `the bundles come from ${commits.size} commits (${[...commits].join(', ')}) and ${new Set(created).size} export dates (${[...new Set(created)].sort().join(', ')}); only the data release ID ties them together`);
  const rc = releaseCompleteness(st);
  R.add('BQ-REL-01', 'provenance', rc.complete ? 'pass' : 'warn', rc.missing.length, `data release ${input.releaseId ?? '(not computed)'}: ${rc.complete ? 'every bundle stamp present' : `stamps missing for ${rc.missing.join(', ')}`}`, rc.missing);
  const lp = input.loadProblems ?? [];
  R.check('BQ-LOAD-01', 'provenance', [...lp], `${files.length} bundle files read and hashed (sha256)`, (k) => `${k} files missing or unreadable`, 'fail', files.length);
  R.check('BQ-DECODE-01', 'schema', b.problems.map((p) => `${p.bundle}: ${p.problem}`), 'every bundle read decodes with the app\'s own decoders', (k) => `${k} decode problems`, 'fail');
}

function memoChecks(input: BundleQcInput, R: Rows): void {
  const pr = input.decoded.partnerships;
  if (!pr) {
    R.add('BQ-MEMO-01', 'memos', 'warn', 0, 'no partnerships bundle in this release');
    return;
  }
  const entries = [...Object.entries(pr.manifest.pairs), ...Object.entries(pr.manifest.compacts)];
  const unverified = entries.filter(([, e]) => e.verified !== true).map(([id, e]) => `${id}: verified ${String(e.verified)}, generated ${e.generated_at ?? 'n/a'}, as of ${e.as_of ?? 'n/a'}`);
  R.check('BQ-MEMO-01', 'memos', unverified, `${entries.length} memos, all independently verified`, (k) => `${k} of ${entries.length} shipped memos are not independently verified (rule-generated; no claim-level provenance shipped)`, 'warn', entries.length);
  const missing = entries.filter(([id]) => !pr.memos[id]).map(([id]) => id);
  R.check('BQ-MEMO-02', 'memos', missing, `every memo the manifest names has its JSON document (${Object.keys(pr.memos).length})`, (k) => `${k} memos named by the manifest have no readable JSON document`, 'fail', entries.length);
}

// ---- entry point ------------------------------------------------------------------------------------------------------

/** Run every shipped-bundle check. */
export function runBundleQc(input: BundleQcInput): QcRow[] {
  const R = new Rows(input.maxExamples ?? 8);
  provenanceChecks(input, R);
  postureChecks(input, R);
  aibChecks(input, R);
  memoChecks(input, R);
  return R.rows;
}

/** Counts by status. */
export function qcSummary(rows: readonly QcRow[]): Record<QcStatus, number> {
  const out: Record<QcStatus, number> = { pass: 0, warn: 0, fail: 0, info: 0 };
  for (const r of rows) out[r.status]++;
  return out;
}

/** The posture cells a check can be keyed by (exported for tests). */
export const postureCellKey = (c: Pick<PostureCell, 'iso3' | 'tech' | 'stage'>): string => cellKey(c.iso3, c.tech, c.stage);
