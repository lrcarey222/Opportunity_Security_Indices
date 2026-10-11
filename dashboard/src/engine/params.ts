// Dials and parameters: port of R/dashboard/dash_params.R (spec_react.md 3, 6.3). Pure.
//
// makeParams() returns the structure of unclass(dash_params()) with character scalars unboxed;
// paramsToRJson() boxes them again ("x" -> ["x"]) so the result is deep-equal to the JSON R writes
// (manifest.baseline_params, the fixtures' params). The dial multiplier m(d) = 4^((d - 50) / 50) and
// the populism stage weights come from the R-computed dial grid (manifest.dial_grid). The endowment
// screen of the allied picks is an advanced setting (require_resource_base, default the build's),
// read by alliedSelection() through params.si.require_resource_base.

import { s1 } from './bundle.ts';
import { deepEqual, EngineError, fmtF, fmtG, sumList } from './numeric.ts';
import { table } from './table.ts';
import type { Advanced, Bundle, Dials, DialGrid, Fossil, NumRecord, Params, PsiView, ScenarioSpec, Table } from './types.ts';
import { DIAL_NAMES } from './types.ts';

export { DIAL_NAMES };
export const DIAL_CODES = { hawkishness: 'H', bullishness: 'B', populism: 'P', greenness: 'G' } as const;

/** m(d) from the dial grid (Math.pow only for a non-integer d, which the UI never produces). */
export function dialMultiplier(d: number, grid: DialGrid): number {
  if (!Number.isFinite(d) || d < 0 || d > 100) throw new EngineError('PARAMS', 'Dial settings must be numbers in [0, 100].');
  if (Number.isInteger(d)) {
    const k = grid.d.indexOf(d);
    if (k >= 0) return grid.m[k];
  }
  return Math.pow(4, (d - 50) / 50);
}

/** Populism stage weights lambda_s = lambda_max (lambda0_s / lambda_max)^mP from the dial grid. */
export function stageWeights(P: number, grid: DialGrid, sw0: NumRecord, mP: number): NumRecord {
  if (mP === 1) return { ...sw0 };
  const out: NumRecord = {};
  const k = Number.isInteger(P) ? grid.d.indexOf(P) : -1;
  const vals = Object.values(sw0);
  const lmax = Math.max(...vals);
  for (const s of Object.keys(sw0)) {
    const g = grid.stage_weights[s];
    out[s] = k >= 0 && g ? g[k] : lmax * Math.pow(sw0[s] / lmax, mP);
  }
  return out;
}

export function defaultDials(): Dials {
  return { hawkishness: 50, bullishness: 50, populism: 50, greenness: 50 };
}

/** dash_check_dials(): missing dials take 50; unknown names and values outside [0, 100] stop. */
export function checkDials(x: unknown): Dials {
  const out = defaultDials();
  if (x === null || x === undefined) return out;
  if (typeof x !== 'object') throw new EngineError('PARAMS', 'Dials must be an object.');
  const unknown = Object.keys(x).filter((k) => !(DIAL_NAMES as readonly string[]).includes(k));
  if (unknown.length) throw new EngineError('PARAMS', `Unknown dial(s): ${unknown.join(', ')}.`);
  for (const [k, v0] of Object.entries(x as Record<string, unknown>)) {
    const v = Array.isArray(v0) && v0.length === 1 ? Number(v0[0]) : Number(v0);
    if (!Number.isFinite(v) || v < 0 || v > 100) throw new EngineError('PARAMS', `Dial ${k} must be one number in [0, 100].`);
    out[k as keyof Dials] = v;
  }
  return out;
}

/** Clamp and round dials read from a URL (values outside 0-100 or non-integers), with a flag. */
export function clampDials(x: Partial<Record<string, unknown>>): { dials: Dials; adjusted: boolean } {
  const out = defaultDials();
  let adjusted = false;
  for (const k of DIAL_NAMES) {
    if (x[k] === undefined || x[k] === null || x[k] === '') continue;
    const v = Number(x[k]);
    if (!Number.isFinite(v)) {
      adjusted = true;
      continue;
    }
    const c = Math.min(100, Math.max(0, Math.round(v)));
    if (c !== v) adjusted = true;
    out[k] = c;
  }
  return { dials: out, adjusted };
}

/** dash_default_advanced(): the paper's settings. */
export function defaultAdvanced(bundle: Pick<Bundle, 'baselineInputs'>): Advanced {
  const bi = bundle.baselineInputs;
  return {
    es_weights: { ...bi.es_weights },
    eo_weights: { ...bi.eo_weights },
    tau: bi.tau,
    fossil: 'auto',
    require_resource_base: bi.require_resource_base !== false,
    psi_view: bi.psi_view,
    network: {
      lambda: null,
      max_share: bi.max_share,
      min_producers: Math.trunc(bi.min_producers),
      portfolio_enable: bi.portfolio.enable === true,
    },
  };
}

function checkCategoryWeights(w: unknown, w0: NumRecord, label: string): NumRecord {
  if (w === null || w === undefined) return { ...w0 };
  if (typeof w !== 'object' || Array.isArray(w)) {
    if (Array.isArray(w) && w.length === 0) return { ...w0 };
    throw new EngineError('PARAMS', `${label} must be a named numeric vector.`);
  }
  const entries = Object.entries(w as Record<string, unknown>);
  const unknown = entries.map(([k]) => k).filter((k) => !(k in w0));
  if (unknown.length) throw new EngineError('PARAMS', `${label}: unknown categories ${unknown.join(', ')}.`);
  const out: NumRecord = { ...w0 };
  for (const [k, v0] of entries) {
    const v = Number(v0);
    if (!Number.isFinite(v) || v < 0) throw new EngineError('PARAMS', `${label} must be finite and >= 0.`);
    out[k] = v;
  }
  if (!(sumList(Object.values(out)) > 0)) throw new EngineError('PARAMS', `${label}: at least one category needs a positive weight.`);
  return out;
}

const isNA = (v: unknown) =>
  v === null || v === undefined || (typeof v === 'number' && v !== v) || (Array.isArray(v) && (v.length === 0 || (v.length === 1 && (v[0] === null || v[0] === 'NA'))));

/** dash_check_advanced(): the advanced settings with defaults for whatever is left out. */
export function checkAdvanced(x: unknown, bundle: Pick<Bundle, 'baselineInputs'>): Advanced {
  const bi = bundle.baselineInputs;
  const def = defaultAdvanced(bundle);
  if (x === null || x === undefined || (Array.isArray(x) && x.length === 0)) return def;
  if (typeof x !== 'object' || Array.isArray(x)) throw new EngineError('PARAMS', 'advanced must be an object (see defaultAdvanced()).');
  const a = x as Record<string, unknown>;
  const unknown = Object.keys(a).filter((k) => !(k in def));
  if (unknown.length) throw new EngineError('PARAMS', `Unknown advanced setting(s): ${unknown.join(', ')}.`);
  const out: Advanced = { ...def, network: { ...def.network } };
  out.es_weights = checkCategoryWeights(a.es_weights, bi.es_weights, 'es_weights');
  out.eo_weights = checkCategoryWeights(a.eo_weights, bi.eo_weights, 'eo_weights');
  if ('tau' in a) {
    if (isNA(a.tau)) out.tau = null;
    else {
      const tau = Number(Array.isArray(a.tau) ? a.tau[0] : a.tau);
      if (!Number.isFinite(tau) || tau < 0 || tau > 1) throw new EngineError('PARAMS', 'tau must be NA or one number in [0, 1].');
      out.tau = tau;
    }
  }
  if (a.fossil !== undefined && a.fossil !== null) {
    const f = s1(a.fossil);
    if (!['auto', 'never', 'always'].includes(f)) throw new EngineError('PARAMS', "fossil must be 'auto', 'never' or 'always'.");
    out.fossil = f as Fossil;
  }
  if (a.require_resource_base !== undefined && a.require_resource_base !== null) {
    const v = Array.isArray(a.require_resource_base) && a.require_resource_base.length === 1 ? a.require_resource_base[0] : a.require_resource_base;
    if (v === true || v === 'TRUE' || v === 'true' || v === 1) out.require_resource_base = true;
    else if (v === false || v === 'FALSE' || v === 'false' || v === 0) out.require_resource_base = false;
    else throw new EngineError('PARAMS', 'require_resource_base must be TRUE or FALSE.');
  }
  if (a.psi_view !== undefined && a.psi_view !== null) {
    const v = s1(a.psi_view);
    if (!['coalition', 'world'].includes(v)) throw new EngineError('PARAMS', "psi_view must be 'coalition' or 'world'.");
    out.psi_view = v as PsiView;
  }
  if (a.network !== undefined && a.network !== null && !(Array.isArray(a.network) && a.network.length === 0)) {
    const net = a.network as Record<string, unknown>;
    const unk = Object.keys(net).filter((k) => !(k in def.network));
    if (unk.length) throw new EngineError('PARAMS', `Unknown network setting(s): ${unk.join(', ')}.`);
    if ('lambda' in net) {
      if (isNA(net.lambda)) out.network.lambda = null;
      else {
        const lam = Number(net.lambda);
        if (!Number.isFinite(lam) || lam < 0) throw new EngineError('PARAMS', 'network lambda must be NA or one number >= 0.');
        out.network.lambda = lam;
      }
    }
    if (net.max_share !== undefined && net.max_share !== null) {
      const ms = Number(net.max_share);
      if (!Number.isFinite(ms) || ms <= 0 || ms > 1) throw new EngineError('PARAMS', 'max_share must be in (0, 1].');
      out.network.max_share = ms;
    }
    if (net.min_producers !== undefined && net.min_producers !== null) {
      const v = Number(net.min_producers);
      const mp = Number.isFinite(v) ? Math.round(v) : NaN;
      if (!Number.isFinite(mp) || mp < 1) throw new EngineError('PARAMS', 'min_producers must be a positive integer.');
      out.network.min_producers = mp;
    }
    if (net.portfolio_enable !== undefined && net.portfolio_enable !== null) {
      const pe = Array.isArray(net.portfolio_enable) ? net.portfolio_enable[0] : net.portfolio_enable;
      out.network.portfolio_enable = pe === true || pe === 'TRUE' || pe === 'true' || pe === 1;
    }
  }
  return out;
}

/** dash_si_weights(): w_k = w0_k m_k S0 / sum_j w0_j m_j (the baseline weights when every m is 1). */
export function siWeights(w0: NumRecord, mult: NumRecord): NumRecord {
  const names = Object.keys(w0);
  if (names.every((k) => mult[k] === 1)) return { ...w0 };
  const raw: NumRecord = {};
  for (const k of names) {
    if (mult[k] === undefined) throw new EngineError('PARAMS', `No multiplier for SI weight ${k}.`);
    raw[k] = w0[k] * mult[k];
  }
  const f = sumList(names.map((k) => w0[k])) / sumList(names.map((k) => raw[k]));
  const out: NumRecord = {};
  for (const k of names) out[k] = raw[k] * f;
  return out;
}

/** dash_params(): the parameter object every engine function reads. */
export function makeParams(spec: ScenarioSpec | null | undefined, bundle: Pick<Bundle, 'baselineInputs' | 'manifest'>): Params {
  const bi = bundle.baselineInputs;
  const grid = bundle.manifest.dial_grid;
  const dials = checkDials(spec?.dials ?? null);
  const adv = checkAdvanced(spec?.advanced ?? null, bundle);
  const mH = dialMultiplier(dials.hawkishness, grid);
  const mB = dialMultiplier(dials.bullishness, grid);
  const mP = dialMultiplier(dials.populism, grid);
  const mG = dialMultiplier(dials.greenness, grid);

  const fossilExcluded = adv.fossil === 'auto' ? dials.greenness >= bi.fossil_threshold : adv.fossil === 'always';
  const excludeTechs = fossilExcluded ? [...bi.fossil_techs] : [];

  const pw: NumRecord = { ...bi.policy_weights };
  if (bi.policy_iea_var in pw) pw[bi.policy_iea_var] = pw[bi.policy_iea_var] * mP;
  if (bi.policy_cat_var in pw) pw[bi.policy_cat_var] = pw[bi.policy_cat_var] * mG;

  const siMult: NumRecord = { policy: mP, economic_opportunity: mB, energy_security_need: mH, supply_chain: 1, technology: mG };
  const si = {
    weights: siWeights(bi.si_weights, siMult),
    stage_weights: stageWeights(dials.populism, grid, bi.stage_weights, mP),
    excluded_sectors: [...bi.excluded_sectors],
    fossil_excluded: fossilExcluded,
    fossil_techs: [...bi.fossil_techs],
    require_resource_base: adv.require_resource_base,
  };

  const fsWeights: NumRecord = { ...bi.fs_weights };
  if (fsWeights.geo_alignment !== undefined && fsWeights.geo_alignment !== null) fsWeights.geo_alignment = fsWeights.geo_alignment * mH;
  const psiWeights: NumRecord = { ...bi.psi_weights };
  psiWeights.friendshore = psiWeights.friendshore * mH;
  psiWeights.opportunity = psiWeights.opportunity * mB;
  const partner = {
    fs_weights: fsWeights,
    fs_penalty: bi.fs_penalty * mG,
    po_weights: { ...bi.po_weights },
    po_penalty: bi.po_penalty * mG,
    psi_weights: psiWeights,
    psi_view: adv.psi_view,
    psi_tau: bi.psi_tau,
    top_k: Math.trunc(bi.top_k),
    exclude_techs: excludeTechs,
  };

  const nw = { eo: bi.node_weights.eo, policy: bi.node_weights.policy, resilience: bi.node_weights.resilience };
  let nodePolicy = bi.node_policy === true;
  const ramp = Math.max(0, (dials.populism - 50) / 50);
  if (nodePolicy) nw.policy = nw.policy * mP;
  else if (ramp > 0) {
    nodePolicy = true;
    nw.policy = bi.node_policy_weight_max * ramp;
  }
  nw.eo = nw.eo * mB;
  nw.resilience = nw.resilience * mH;
  const lambda = adv.network.lambda === null ? bi.lambda * mH : adv.network.lambda;
  const network = {
    node_weights: { eo: nw.eo, policy: nw.policy, resilience: nw.resilience },
    node_policy: nodePolicy,
    edge_weights: { friendshore: bi.edge_weights.friendshore * mH, opportunity: bi.edge_weights.opportunity * mB },
    demand_weights: { need: bi.demand_weights.need * mH, size: bi.demand_weights.size * mB },
    diversity_lambda: lambda,
    self_multiplier: mP,
    max_share: adv.network.max_share,
    min_share: bi.min_share,
    min_producers: adv.network.min_producers,
    min_suppliers_per_consumer: bi.min_suppliers_per_consumer,
    epsilon_supplier_share: bi.epsilon_supplier_share,
    w_node: bi.w_node,
    w_edge: bi.w_edge,
    w_dev: bi.w_dev,
    allow_self: bi.allow_self,
    portfolio: { ...bi.portfolio, enable: adv.network.portfolio_enable },
    diversity_breakpoint_step: bi.diversity_breakpoint_step,
    supply_chains: [...bi.supply_chains],
    exclude_techs: [...excludeTechs],
    milp_time_limit: bi.milp_time_limit,
    policy_missing: bi.policy_missing,
    missing_components: bi.missing_components,
    demand_mode: bi.demand_mode,
    size_transform: bi.size_transform,
  };
  return {
    dials,
    m: { H: mH, B: mB, P: mP, G: mG },
    advanced: adv,
    pillars: { es_weights: { ...adv.es_weights }, eo_weights: { ...adv.eo_weights }, tau: adv.tau },
    policy: { weights: pw },
    si,
    partner,
    network,
  };
}

/** The params as R serialises them (character scalars boxed: "x" -> ["x"]). */
export function paramsToRJson(x: unknown): unknown {
  if (typeof x === 'string') return [x];
  if (Array.isArray(x)) return x.map((v) => (typeof v === 'string' ? v : paramsToRJson(v)));
  if (x !== null && typeof x === 'object') {
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(x)) out[k] = paramsToRJson(v);
    return out;
  }
  return x;
}

/** dash_is_baseline(): params equal to the paper's (manifest.baseline_params). */
export function isBaseline(params: Params, bundle: Pick<Bundle, 'manifest'>): boolean {
  return deepEqual(paramsToRJson(params), bundle.manifest.baseline_params);
}

export type Layer =
  | 'pillars' | 'policy' | 'si' | 'partner_cells' | 'fs_cells' | 'po_cells' | 'psi' | 'top5'
  | 'net_inputs' | 'net_score' | 'net_solve' | 'all';

/** dash_params_layer(): the subset of the parameters a recompute layer reads. */
export function paramsLayer(p: Params, layer: Layer): unknown {
  const fs = { fs_weights: p.partner.fs_weights, fs_penalty: p.partner.fs_penalty };
  const po = { po_weights: p.partner.po_weights, po_penalty: p.partner.po_penalty };
  const netInputs = { fs, po, stages: { supply_chains: p.network.supply_chains, exclude_techs: p.network.exclude_techs } };
  const n = p.network;
  const score = {
    node_weights: n.node_weights, node_policy: n.node_policy, edge_weights: n.edge_weights, demand_weights: n.demand_weights,
    self_multiplier: n.self_multiplier, policy_missing: n.policy_missing, missing_components: n.missing_components,
    demand_mode: n.demand_mode, size_transform: n.size_transform,
  };
  const netScore = { ...netInputs, policy: n.node_policy ? p.policy : null, score };
  switch (layer) {
    case 'pillars':
      return p.pillars;
    case 'policy':
      return p.policy;
    case 'si':
      return [p.pillars, p.policy, p.si];
    case 'fs_cells':
      return fs;
    case 'po_cells':
      return po;
    case 'partner_cells':
      return { fs, po };
    case 'top5':
      return { fs, po, exclude_techs: p.partner.exclude_techs, top_k: p.partner.top_k };
    case 'psi':
      return {
        fs, po, psi_weights: p.partner.psi_weights, psi_view: p.partner.psi_view, psi_tau: p.partner.psi_tau,
        top_k: p.partner.top_k, exclude_techs: p.partner.exclude_techs,
      };
    case 'net_inputs':
      return netInputs;
    case 'net_score':
      return netScore;
    case 'net_solve': {
      const solve: Record<string, unknown> = {};
      for (const [k, v] of Object.entries(n)) if (!(k in score)) solve[k] = v;
      return { ...netScore, solve };
    }
    case 'all':
      return p;
  }
}

function canonical(x: unknown): string {
  if (x === null || x === undefined) return 'null';
  if (typeof x === 'number') return x !== x ? '"NaN"' : Object.is(x, -0) ? '0' : String(x);
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'boolean') return x ? 'true' : 'false';
  if (Array.isArray(x)) return '[' + x.map(canonical).join(',') + ']';
  const keys = Object.keys(x as object).sort();
  return '{' + keys.map((k) => JSON.stringify(k) + ':' + canonical((x as Record<string, unknown>)[k])).join(',') + '}';
}

/** FNV-1a 64 of a string, as 16 hex digits. */
export function fnv1a64(s: string): string {
  let h = 0xcbf29ce484222325n;
  const prime = 0x100000001b3n;
  for (let i = 0; i < s.length; i++) {
    h ^= BigInt(s.charCodeAt(i));
    h = (h * prime) & 0xffffffffffffffffn;
  }
  return h.toString(16).padStart(16, '0');
}

/** Cache key of a layer: layer + ':' + FNV-1a 64 of the canonical JSON of the parameters it reads. */
export function paramsKey(p: Params, layer: Layer): string {
  return `${layer}:${fnv1a64(canonical(paramsLayer(p, layer)))}`;
}

// ---- Readout ---------------------------------------------------------------------------------------

type ReadoutRow = [dial: string, parameter: string, label: string, f: (p: Params) => number, salient: boolean];

function share(w: Record<string, number | null> | undefined, k: string): number {
  if (!w) return NaN;
  const v = w[k];
  if (v === undefined || v === null || v !== v) return NaN;
  return v / sumList(Object.values(w).filter((x): x is number => x !== null && x === x));
}

/** dash_dial_readout(): one row per (dial, parameter) with the baseline and current values. */
export function dialReadout(params: Params, bundle: Pick<Bundle, 'baselineInputs' | 'baselineParams'>): Table {
  const bi = bundle.baselineInputs;
  const base = bundle.baselineParams;
  const effNode = (p: Params, k: 'eo' | 'policy' | 'resilience') => {
    const nw = { ...p.network.node_weights };
    if (!p.network.node_policy) nw.policy = 0;
    return nw[k] / sumList([nw.eo, nw.policy, nw.resilience]);
  };
  const polShare = (p: Params, v: string) => {
    const w = p.policy.weights;
    if (!(v in w)) return NaN;
    return w[v] / sumList(Object.values(w));
  };
  const stageRatio = (p: Params, s: string) => p.si.stage_weights[s] / p.si.stage_weights.Midstream;
  const rows: ReadoutRow[] = [
    ['hawkishness', 'si_need', 'SI weight on security need', (p) => p.si.weights.energy_security_need, true],
    ['hawkishness', 'psi_fs_share', 'Friendshore share of the PSI', (p) => share(p.partner.psi_weights, 'friendshore'), true],
    ['hawkishness', 'edge_fs_share', 'Friendshore share of network edges', (p) => share(p.network.edge_weights, 'friendshore'), false],
    ['hawkishness', 'fs_geo_share', 'Alignment share of Friendshore', (p) => share(p.partner.fs_weights, 'geo_alignment'), false],
    ['hawkishness', 'lambda', 'Network diversification lambda', (p) => p.network.diversity_lambda, true],
    ['hawkishness', 'demand_need_share', 'Need share of network demand', (p) => share(p.network.demand_weights, 'need'), false],
    ['hawkishness', 'node_resilience', 'Resilience weight in producer score', (p) => effNode(p, 'resilience'), false],
    ['bullishness', 'si_eo', 'SI weight on economic opportunity', (p) => p.si.weights.economic_opportunity, true],
    ['bullishness', 'psi_po_share', 'Opportunity share of the PSI', (p) => share(p.partner.psi_weights, 'opportunity'), true],
    ['bullishness', 'edge_po_share', 'Opportunity share of network edges', (p) => share(p.network.edge_weights, 'opportunity'), true],
    ['bullishness', 'node_eo', 'EO weight in producer score', (p) => effNode(p, 'eo'), false],
    ['bullishness', 'demand_size_share', 'Size share of network demand', (p) => share(p.network.demand_weights, 'size'), false],
    ['populism', 'si_policy', 'SI weight on policy', (p) => p.si.weights.policy, true],
    ['populism', 'policy_iea_share', 'IEA PAMS share of the Policy index', (p) => polShare(p, bi.policy_iea_var), false],
    ['populism', 'stage_up_ratio', 'Upstream / Midstream stage weight', (p) => stageRatio(p, 'Upstream'), true],
    ['populism', 'stage_down_ratio', 'Downstream / Midstream stage weight', (p) => stageRatio(p, 'Downstream'), false],
    ['populism', 'node_policy', 'Policy weight in producer score', (p) => effNode(p, 'policy'), false],
    ['populism', 'home_bias', 'Home-bias multiplier on self-supply', (p) => p.network.self_multiplier, true],
    ['greenness', 'si_tech', 'SI weight on technology (GHG)', (p) => p.si.weights.technology, true],
    ['greenness', 'policy_cat_share', 'CAT share of the Policy index', (p) => polShare(p, bi.policy_cat_var), false],
    ['greenness', 'fs_penalty', 'Friendshore climate penalty (per side)', (p) => p.partner.fs_penalty, true],
    ['greenness', 'po_penalty', 'Opportunity climate penalty', (p) => p.partner.po_penalty, false],
    ['greenness', 'fossil', 'Fossil technologies excluded', (p) => (p.si.fossil_excluded ? 1 : 0), true],
  ];
  const n = rows.length;
  const dial: string[] = [];
  const parameter: string[] = [];
  const label: string[] = [];
  const baseline = new Float64Array(n);
  const current = new Float64Array(n);
  const multiplier = new Float64Array(n);
  const text: string[] = [];
  const salient = new Uint8Array(n);
  rows.forEach(([d, prm, lab, f, sal], i) => {
    const b = f(base);
    const cur = f(params);
    const ratio = Number.isFinite(b) && b !== 0 ? cur / b : NaN;
    let t: string;
    if (prm === 'fossil') t = cur > 0 ? 'Coal, Oil and Gas excluded' : 'Fossil technologies included';
    else if (prm === 'home_bias') t = `${lab} x${fmtF(cur, 2)}`;
    else if (Number.isFinite(ratio) && Math.abs(ratio - 1) > 1e-9) t = `${lab} ${fmtG(cur, 3)} (x${fmtF(ratio, 2)} vs ${fmtG(b, 3)})`;
    else if (!Number.isFinite(ratio) && Number.isFinite(cur) && cur !== b) t = `${lab} ${fmtG(cur, 3)} (paper: ${fmtG(b, 3)})`;
    else t = `${lab} ${fmtG(cur, 3)} (paper)`;
    dial.push(d);
    parameter.push(prm);
    label.push(lab);
    baseline[i] = b;
    current[i] = cur;
    multiplier[i] = ratio;
    text.push(t);
    salient[i] = sal ? 1 : 0;
  });
  return table({ dial, parameter, label, baseline, current, multiplier, text, salient });
}

/** dash_presets(). */
export function presets(): Record<string, Dials> {
  const base = defaultDials();
  return {
    'Paper baseline': base,
    Hawk: { ...base, hawkishness: 90 },
    Bull: { ...base, bullishness: 90 },
    Populist: { ...base, populism: 90 },
    Green: { ...base, greenness: 90 },
    'Green hawk': { ...base, hawkishness: 80, greenness: 80 },
  };
}

/** dash_params_spec(): the dials and advanced settings of a params object. */
export function paramsSpec(p: Params): ScenarioSpec {
  return { dials: { ...p.dials }, advanced: JSON.parse(JSON.stringify(p.advanced)) as Advanced };
}

/** The advanced settings that differ from the defaults (what the URL needs to carry). */
export function advancedDiff(adv: Advanced, bundle: Pick<Bundle, 'baselineInputs'>): ScenarioSpec['advanced'] {
  const def = defaultAdvanced(bundle);
  const out: Record<string, unknown> = {};
  for (const k of ['es_weights', 'eo_weights', 'tau', 'fossil', 'require_resource_base', 'psi_view'] as const) {
    if (!deepEqual(adv[k], def[k])) out[k] = adv[k];
  }
  const net: Record<string, unknown> = {};
  for (const k of ['lambda', 'max_share', 'min_producers', 'portfolio_enable'] as const) {
    if (!deepEqual(adv.network[k], def.network[k])) net[k] = adv.network[k];
  }
  if (Object.keys(net).length) out.network = net;
  return out as ScenarioSpec['advanced'];
}
