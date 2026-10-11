// L5b: producer scores, demand and edge weights (port of allied_network_score_nodes(),
// allied_network_normalize_weights() and dash_network_score(); spec_react.md 6.6). Pure.

import { EngineError, LdAcc, medianR, medianScurve, NA_BOOL, sumList } from '../numeric.ts';
import { lgl, num, str, table } from '../table.ts';
import type { Params, Str, Table } from '../types.ts';
import type { NetInputs, Stage } from './inputs.ts';

/** allied_network_normalize_weights(): w / sum(w); all NA when the weights do not sum above 0. */
export function normalizeWeights(w: Record<string, number>): Record<string, number> {
  const vals = Object.values(w);
  const fin = vals.filter((v) => v === v);
  const out: Record<string, number> = {};
  if (!vals.length || !fin.length || !(sumList(fin) > 0)) {
    for (const k of Object.keys(w)) out[k] = NaN;
    return out;
  }
  const s = sumList(fin);
  for (const [k, v] of Object.entries(w)) out[k] = v / s;
  return out;
}

function groupsOf(T: Str, S: Str): number[][] {
  const m = new Map<string, number[]>();
  for (let i = 0; i < T.length; i++) {
    const k = `${T[i]}\r${S[i]}`;
    let g = m.get(k);
    if (!g) m.set(k, (g = []));
    g.push(i);
  }
  return Array.from(m.values());
}

export interface ScoreOptions {
  node_weights: { eo: number; policy: number; resilience: number };
  demand_mode: string;
  demand_weights: { need: number; size: number };
  size_transform: string;
  policy_missing: string;
  missing_components: string;
  node_policy: boolean;
}

/** allied_network_score_nodes(). */
export function scoreNodes(raw: Table, o: ScoreOptions): Table {
  const n = raw.nrow;
  const wn = normalizeWeights({ eo: o.node_weights.eo, policy: o.node_policy ? o.node_weights.policy : 0, resilience: o.node_weights.resilience });
  if (Object.values(wn).some((v) => v !== v)) throw new EngineError('NETWORK', 'node_weights must be positive (eo/policy/resilience).');
  const T = str(raw, 'tech');
  const S = str(raw, 'supply_chain');
  const groups = groupsOf(T, S);
  const eoRaw = num(raw, 'eo_raw');
  const esRaw = num(raw, 'es_raw');
  const esNeedRaw = raw.cols.es_need_raw ? num(raw, 'es_need_raw') : esRaw;
  let polRaw = Float64Array.from(num(raw, 'policy_raw'));
  const polImp = new Uint8Array(n);
  if (o.node_policy) {
    for (const g of groups) {
      const anyObs = g.some((i) => polRaw[i] === polRaw[i]);
      const imp = g.map((i) => o.policy_missing === 'coalition_median' && polRaw[i] !== polRaw[i] && anyObs);
      const med = imp.some(Boolean) ? medianR(g.map((i) => polRaw[i])) : NaN;
      g.forEach((i, k) => {
        polImp[i] = imp[k] ? 1 : 0;
        if (imp[k]) polRaw[i] = med;
      });
    }
  } else {
    polRaw = new Float64Array(n).fill(NaN);
  }
  const dev = Float64Array.from(num(raw, 'dev_potential'), (v) => (v !== v ? 0.5 : v));
  const gdp = num(raw, 'gdp_usd');
  const eoIdx = new Float64Array(n);
  const esIdx = new Float64Array(n);
  const polIdx = new Float64Array(n);
  const needIdx = new Float64Array(n);
  const sizeIdx = new Float64Array(n);
  const tmp = new Float64Array(n);
  const sizeIn = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    sizeIn[i] = o.size_transform === 'scurve' || o.size_transform === 'rank' ? gdp[i] : Math.log1p(gdp[i]);
  }
  for (const g of groups) {
    medianScurve(eoRaw, eoIdx, g);
    medianScurve(esRaw, esIdx, g);
    medianScurve(polRaw, polIdx, g);
    medianScurve(esNeedRaw, tmp, g);
    for (const i of g) needIdx[i] = 1 - tmp[i];
    if (o.size_transform === 'rank') {
      // dplyr::percent_rank(): (min_rank - 1) / (n - 1) over the non-NA values
      const vals = g.filter((i) => sizeIn[i] === sizeIn[i]);
      const m = vals.length;
      for (const i of g) {
        if (sizeIn[i] !== sizeIn[i]) {
          sizeIdx[i] = NaN;
          continue;
        }
        let less = 0;
        for (const j of vals) if (sizeIn[j] < sizeIn[i]) less++;
        sizeIdx[i] = m > 1 ? less / (m - 1) : NaN;
      }
    } else {
      medianScurve(sizeIn, sizeIdx, g);
    }
    for (const i of g) if (sizeIdx[i] !== sizeIdx[i]) sizeIdx[i] = 0;
  }
  const comps = new Int32Array(n);
  const score = new Float64Array(n);
  const needMissing = new Uint8Array(n);
  const demandMixed = new Float64Array(n);
  let wNeed = Number(o.demand_weights.need);
  let wSize = Number(o.demand_weights.size);
  if (!Number.isFinite(wNeed)) wNeed = 0.5;
  if (!Number.isFinite(wSize)) wSize = 0.5;
  for (let i = 0; i < n; i++) {
    const e = eoIdx[i] === eoIdx[i];
    const p = polIdx[i] === polIdx[i];
    const s = esIdx[i] === esIdx[i];
    const wAvail = wn.eo * (e ? 1 : 0) + wn.policy * (p ? 1 : 0) + wn.resilience * (s ? 1 : 0);
    const sum = wn.eo * (e ? eoIdx[i] : 0) + wn.policy * (p ? polIdx[i] : 0) + wn.resilience * (s ? esIdx[i] : 0);
    comps[i] = (e ? 1 : 0) + (p ? 1 : 0) + (s ? 1 : 0);
    if (o.missing_components === 'renormalize') score[i] = wAvail > 0 ? sum / wAvail : NaN;
    else score[i] = wn.eo * eoIdx[i] + (o.node_policy ? wn.policy * polIdx[i] : 0) + wn.resilience * esIdx[i];
    needMissing[i] = needIdx[i] !== needIdx[i] ? 1 : 0;
    const mixedComplete = wNeed + wSize > 0 ? (wNeed * needIdx[i] + wSize * sizeIdx[i]) / (wNeed + wSize) : NaN;
    const mixedSizeOnly = wSize > 0 ? sizeIdx[i] : 0;
    demandMixed[i] = o.missing_components === 'renormalize'
      ? (needIdx[i] !== needIdx[i] ? mixedSizeOnly : mixedComplete)
      : wNeed * needIdx[i] + wSize * sizeIdx[i];
  }
  const dw = new Float64Array(n);
  const dnc = new Float64Array(n);
  const dsc = new Float64Array(n);
  const co = (v: number) => (v !== v ? 0 : v);
  for (const g of groups) {
    for (const i of g) {
      switch (o.demand_mode) {
        case 'equal':
          dw[i] = 1;
          break;
        case 'size':
          dw[i] = co(sizeIdx[i]);
          break;
        case 'mixed':
          dw[i] = co(demandMixed[i]);
          break;
        default:
          dw[i] = co(needIdx[i]);
      }
      dnc[i] = o.demand_mode === 'mixed' ? co(wNeed * needIdx[i]) : o.demand_mode === 'need' ? co(needIdx[i]) : 0;
      dsc[i] = o.demand_mode === 'mixed' ? co(wSize * sizeIdx[i]) : o.demand_mode === 'size' ? co(sizeIdx[i]) : 0;
    }
    const sumOf = (x: Float64Array) => {
      const acc = new LdAcc();
      for (const i of g) if (x[i] === x[i]) acc.add(x[i]);
      return acc.value();
    };
    const allNa = g.every((i) => dw[i] !== dw[i]);
    if (allNa || sumOf(dw) <= 0) for (const i of g) dw[i] = 1;
    const sN = sumOf(dnc);
    for (const i of g) dnc[i] = sN > 0 ? dnc[i] / sN : 0;
    const sS = sumOf(dsc);
    for (const i of g) dsc[i] = sS > 0 ? dsc[i] / sS : 0;
    const sW = sumOf(dw);
    for (const i of g) dw[i] = dw[i] / sW;
  }
  const keys = new Set<string>();
  const I = str(raw, 'iso3c');
  for (let i = 0; i < n; i++) {
    const k = `${I[i]}\r${T[i]}\r${S[i]}`;
    if (keys.has(k)) throw new EngineError('NETWORK', 'allied_network_prepare_nodes: nodes are not unique per (iso3c, tech, supply_chain).');
    keys.add(k);
  }
  const cols: Table['cols'] = {};
  for (const [k, c] of Object.entries(raw.cols)) {
    if (k === 'es_need_raw') continue;
    cols[k] = c;
  }
  cols.policy_raw = polRaw;
  cols.dev_potential = dev;
  cols.policy_imputed = polImp;
  cols.eo_idx = eoIdx;
  cols.es_idx = esIdx;
  cols.pol_idx = polIdx;
  cols.need_idx = needIdx;
  cols.size_idx = sizeIdx;
  cols.node_components_n = comps;
  cols.producer_score = score;
  cols.demand_need_missing = needMissing;
  cols.demand_weight = dw;
  cols.demand_need_component = dnc;
  cols.demand_size_component = dsc;
  return table(cols, n);
}

export interface NetScored {
  nodes: Table;
  edges: Table;
  self_edges: Table | null;
  stages: Stage[];
  inputs: NetInputs;
}

/** dash_network_score(): producer scores and demand, edge weights and (populism != 50) self edges. */
export function networkScore(inputs: NetInputs, params: Params): NetScored {
  const np = params.network;
  const nodes = scoreNodes(inputs.raw_nodes, {
    node_weights: np.node_weights, demand_mode: np.demand_mode, demand_weights: np.demand_weights,
    size_transform: np.size_transform, policy_missing: np.policy_missing, missing_components: np.missing_components,
    node_policy: np.node_policy,
  });
  const w = normalizeWeights({ friendshore: np.edge_weights.friendshore, opportunity: np.edge_weights.opportunity });
  if (Object.values(w).some((v) => v !== v)) throw new EngineError('NETWORK', 'Network edge weights must be positive.');
  const e = inputs.edges;
  const fsi = num(e, 'friendshore_index');
  const opi = num(e, 'opportunity_index');
  const ew = Float64Array.from(fsi, (f, i) => w.friendshore * f + w.opportunity * opi[i]);
  let edges = table({
    tech: str(e, 'tech'), supply_chain: str(e, 'supply_chain'), reporter_iso: str(e, 'reporter_iso'), partner_iso: str(e, 'partner_iso'),
    edge_weight: ew, friendshore_index: fsi, opportunity_index: opi,
  });
  let selfEdges: Table | null = null;
  if (np.self_multiplier !== 1) {
    const nT = str(nodes, 'tech');
    const nS = str(nodes, 'supply_chain');
    const nI = str(nodes, 'iso3c');
    const eT = str(edges, 'tech');
    const eS = str(edges, 'supply_chain');
    const eR = str(edges, 'reporter_iso');
    const eP = str(edges, 'partner_iso');
    const st: Str = [];
    const ss: Str = [];
    const si: Str = [];
    const sw: number[] = [];
    for (const s of inputs.stages) {
      const iso: string[] = [];
      for (let i = 0; i < nodes.nrow; i++) if (nT[i] === s.tech && nS[i] === s.supply_chain) iso.push(nI[i] as string);
      const isoSet = new Set(iso);
      const vals: number[] = [];
      for (let i = 0; i < edges.nrow; i++) {
        if (eT[i] === s.tech && eS[i] === s.supply_chain && isoSet.has(eR[i] as string) && isoSet.has(eP[i] as string)) vals.push(ew[i]);
      }
      let med = medianR(vals);
      if (!Number.isFinite(med)) med = 0.5;
      const v = Math.min(1, np.self_multiplier * med);
      for (const x of iso) {
        st.push(s.tech);
        ss.push(s.supply_chain);
        si.push(x);
        sw.push(v);
      }
    }
    const k = st.length;
    selfEdges = table({
      tech: st, supply_chain: ss, reporter_iso: si, partner_iso: [...si], edge_weight: Float64Array.from(sw),
      friendshore_index: new Float64Array(k).fill(NaN), opportunity_index: new Float64Array(k).fill(NaN),
    });
    const cat = (a: Table['cols'][string], b: Table['cols'][string]) => {
      if (Array.isArray(a)) return [...a, ...(b as Str)];
      const out = new Float64Array(a.length + b.length);
      out.set(a as Float64Array);
      out.set(b as Float64Array, a.length);
      return out;
    };
    const cols: Table['cols'] = {};
    for (const key of Object.keys(edges.cols)) cols[key] = cat(edges.cols[key], selfEdges.cols[key]);
    edges = table(cols);
  }
  return { nodes, edges, self_edges: selfEdges, stages: inputs.stages, inputs };
}

export { lgl, NA_BOOL };
