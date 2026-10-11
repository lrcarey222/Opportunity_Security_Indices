// L5c: the greedy network solve with the portfolio-cap loop (port of dash_network_solve(),
// dash_build_topk_tbl(), allied_network_count_topk(), allied_network_disaggregate_shares(),
// dash_network_compare() and dash_network_concentration(); spec_react.md 6.6). Pure; async only to
// yield between stage solves (cancellation).

import { cmpC, EngineError, NA_BOOL, NA_INT, orderBy, roundR, sumList, yieldNow } from '../numeric.ts';
import { bindRows, lgl, num, str, table } from '../table.ts';
import type { Bundle, Column, Params, Str, Table } from '../types.ts';
import { memberWeightMap, stageKey, type Stage } from './inputs.ts';
import type { NetScored } from './score.ts';
import {
  NODE_EXTRA_COLS, objectiveTerms, solveStageGreedy, supplierStats, type StageEdges, type StageFlows, type StageNodes, type StageSpecialization,
} from './solver.ts';

export const SHARE_RANK_DIGITS = 9;

interface StageResult {
  tech: string;
  supply_chain: string;
  specialization: StageSpecialization | null;
  flows: StageFlows | null;
  objective: number;
  hhi: number;
  n_producers: number | null;
  method_used: string;
  fallback_reason: string | null;
  max_production_share: number;
  n_at_max_share?: number;
  objective_node?: number;
  objective_edge?: number;
  objective_dev?: number;
  diversity_penalty?: number;
  hhi_tangent_max_error?: number;
  min_suppliers_realized?: number | null;
  min_supplier_share_realized?: number;
  producers_forbidden?: string;
}

export interface Portfolio {
  caps: Table;
  counts: Table;
  topk: Table;
  converged: boolean | null;
  iters: number | null;
  violations_remaining: number | null;
}

export interface NetworkSolution {
  specialization: Table;
  flows: Table;
  diagnostics: Table;
  portfolio: Portfolio | null;
  member_shares: Table | null;
  forbidden: Record<string, string[]>;
  solver_by_stage: Table;
  stages: Stage[];
  elapsed_ms: number;
}

export interface SolveProgress {
  event: 'start_stage' | 'end_stage';
  current: number;
  total: number;
  tech: string;
  supply_chain: string;
  phase: 'initial' | 'portfolio';
  iter: number;
}

export interface SolveOptions {
  forbidden?: Record<string, string[]> | null;
  shareRankDigits?: number | null;
  onProgress?: (p: SolveProgress) => void;
  /** checked between stage solves; return true to abandon the solve (networkSolve then returns null) */
  shouldAbort?: () => boolean;
  /** yield to the event loop between stage solves (default true) */
  yieldBetweenStages?: boolean;
}

function colOf(t: Table, k: string): Column | undefined {
  return t.cols[k];
}

function pickCol(c: Column, idx: number[]): Column {
  if (Array.isArray(c)) return idx.map((i) => c[i]);
  const Ctor = c.constructor as { new (n: number): typeof c };
  const o = new Ctor(idx.length);
  idx.forEach((i, q) => (o[q] = c[i]));
  return o;
}

/** dash_build_topk_tbl() with share_rank_digits (null: allied_network_build_topk_tbl()). */
export function buildTopkTbl(spec: Table, topK: number, shareRankDigits: number | null = SHARE_RANK_DIGITS): Table {
  const n = spec.nrow;
  if (!n) return table({});
  const T = str(spec, 'tech');
  const S = str(spec, 'supply_chain');
  const I = str(spec, 'iso3c');
  const share = num(spec, 'production_share');
  const score = spec.cols.producer_score ? num(spec, 'producer_score') : null;
  const rankShare = Float64Array.from(share, (v) => (shareRankDigits === null ? v : roundR(v, shareRankDigits)));
  const rankIn = new Int32Array(n);
  const topkRank = new Int32Array(n);
  const inTop = new Uint8Array(n);
  const groups = new Map<string, number[]>();
  for (let i = 0; i < n; i++) {
    const k = `${T[i]}\r${S[i]}`;
    let g = groups.get(k);
    if (!g) groups.set(k, (g = []));
    g.push(i);
  }
  const co0 = (v: number) => (v !== v ? 0 : v);
  for (const g of groups.values()) {
    // dense_rank(desc(coalesce(share, 0)))
    const vals = Array.from(new Set(g.map((i) => co0(rankShare[i])))).sort((a, b) => b - a);
    for (const i of g) rankIn[i] = vals.indexOf(co0(rankShare[i])) + 1;
    // strict rank: share desc, score desc (NA = -Inf), iso asc
    const ord = orderBy(
      [
        { num: g.map((i) => co0(rankShare[i])), desc: true },
        { num: g.map((i) => (score && score[i] === score[i] ? score[i] : -Infinity)), desc: true },
        { str: g.map((i) => I[i]) },
      ],
      g.length,
    );
    ord.forEach((q, r) => (topkRank[g[q]] = r + 1));
    for (const i of g) inTop[i] = topkRank[i] <= topK && co0(share[i]) > 0 ? 1 : 0;
  }
  const cols: Record<string, Column> = { tech: T, supply_chain: S, iso3c: I, production_share: share };
  if (spec.cols.production_usd) cols.production_usd = spec.cols.production_usd;
  if (score) cols.producer_score = score;
  cols.rank_in_stage = rankIn;
  cols.topk_rank = topkRank;
  cols.in_top_k = inTop;
  return table(cols, n);
}

/** allied_network_count_topk(): stages in which each node is a top-K producer, against its cap. */
export function countTopk(topk: Table, isoNodes: string[], capByIso: Map<string, number>): Table {
  const counted = new Map<string, number>();
  if (topk.nrow) {
    const T = str(topk, 'tech');
    const S = str(topk, 'supply_chain');
    const I = str(topk, 'iso3c');
    const inTop = lgl(topk, 'in_top_k');
    const seen = new Set<string>();
    for (let i = 0; i < topk.nrow; i++) {
      if (inTop[i] !== 1) continue;
      const k = `${T[i]}\r${S[i]}\r${I[i]}`;
      if (seen.has(k)) continue;
      seen.add(k);
      counted.set(I[i] as string, (counted.get(I[i] as string) ?? 0) + 1);
    }
  }
  // count() sorts by iso3c; right_join keeps those rows (that are nodes) first, then the other nodes
  const nodeSet = new Set(isoNodes);
  const first = Array.from(counted.keys()).filter((k) => nodeSet.has(k)).sort(cmpC);
  const firstSet = new Set(first);
  const rest = isoNodes.filter((k) => !firstSet.has(k));
  const iso = [...first, ...rest];
  const cnt = Int32Array.from(iso, (k) => counted.get(k) ?? 0);
  const cap = Int32Array.from(iso, (k) => (capByIso.has(k) ? (capByIso.get(k) as number) : NA_INT));
  const over = Int32Array.from(iso, (_, i) => (cap[i] === NA_INT ? NA_INT : Math.max(0, cnt[i] - cap[i])));
  return table({ iso3c: iso, topk_count: cnt, portfolio_cap: cap, over_by: over });
}

function stageNodes(nodes: Table, idx: number[]): StageNodes {
  const extra: StageNodes['extra'] = {};
  for (const k of NODE_EXTRA_COLS) if (nodes.cols[k]) extra[k] = pickCol(nodes.cols[k], idx) as StageNodes['extra'][string];
  return {
    iso3c: idx.map((i) => (nodes.cols.iso3c as Str)[i] as string),
    country: idx.map((i) => (nodes.cols.country as Str)[i]),
    producer_score: Float64Array.from(idx, (i) => (nodes.cols.producer_score as Float64Array)[i]),
    demand_weight: Float64Array.from(idx, (i) => (nodes.cols.demand_weight as Float64Array)[i]),
    dev_potential: Float64Array.from(idx, (i) => (nodes.cols.dev_potential as Float64Array)[i]),
    extra,
  };
}

function stageEdges(edges: Table, idx: number[]): StageEdges {
  return {
    reporter_iso: idx.map((i) => (edges.cols.reporter_iso as Str)[i] as string),
    partner_iso: idx.map((i) => (edges.cols.partner_iso as Str)[i] as string),
    edge_weight: Float64Array.from(idx, (i) => (edges.cols.edge_weight as Float64Array)[i]),
    friendshore_index: Float64Array.from(idx, (i) => (edges.cols.friendshore_index as Float64Array)[i]),
    opportunity_index: Float64Array.from(idx, (i) => (edges.cols.opportunity_index as Float64Array)[i]),
  };
}

function specTable(r: StageResult): Table {
  const s = r.specialization as StageSpecialization;
  const n = s.iso3c.length;
  const cols: Record<string, Column> = {
    tech: new Array(n).fill(r.tech),
    supply_chain: new Array(n).fill(r.supply_chain),
    iso3c: s.iso3c,
    country: s.country,
    producer_score: s.producer_score,
    connectivity_out: s.connectivity_out,
    dev_potential: s.dev_potential,
    demand_weight: s.demand_weight,
    selected: s.selected,
    production_share: s.production_share,
  };
  for (const [k, c] of Object.entries(s.extra)) cols[k] = c;
  return table(cols, n);
}

function flowsTable(r: StageResult): Table {
  const f = r.flows as StageFlows;
  const n = f.reporter_iso.length;
  return table({
    tech: new Array(n).fill(r.tech), supply_chain: new Array(n).fill(r.supply_chain),
    reporter_iso: f.reporter_iso, partner_iso: f.partner_iso, edge_weight: f.edge_weight,
    friendshore_index: f.friendshore_index, opportunity_index: f.opportunity_index, flow_share: f.flow_share,
  }, n);
}

/** Left join of columns of y onto x by one or more key columns (first match). */
function leftJoin(x: Table, y: Table, keys: string[], cols: Record<string, string>): Table {
  const kx = Array.from({ length: x.nrow }, (_, i) => keys.map((k) => String((x.cols[k] as ArrayLike<unknown>)[i])).join('\r'));
  const ym = new Map<string, number>();
  for (let i = 0; i < y.nrow; i++) {
    const k = keys.map((kk) => String((y.cols[kk] as ArrayLike<unknown>)[i])).join('\r');
    if (!ym.has(k)) ym.set(k, i);
  }
  const idx = kx.map((k) => (ym.has(k) ? (ym.get(k) as number) : -1));
  const out: Record<string, Column> = { ...x.cols };
  for (const [src, dst] of Object.entries(cols)) {
    const c = y.cols[src];
    if (Array.isArray(c)) out[dst] = idx.map((i) => (i >= 0 ? c[i] : null));
    else if (c instanceof Float64Array) out[dst] = Float64Array.from(idx, (i) => (i >= 0 ? c[i] : NaN));
    else if (c instanceof Int32Array) out[dst] = Int32Array.from(idx, (i) => (i >= 0 ? c[i] : NA_INT));
    else out[dst] = Uint8Array.from(idx, (i) => (i >= 0 ? (c as Uint8Array)[i] : NA_BOOL));
  }
  return { nrow: x.nrow, cols: out };
}

/** allied_network_disaggregate_shares(): node shares split back to members by stage weight. */
export function disaggregateShares(spec: Table, mi: { iso3c: string[]; tier: Str; network_node: string[]; multi: Set<string> }, mw: Table, countryInfo: Table | null): Table | null {
  if (!spec.nrow) return null;
  const ciIso = countryInfo ? str(countryInfo, 'iso3c') : [];
  const ciName = countryInfo ? str(countryInfo, 'country') : [];
  const memberName = mi.iso3c.map((i) => {
    const k = ciIso.indexOf(i);
    return k >= 0 ? ciName[k] : null;
  });
  const byNode = new Map<string, number[]>();
  mi.network_node.forEach((n, k) => {
    let g = byNode.get(n);
    if (!g) byNode.set(n, (g = []));
    g.push(k);
  });
  const sizes = new Map(Array.from(byNode.entries()).map(([k, v]) => [k, v.length] as [string, number]));
  const mwm = memberWeightMap(mw);
  const T = str(spec, 'tech');
  const S = str(spec, 'supply_chain');
  const N = str(spec, 'iso3c');
  const sel = lgl(spec, 'selected');
  const sh = num(spec, 'production_share');
  type R = { tech: string | null; sc: string | null; iso: string; country: string | null; tier: string | null; node: string; selected: number; nps: number; mw: number; basis: string | null; derived: number; isDerived: number };
  const rows: R[] = [];
  for (let i = 0; i < spec.nrow; i++) {
    const mem = byNode.get(N[i] as string);
    if (!mem) continue;
    for (const k of mem) {
      const w = mwm.get(`${mi.iso3c[k]}\r${T[i]}\r${S[i]}`);
      const mwv = w ? w.w : NaN;
      rows.push({
        tech: T[i], sc: S[i], iso: mi.iso3c[k], country: memberName[k], tier: mi.tier[k], node: N[i] as string, selected: sel[i], nps: sh[i],
        mw: mwv, basis: w ? w.basis : null, derived: sh[i] * mwv, isDerived: (sizes.get(N[i] as string) ?? 0) > 1 ? 1 : 0,
      });
    }
  }
  const ord = orderBy(
    [{ str: rows.map((r) => r.tech) }, { str: rows.map((r) => r.sc) }, { num: rows.map((r) => r.derived), desc: true }, { str: rows.map((r) => r.iso) }],
    rows.length,
  );
  const o = ord.map((i) => rows[i]);
  return table({
    tech: o.map((r) => r.tech), supply_chain: o.map((r) => r.sc), iso3c: o.map((r) => r.iso), country: o.map((r) => r.country),
    tier: o.map((r) => r.tier), network_node: o.map((r) => r.node), selected: Uint8Array.from(o, (r) => r.selected),
    node_production_share: Float64Array.from(o, (r) => r.nps), member_weight: Float64Array.from(o, (r) => r.mw),
    weight_basis: o.map((r) => r.basis), production_share_derived: Float64Array.from(o, (r) => r.derived),
    share_is_derived: Uint8Array.from(o, (r) => r.isDerived),
  }, o.length);
}

/** dash_network_solve() (greedy, all stages, with the portfolio-cap loop). Null when aborted. */
export async function networkSolve(scored: NetScored, params: Params, bundle: Pick<Bundle, 'network'>, opts: SolveOptions = {}): Promise<NetworkSolution | null> {
  const t0 = typeof performance !== 'undefined' ? performance.now() : Date.now();
  const np = params.network;
  const lambda = np.diversity_lambda;
  if (!Number.isFinite(lambda) || lambda < 0) throw new EngineError('NETWORK', 'diversity_lambda must be one finite number >= 0.');
  const shareRankDigits = opts.shareRankDigits === undefined ? SHARE_RANK_DIGITS : opts.shareRankDigits;
  const stages = scored.stages;
  const keys = stages.map((s) => stageKey(s.tech, s.supply_chain));
  const nodes = scored.nodes;
  const edges = scored.edges;
  const nT = str(nodes, 'tech');
  const nS = str(nodes, 'supply_chain');
  const nI = str(nodes, 'iso3c');
  const eT = str(edges, 'tech');
  const eS = str(edges, 'supply_chain');
  const nodesBy = new Map<string, number[]>();
  for (let i = 0; i < nodes.nrow; i++) {
    const k = stageKey(nT[i], nS[i]);
    let g = nodesBy.get(k);
    if (!g) nodesBy.set(k, (g = []));
    g.push(i);
  }
  const edgesBy = new Map<string, number[]>();
  for (let i = 0; i < edges.nrow; i++) {
    const k = stageKey(eT[i], eS[i]);
    let g = edgesBy.get(k);
    if (!g) edgesBy.set(k, (g = []));
    g.push(i);
  }
  const forbidden: Record<string, string[]> = {};
  for (const k of keys) forbidden[k] = [];
  if (opts.forbidden) for (const [k, v] of Object.entries(opts.forbidden)) if (k in forbidden) forbidden[k] = [...v];

  const isoNodes = scored.inputs.nodes;
  const pf = np.portfolio;
  const portfolioEnable = pf.enable === true;
  let capByIso = new Map<string, number>();
  let capsTbl: Table | null = null;
  if (portfolioEnable) {
    const caps = bundle.network?.caps;
    if (!caps) throw new EngineError('NETWORK', 'Portfolio caps need the precomputed node caps (network chunk).');
    const cI = str(caps, 'iso3c');
    const cC = caps.cols.portfolio_cap as Int32Array | Float64Array;
    capByIso = new Map(cI.map((k, i) => [k as string, Number(cC[i])] as [string, number]));
    // caps_tbl: nodes in order, cap, and the gdp columns of distinct node rows (left join)
    const gdp = num(nodes, 'gdp_usd');
    const gimp = lgl(nodes, 'gdp_imputed');
    const seen = new Set<string>();
    const dI: string[] = [];
    const dG: number[] = [];
    const dM: number[] = [];
    for (let i = 0; i < nodes.nrow; i++) {
      const k = `${nI[i]}\r${gdp[i]}\r${gimp[i]}`;
      if (seen.has(k)) continue;
      seen.add(k);
      dI.push(nI[i] as string);
      dG.push(gdp[i]);
      dM.push(gimp[i]);
    }
    const ci: string[] = [];
    const cc: number[] = [];
    const cg: number[] = [];
    const cm: number[] = [];
    for (const iso of isoNodes) {
      const matches = dI.map((x, q) => (x === iso ? q : -1)).filter((q) => q >= 0);
      const cap = capByIso.has(iso) ? (capByIso.get(iso) as number) : NA_INT;
      if (!matches.length) {
        ci.push(iso);
        cc.push(cap);
        cg.push(NaN);
        cm.push(NA_BOOL);
      } else {
        for (const q of matches) {
          ci.push(iso);
          cc.push(cap);
          cg.push(dG[q]);
          cm.push(dM[q]);
        }
      }
    }
    capsTbl = table({ iso3c: ci, portfolio_cap: Int32Array.from(cc), gdp_usd: Float64Array.from(cg), gdp_imputed: Uint8Array.from(cm) });
  }

  const solveOne = (i: number): StageResult => {
    const st = stages[i];
    const key = keys[i];
    const nidx = nodesBy.get(key) ?? [];
    const skipped = (reason: string): StageResult => ({
      tech: st.tech, supply_chain: st.supply_chain, specialization: null, flows: null, objective: NaN, hhi: NaN,
      n_producers: null, method_used: reason, fallback_reason: null, max_production_share: NaN,
    });
    if (new Set(nidx.map((q) => nI[q])).size < 3) return skipped('skipped_too_few_nodes');
    const ps = nodes.cols.producer_score as Float64Array;
    if (!nidx.some((q) => Number.isFinite(ps[q]))) return skipped('skipped_no_node_signal');
    const sn = stageNodes(nodes, nidx);
    const se = stageEdges(edges, edgesBy.get(key) ?? []);
    const fb = forbidden[key];
    const sol = solveStageGreedy(sn, se, {
      producer_forbidden: fb, min_producers: np.min_producers, max_share: np.max_share, min_share: np.min_share,
      min_suppliers_per_consumer: np.min_suppliers_per_consumer, epsilon_supplier_share: np.epsilon_supplier_share,
      w_edge: np.w_edge, w_node: np.w_node, w_dev: np.w_dev, allow_self: np.allow_self, diversity_lambda: lambda,
    });
    const shares = sol.specialization.production_share;
    const red = supplierStats(sol.flows, sol.specialization);
    const terms = objectiveTerms(sol.specialization, sol.flows, np.w_node, np.w_edge, np.w_dev, lambda);
    let mx = -Infinity;
    let nAt = 0;
    for (const v of shares) {
      if (v !== v) continue;
      if (v > mx) mx = v;
      if (v >= np.max_share - 1e-6) nAt++;
    }
    return {
      tech: st.tech, supply_chain: st.supply_chain, specialization: sol.specialization, flows: sol.flows,
      objective: sol.objective, hhi: sol.hhi, n_producers: sol.n_producers, method_used: 'greedy', fallback_reason: null,
      max_production_share: shares.length ? mx : NaN, n_at_max_share: nAt,
      objective_node: terms.node, objective_edge: terms.edge, objective_dev: terms.dev, diversity_penalty: terms.diversity_penalty,
      hhi_tangent_max_error: NaN, min_suppliers_realized: red.min_suppliers, min_supplier_share_realized: red.min_supplier_share,
      producers_forbidden: [...fb].sort(cmpC).join(' '),
    };
  };

  const yieldOk = opts.yieldBetweenStages !== false;
  const pause = async () => {
    if (yieldOk) await yieldNow();
    return opts.shouldAbort?.() === true;
  };
  const results: StageResult[] = new Array(stages.length);
  for (let i = 0; i < stages.length; i++) {
    opts.onProgress?.({ event: 'start_stage', current: i + 1, total: stages.length, tech: stages[i].tech, supply_chain: stages[i].supply_chain, phase: 'initial', iter: 0 });
    results[i] = solveOne(i);
    opts.onProgress?.({ event: 'end_stage', current: i + 1, total: stages.length, tech: stages[i].tech, supply_chain: stages[i].supply_chain, phase: 'initial', iter: 0 });
    if (await pause()) return null;
  }
  const bindSpec = () => bindRows(results.filter((r) => r.specialization).map(specTable));

  let converged: boolean | null = true;
  let iters = 0;
  let violations = 0;
  let counts: Table | null = null;
  let topk: Table = table({});
  const runCaps = portfolioEnable;
  if (runCaps) {
    for (let iter = 1; iter <= pf.max_iters; iter++) {
      iters = iter;
      topk = buildTopkTbl(bindSpec(), pf.top_k, shareRankDigits);
      counts = countTopk(topk, isoNodes, capByIso);
      const cIso = str(counts, 'iso3c');
      const over = counts.cols.over_by as Int32Array;
      const offenders: number[] = [];
      for (let i = 0; i < counts.nrow; i++) if (over[i] !== NA_INT && over[i] > 0) offenders.push(i);
      if (!offenders.length) {
        converged = true;
        break;
      }
      converged = false;
      const impacted: number[] = [];
      const tT = str(topk, 'tech');
      const tS = str(topk, 'supply_chain');
      const tI = str(topk, 'iso3c');
      const tIn = lgl(topk, 'in_top_k');
      const tShare = num(topk, 'production_share');
      const tScore = topk.cols.producer_score ? num(topk, 'producer_score') : null;
      for (const r of offenders) {
        const iso = cIso[r] as string;
        const by = over[r];
        const seen = new Set<string>();
        const cand: number[] = [];
        for (let i = 0; i < topk.nrow; i++) {
          if (tIn[i] !== 1 || tI[i] !== iso) continue;
          const k = stageKey(tT[i], tS[i]);
          if (seen.has(k)) continue;
          seen.add(k);
          cand.push(i);
        }
        if (!cand.length) continue;
        const prio = cand.map((i) => {
          const sc = tScore ? tScore[i] : NaN;
          return tShare[i] * (sc !== sc ? 1 : sc);
        });
        const ord = orderBy([{ num: prio }], cand.length);
        for (const q of ord.slice(0, by)) {
          const i = cand[q];
          const sk = stageKey(tT[i], tS[i]);
          if (!forbidden[sk]) forbidden[sk] = [];
          if (!forbidden[sk].includes(iso)) forbidden[sk].push(iso);
          const si = keys.indexOf(sk);
          if (!impacted.includes(si)) impacted.push(si);
        }
      }
      if (!impacted.length) break;
      for (let q = 0; q < impacted.length; q++) {
        const idx = impacted[q];
        if (idx < 0) continue;
        opts.onProgress?.({ event: 'start_stage', current: q + 1, total: impacted.length, tech: stages[idx].tech, supply_chain: stages[idx].supply_chain, phase: 'portfolio', iter });
        results[idx] = solveOne(idx);
        if (await pause()) return null;
      }
    }
    topk = buildTopkTbl(bindSpec(), pf.top_k, shareRankDigits);
    counts = countTopk(topk, isoNodes, capByIso);
    const over = counts.cols.over_by as Int32Array;
    violations = 0;
    for (const v of over) if (v !== NA_INT && v > 0) violations++;
    if (violations === 0) converged = true;
  }

  let spec = bindSpec();
  const flows = bindRows(results.filter((r) => r.flows).map(flowsTable));
  // diagnostics
  const nr = results.length;
  const diag: Record<string, Column> = {
    tech: results.map((r) => r.tech),
    supply_chain: results.map((r) => r.supply_chain),
    method: results.map((r) => r.method_used),
    method_requested: new Array(nr).fill('greedy'),
    fallback_reason: results.map((r) => r.fallback_reason),
    producers_forbidden: results.map((r) => (r.producers_forbidden === undefined ? null : r.producers_forbidden)),
    n_producers: Int32Array.from(results, (r) => (r.n_producers === null ? NA_INT : r.n_producers)),
    hhi: Float64Array.from(results, (r) => r.hhi),
    objective: Float64Array.from(results, (r) => r.objective),
    max_production_share: Float64Array.from(results, (r) => r.max_production_share),
    min_suppliers_realized: Int32Array.from(results, (r) => (r.min_suppliers_realized === undefined || r.min_suppliers_realized === null ? NA_INT : r.min_suppliers_realized)),
    min_supplier_share_realized: Float64Array.from(results, (r) => (r.min_supplier_share_realized === undefined ? NaN : r.min_supplier_share_realized)),
    diversity_lambda: new Float64Array(nr).fill(lambda),
    node_policy: new Uint8Array(nr).fill(np.node_policy ? 1 : 0),
    effective_producers: Float64Array.from(results, (r) => 1 / r.hhi),
    n_at_max_share: Int32Array.from(results, (r) => (r.n_at_max_share === undefined ? NA_INT : r.n_at_max_share)),
    objective_node: Float64Array.from(results, (r) => r.objective_node ?? NaN),
    objective_edge: Float64Array.from(results, (r) => r.objective_edge ?? NaN),
    objective_dev: Float64Array.from(results, (r) => r.objective_dev ?? NaN),
    diversity_penalty: Float64Array.from(results, (r) => r.diversity_penalty ?? NaN),
    hhi_tangent_max_error: Float64Array.from(results, (r) => r.hhi_tangent_max_error ?? NaN),
  };
  // stage flags from the stage's nodes
  const ps = nodes.cols.producer_score as Float64Array;
  const ncomp = nodes.cols.node_components_n as Int32Array;
  const pimp = nodes.cols.policy_imputed as Uint8Array;
  const dmiss = nodes.cols.demand_need_missing as Uint8Array;
  const flag = (f: (idx: number[]) => number) => Int32Array.from(keys, (k) => {
    const idx = nodesBy.get(k);
    return idx ? f(idx) : NA_INT;
  });
  diag.n_nodes = flag((idx) => new Set(idx.map((i) => nI[i])).size);
  diag.n_nodes_no_score = flag((idx) => idx.filter((i) => ps[i] !== ps[i]).length);
  diag.n_nodes_partial_score = flag((idx) => idx.filter((i) => ncomp[i] >= 1 && ncomp[i] <= 2).length);
  diag.n_policy_imputed = flag((idx) => idx.filter((i) => pimp[i] === 1).length);
  diag.n_demand_need_missing = flag((idx) => idx.filter((i) => dmiss[i] === 1).length);
  if (portfolioEnable && spec.nrow) {
    spec = leftJoin(spec, counts as Table, ['iso3c'], { topk_count: 'portfolio_topk_count', portfolio_cap: 'portfolio_cap' });
    diag.portfolio_converged = new Uint8Array(nr).fill(runCaps ? (converged ? 1 : 0) : NA_BOOL);
    diag.portfolio_iters = new Int32Array(nr).fill(runCaps ? iters : NA_INT);
    diag.portfolio_violations_remaining = new Int32Array(nr).fill(runCaps ? violations : NA_INT);
  }
  const diagnostics = table(diag, nr);

  // node info and member shares
  const mi = scored.inputs.mi;
  let memberShares: Table | null = null;
  if (spec.nrow) {
    const nodeInfo = mi.aggregate
      ? table({
          iso3c: mi.nodes.map((n) => n.network_node), tier: mi.nodes.map((n) => n.tier),
          network_node: mi.nodes.map((n) => n.network_node), n_members: Int32Array.from(mi.nodes, (n) => n.n_members),
        })
      : table({ iso3c: [...mi.iso3c], tier: [...mi.tier], network_node: [...mi.network_node], n_members: new Int32Array(mi.iso3c.length).fill(1) });
    const joined = leftJoin(
      { nrow: spec.nrow, cols: Object.fromEntries(Object.entries(spec.cols).filter(([k]) => !['tier', 'network_node', 'n_members'].includes(k))) },
      nodeInfo, ['iso3c'], { tier: 'tier', network_node: 'network_node', n_members: 'n_members' },
    );
    const order = Object.keys(joined.cols).filter((k) => !['tier', 'network_node', 'n_members'].includes(k));
    const at = order.indexOf('iso3c') + 1;
    order.splice(at, 0, 'tier', 'network_node', 'n_members');
    spec = { nrow: joined.nrow, cols: Object.fromEntries(order.map((k) => [k, joined.cols[k]])) };
    const shareWeights = mi.aggregate
      ? (scored.inputs.member_weights as Table)
      : table({
          network_node: stages.flatMap(() => mi.network_node), iso3c: stages.flatMap(() => mi.iso3c),
          tech: stages.flatMap((s) => mi.iso3c.map(() => s.tech)), supply_chain: stages.flatMap((s) => mi.iso3c.map(() => s.supply_chain)),
          member_weight: new Float64Array(stages.length * mi.iso3c.length).fill(1), weight_basis: new Array(stages.length * mi.iso3c.length).fill('own'),
        });
    memberShares = disaggregateShares(spec, mi, shareWeights, bundle.network?.country_info ?? null);
  }
  const t1 = typeof performance !== 'undefined' ? performance.now() : Date.now();
  return {
    specialization: spec,
    flows,
    diagnostics,
    portfolio: portfolioEnable
      ? { caps: capsTbl as Table, counts: counts as Table, topk, converged: runCaps ? converged : null, iters: runCaps ? iters : null, violations_remaining: runCaps ? violations : null }
      : null,
    member_shares: memberShares,
    forbidden,
    solver_by_stage: table({ tech: stages.map((s) => s.tech), supply_chain: stages.map((s) => s.supply_chain), solver: results.map((r) => r.method_used) }),
    stages,
    elapsed_ms: t1 - t0,
  };
}

// ---- Compare and concentration ---------------------------------------------------------------------

/** dash_network_compare(): producer sets, concentration and flow turnover against a reference. */
export function networkCompare(
  scenario: Pick<NetworkSolution, 'specialization' | 'flows' | 'diagnostics'>,
  reference: Pick<NetworkSolution, 'specialization' | 'flows' | 'diagnostics'>,
): { stage: Table; node: Table; node_summary: Table } {
  const s = scenario.specialization;
  const r = reference.specialization;
  const keyOf = (t: Table) => {
    const T = str(t, 'tech');
    const S = str(t, 'supply_chain');
    return Array.from({ length: t.nrow }, (_, i) => stageKey(T[i], S[i]));
  };
  const stageList: Stage[] = [];
  const seenSt = new Set<string>();
  for (const d of [scenario.diagnostics, reference.diagnostics]) {
    const T = str(d, 'tech');
    const S = str(d, 'supply_chain');
    for (let i = 0; i < d.nrow; i++) {
      const k = stageKey(T[i], S[i]);
      if (seenSt.has(k)) continue;
      seenSt.add(k);
      stageList.push({ tech: T[i] as string, supply_chain: S[i] as string });
    }
  }
  const sk = keyOf(s);
  const rk = keyOf(r);
  const fsk = keyOf(scenario.flows);
  const frk = keyOf(reference.flows);
  const sI = str(s, 'iso3c');
  const rI = str(r, 'iso3c');
  const sSh = num(s, 'production_share');
  const rSh = num(r, 'production_share');
  const out = {
    tech: [] as Str, supply_chain: [] as Str, n_producers: [] as number[], n_producers_ref: [] as number[], producers_added: [] as Str,
    producers_removed: [] as Str, jaccard: [] as number[], hhi: [] as number[], hhi_ref: [] as number[], hhi_delta: [] as number[],
    max_abs_share_delta: [] as number[], flow_turnover: [] as number[], changed: [] as number[],
  };
  const flowMap = (f: Table, keys: string[], k: string) => {
    const R = str(f, 'reporter_iso');
    const P = str(f, 'partner_iso');
    const V = num(f, 'flow_share');
    const m = new Map<string, number>();
    const order: string[] = [];
    for (let i = 0; i < f.nrow; i++) {
      if (keys[i] !== k) continue;
      const pk = `${R[i]} ${P[i]}`;
      if (!m.has(pk)) {
        m.set(pk, V[i]);
        order.push(pk);
      }
    }
    return { m, order };
  };
  for (const st of stageList) {
    const k = stageKey(st.tech, st.supply_chain);
    const ssIdx = sk.map((x, i) => (x === k ? i : -1)).filter((i) => i >= 0);
    const rrIdx = rk.map((x, i) => (x === k ? i : -1)).filter((i) => i >= 0);
    const psS = ssIdx.filter((i) => sSh[i] > 0).map((i) => sI[i] as string);
    const prS = rrIdx.filter((i) => rSh[i] > 0).map((i) => rI[i] as string);
    const nodesU = Array.from(new Set([...ssIdx.map((i) => sI[i] as string), ...rrIdx.map((i) => rI[i] as string)]));
    const shOf = (idx: number[], I: Str, Sh: Float64Array) => {
      const m = new Map<string, number>();
      for (const i of idx) if (!m.has(I[i] as string)) m.set(I[i] as string, Sh[i]);
      return nodesU.map((x) => {
        const v = m.get(x);
        return v === undefined || v !== v ? 0 : v;
      });
    };
    const shS = shOf(ssIdx, sI, sSh);
    const shR = shOf(rrIdx, rI, rSh);
    const fS = flowMap(scenario.flows, fsk, k);
    const fR = flowMap(reference.flows, frk, k);
    const pk = Array.from(new Set([...fS.order, ...fR.order]));
    const turnover = sumList(pk.map((x) => Math.abs((fS.m.get(x) ?? 0) - (fR.m.get(x) ?? 0)))) / 2;
    const hS = psS.length ? sumList(shS.map((v) => v * v)) : NaN;
    const hR = prS.length ? sumList(shR.map((v) => v * v)) : NaN;
    const setS = new Set(psS);
    const setR = new Set(prS);
    const union = Array.from(new Set([...psS, ...prS]));
    const inter = psS.filter((x, i) => setR.has(x) && psS.indexOf(x) === i);
    let maxAbs = NaN;
    if (nodesU.length) {
      maxAbs = 0;
      for (let i = 0; i < nodesU.length; i++) maxAbs = Math.max(maxAbs, Math.abs(shS[i] - shR[i]));
    }
    const setEq = psS.every((x) => setR.has(x)) && prS.every((x) => setS.has(x));
    out.tech.push(st.tech);
    out.supply_chain.push(st.supply_chain);
    out.n_producers.push(psS.length);
    out.n_producers_ref.push(prS.length);
    out.producers_added.push(Array.from(new Set(psS.filter((x) => !setR.has(x)))).sort(cmpC).join(' '));
    out.producers_removed.push(Array.from(new Set(prS.filter((x) => !setS.has(x)))).sort(cmpC).join(' '));
    out.jaccard.push(union.length ? inter.length / union.length : NaN);
    out.hhi.push(hS);
    out.hhi_ref.push(hR);
    out.hhi_delta.push(hS - hR);
    out.max_abs_share_delta.push(maxAbs);
    out.flow_turnover.push(turnover);
    out.changed.push(!setEq || (nodesU.length > 0 && maxAbs > 1e-9) ? 1 : 0);
  }
  const stage = table({
    tech: out.tech, supply_chain: out.supply_chain, n_producers: Int32Array.from(out.n_producers), n_producers_ref: Int32Array.from(out.n_producers_ref),
    producers_added: out.producers_added, producers_removed: out.producers_removed, jaccard: Float64Array.from(out.jaccard),
    hhi: Float64Array.from(out.hhi), hhi_ref: Float64Array.from(out.hhi_ref), hhi_delta: Float64Array.from(out.hhi_delta),
    max_abs_share_delta: Float64Array.from(out.max_abs_share_delta), flow_turnover: Float64Array.from(out.flow_turnover),
    changed: Uint8Array.from(out.changed),
  });
  // node table: full join on (tech, supply_chain, iso3c)
  const sT = str(s, 'tech');
  const sS = str(s, 'supply_chain');
  const rT = str(r, 'tech');
  const rS = str(r, 'supply_chain');
  const nk = (T: Str, S: Str, I: Str, i: number) => `${T[i]}\r${S[i]}\r${I[i]}`;
  const rMap = new Map<string, number[]>();
  for (let i = 0; i < r.nrow; i++) {
    const k = nk(rT, rS, rI, i);
    let g = rMap.get(k);
    if (!g) rMap.set(k, (g = []));
    g.push(i);
  }
  const nodeRows: { t: string | null; s: string | null; i: string | null; a: number; b: number }[] = [];
  const usedR = new Set<number>();
  for (let i = 0; i < s.nrow; i++) {
    const m = rMap.get(nk(sT, sS, sI, i));
    if (m) {
      for (const j of m) {
        usedR.add(j);
        nodeRows.push({ t: sT[i], s: sS[i], i: sI[i], a: sSh[i], b: rSh[j] });
      }
    } else nodeRows.push({ t: sT[i], s: sS[i], i: sI[i], a: sSh[i], b: NaN });
  }
  for (let j = 0; j < r.nrow; j++) if (!usedR.has(j)) nodeRows.push({ t: rT[j], s: rS[j], i: rI[j], a: NaN, b: rSh[j] });
  const co = (v: number) => (v !== v ? 0 : v);
  const node = table({
    tech: nodeRows.map((x) => x.t), supply_chain: nodeRows.map((x) => x.s), iso3c: nodeRows.map((x) => x.i),
    production_share: Float64Array.from(nodeRows, (x) => co(x.a)), production_share_ref: Float64Array.from(nodeRows, (x) => co(x.b)),
    share_delta: Float64Array.from(nodeRows, (x) => co(x.a) - co(x.b)),
  });
  const byIso = new Map<string, number[]>();
  nodeRows.forEach((x, q) => {
    let g = byIso.get(x.i as string);
    if (!g) byIso.set(x.i as string, (g = []));
    g.push(q);
  });
  const isoSorted = Array.from(byIso.keys()).sort(cmpC);
  const sums = isoSorted.map((iso) => {
    const g = byIso.get(iso) as number[];
    const a = g.map((q) => co(nodeRows[q].a));
    const b = g.map((q) => co(nodeRows[q].b));
    return {
      iso,
      sp: a.filter((v) => v > 0).length,
      spr: b.filter((v) => v > 0).length,
      ts: sumList(a),
      tsr: sumList(b),
      tsd: sumList(a.map((v, k) => v - b[k])),
    };
  });
  const ord = orderBy([{ num: sums.map((x) => Math.abs(x.tsd)), desc: true }], sums.length);
  const so = ord.map((i) => sums[i]);
  const node_summary = table({
    iso3c: so.map((x) => x.iso), stages_producing: Int32Array.from(so, (x) => x.sp), stages_producing_ref: Int32Array.from(so, (x) => x.spr),
    total_share: Float64Array.from(so, (x) => x.ts), total_share_ref: Float64Array.from(so, (x) => x.tsr), total_share_delta: Float64Array.from(so, (x) => x.tsd),
  }, so.length);
  return { stage, node, node_summary };
}

/** dash_network_concentration(): designed concentration against the reference and the observed HHI. */
export function networkConcentration(solution: Pick<NetworkSolution, 'diagnostics'>, bundle: Pick<Bundle, 'network'>, reference?: Pick<NetworkSolution, 'diagnostics'> | null): Table {
  const d = solution.diagnostics;
  let t: Table = table({
    tech: str(d, 'tech'), supply_chain: str(d, 'supply_chain'), hhi_designed: num(d, 'hhi'),
    effective_producers: num(d, 'effective_producers'), n_producers: d.cols.n_producers, method: str(d, 'method'),
  }, d.nrow);
  if (reference) {
    t = leftJoin(t, reference.diagnostics, ['tech', 'supply_chain'], { hhi: 'hhi_baseline' });
  } else {
    t.cols.hhi_baseline = new Float64Array(d.nrow).fill(NaN);
  }
  const obs = bundle.network?.observed_hhi;
  if (obs && obs.nrow) {
    t = leftJoin(t, obs, ['tech', 'supply_chain'], { hhi: 'hhi_observed', effective_exporters: 'effective_exporters', top_node: 'top_node', top_share: 'top_share' });
  } else {
    t.cols.hhi_observed = new Float64Array(d.nrow).fill(NaN);
  }
  void colOf;
  return t;
}
