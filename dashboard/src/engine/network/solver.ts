// The greedy stage solver (port of dash_solve_stage_greedy(), dash_ras_fast(),
// allied_network_prune_small_flows(), allied_network_prune_small_flows_stepwise(),
// allied_network_cap_shares(), allied_network_waterfill(), allied_network_diversified_shares(),
// dash_supplier_stats(), allied_network_objective_terms() and allied_network_hhi();
// spec_react.md 6.6). Pure. Matrices are column-major Float64Arrays.

import { cmpC, EngineError, LdAcc, ldFinishF64, ldSumF64, medianR, orderBy, orderNum, sumList } from '../numeric.ts';

// ---- Long-double row / column sums of a matrix (R's .rowSums / .colSums) ---------------------------

class Sums {
  constructor(_n: number) {
    void _n;
  }
  /** rowSums(): each row's long-double sum over the columns in order. */
  rows(f: Float64Array, nr: number, nc: number, out: Float64Array): Float64Array {
    for (let i = 0; i < nr; i++) out[i] = ldSumF64(f, i, nr, nc);
    return out;
  }
  /** colSums(). */
  cols(f: Float64Array, nr: number, nc: number, out: Float64Array): Float64Array {
    for (let j = 0; j < nc; j++) out[j] = ldSumF64(f, j * nr, 1, nr);
    return out;
  }
}

/**
 * dash_ras_fast(): biproportional fit of K to the row and column targets. Each iteration takes the
 * row sums, scales the rows, takes the column sums, scales the columns and stops when the row sums
 * are within tol of the targets; the row sums of that check are the next iteration's (f has not
 * changed in between), so they are computed once.
 *
 * The passes are fused per column (same operations, same order): column j is scaled by the row
 * factors, summed (colSums), scaled by its own factor, and its values are added to the running row
 * sums (rowSums accumulates column by column), using the certified long-double kernels of
 * numeric.ts.
 */
export function ras(K: Float64Array, nr: number, nc: number, rt: Float64Array, ct: Float64Array, maxIter = 2000, tol = 1e-12): Float64Array {
  const f = Float64Array.from(K);
  const rs = new Float64Array(nr);
  const v = new Float64Array(nr);
  const rS = new Float64Array(nr);
  const rC = new Float64Array(nr);
  const rA = new Float64Array(nr);
  for (let i = 0; i < nr; i++) rs[i] = ldSumF64(f, i, nr, nc);
  for (let iter = 0; iter < maxIter; iter++) {
    for (let i = 0; i < nr; i++) {
      v[i] = rs[i] > 0 ? rt[i] / rs[i] : 0;
      rS[i] = 0;
      rC[i] = 0;
      rA[i] = 0;
    }
    for (let j = 0; j < nc; j++) {
      const off = j * nr;
      // f[, j] * v and its long-double sum
      let s = 0;
      let c = 0;
      let a = 0;
      for (let i = 0; i < nr; i++) {
        const x = f[off + i] * v[i];
        f[off + i] = x;
        const t = s + x;
        const z = t - s;
        c += s - (t - z) + (x - z);
        s = t;
        if (x !== 0) a += t < 0 ? -t : t;
      }
      const csj = ldFinishF64(s, c, a, f, off, 1, nr);
      const uj = csj > 0 ? ct[j] / csj : 0;
      // f[, j] * u_j, added to the row sums
      for (let i = 0; i < nr; i++) {
        const y = f[off + i] * uj;
        f[off + i] = y;
        const si = rS[i];
        const t = si + y;
        const z = t - si;
        rC[i] += si - (t - z) + (y - z);
        rS[i] = t;
        if (y !== 0) rA[i] += t < 0 ? -t : t;
      }
    }
    for (let i = 0; i < nr; i++) rs[i] = ldFinishF64(rS[i], rC[i], rA[i], f, i, nr, nc);
    let mx = 0;
    for (let i = 0; i < nr; i++) {
      const d = Math.abs(rs[i] - rt[i]);
      if (d !== d) {
        mx = NaN;
        break;
      }
      if (d > mx) mx = d;
    }
    if (mx < tol) break;
  }
  return f;
}

function maxAbsDiff(a: Float64Array, b: Float64Array): number {
  let mx = 0;
  for (let i = 0; i < a.length; i++) {
    const d = Math.abs(a[i] - b[i]);
    if (d !== d) return NaN;
    if (d > mx) mx = d;
  }
  return mx;
}

function fitOk(f: Float64Array, nr: number, nc: number, rt: Float64Array, ct: Float64Array, tol: number): boolean {
  const s = new Sums(Math.max(nr, nc));
  const rs = s.rows(f, nr, nc, new Float64Array(nr));
  const cs = s.cols(f, nr, nc, new Float64Array(nc));
  const a = maxAbsDiff(rs, rt);
  const b = maxAbsDiff(cs, ct);
  return !(a > tol) && !(b > tol) && a === a && b === b;
}

/** allied_network_prune_small_flows(). */
export function pruneSmallFlows(
  f0: Float64Array, K0: Float64Array, nr: number, nc: number, rt: Float64Array, ct: Float64Array, minSuppliers = 2, epsilon = 0.1, maxPasses = 5, tol = 1e-6,
): Float64Array {
  if (!Number.isFinite(epsilon) || epsilon <= 0) return f0;
  let f = f0;
  let K = K0;
  for (let pass = 0; pass < maxPasses; pass++) {
    const rel = new Float64Array(nr * nc);
    const small = new Uint8Array(nr * nc);
    let any = false;
    for (let j = 0; j < nc; j++) {
      const d = ct[j] > 0 ? ct[j] : 1;
      for (let i = 0; i < nr; i++) {
        const p = i + nr * j;
        rel[p] = f[p] / d;
        small[p] = f[p] > 0 && rel[p] < epsilon - 1e-9 ? 1 : 0;
        if (small[p]) any = true;
      }
    }
    if (!any) break;
    const Kn = Float64Array.from(K);
    for (let j = 0; j < nc; j++) {
      let nSmall = 0;
      let nPos = 0;
      const cand: number[] = [];
      for (let i = 0; i < nr; i++) {
        const p = i + nr * j;
        if (small[p]) {
          nSmall++;
          cand.push(i);
        }
        if (f[p] > 0) nPos++;
      }
      if (!nSmall) continue;
      const nDrop = Math.min(nSmall, nPos - minSuppliers);
      if (nDrop <= 0) continue;
      const ord = orderNum(cand.map((i) => rel[i + nr * j]));
      for (let q = 0; q < nDrop; q++) Kn[cand[ord[q]] + nr * j] = 0;
    }
    let same = true;
    for (let p = 0; p < Kn.length; p++) {
      if (Kn[p] !== K[p]) {
        same = false;
        break;
      }
    }
    if (same) break;
    const fn = ras(Kn, nr, nc, rt, ct);
    if (!fitOk(fn, nr, nc, rt, ct, tol)) break;
    K = Kn;
    f = fn;
  }
  return f;
}

/** allied_network_prune_small_flows_stepwise(). */
export function pruneSmallFlowsStepwise(
  f0: Float64Array, K0: Float64Array, nr: number, nc: number, rt: Float64Array, ct: Float64Array, minSuppliers = 2, epsilon = 0.1, tol = 1e-6, maxRefits = 2000,
): Float64Array {
  if (!Number.isFinite(epsilon) || epsilon <= 0) return f0;
  let f = f0;
  let K = Float64Array.from(K0, (k, p) => k * (f0[p] > 0 ? 1 : 0));
  let refits = 0;
  for (;;) {
    const posCol = new Int32Array(nc);
    for (let j = 0; j < nc; j++) for (let i = 0; i < nr; i++) if (f[i + nr * j] > 0) posCol[j]++;
    const small: number[] = [];
    const rels: number[] = [];
    for (let j = 0; j < nc; j++) {
      const d = ct[j] > 0 ? ct[j] : 1;
      for (let i = 0; i < nr; i++) {
        const p = i + nr * j;
        const rel = f[p] / d;
        if (f[p] > 0 && rel < epsilon - 1e-9 && posCol[j] > minSuppliers) {
          small.push(p);
          rels.push(rel);
        }
      }
    }
    if (!small.length) break;
    // candidates in order(rel) (ascending, ties in position order), picked lazily: the first refit
    // usually succeeds, so a full sort is wasted work
    const tried = new Uint8Array(small.length);
    let dropped = false;
    for (let t = 0; t < small.length; t++) {
      let q = -1;
      for (let r = 0; r < small.length; r++) if (!tried[r] && (q < 0 || rels[r] < rels[q])) q = r;
      tried[q] = 1;
      if (refits >= maxRefits) break;
      const Kt = Float64Array.from(K);
      Kt[small[q]] = 0;
      const ft = ras(Kt, nr, nc, rt, ct);
      refits++;
      if (fitOk(ft, nr, nc, rt, ct, tol)) {
        K = Kt;
        f = ft;
        dropped = true;
        break;
      }
    }
    if (!dropped) break;
  }
  return f;
}

/** allied_network_cap_shares(). */
export function capShares(shares0: ArrayLike<number>, minShare0: number, maxShare0: number): Float64Array {
  const n = shares0.length;
  let s = Float64Array.from(shares0);
  if (n === 0) return s;
  const minShare = Math.max(0, Math.min(minShare0, 1 / n));
  const maxShare = Math.max(minShare, Math.min(maxShare0, 1));
  const sumAll = (x: Float64Array) => sumList(Array.from(x).filter((v) => v === v));
  if (sumAll(s) <= 0) s = new Float64Array(n).fill(1 / n);
  {
    const t = sumList(Array.from(s));
    s = s.map((v) => v / t);
  }
  for (let iter = 0; iter < 50; iter++) {
    const over: number[] = [];
    for (let i = 0; i < n; i++) if (s[i] > maxShare + 1e-10) over.push(i);
    if (!over.length) break;
    const excess = sumList(over.map((i) => s[i] - maxShare));
    for (const i of over) s[i] = maxShare;
    const overSet = new Set(over);
    const under: number[] = [];
    for (let i = 0; i < n; i++) if (!overSet.has(i)) under.push(i);
    if (!under.length) break;
    const su = sumList(under.map((i) => s[i]));
    const vals = under.map((i) => s[i] + excess * (s[i] / su));
    under.forEach((i, k) => (s[i] = vals[k]));
  }
  for (let iter = 0; iter < 50; iter++) {
    const under: number[] = [];
    for (let i = 0; i < n; i++) if (s[i] < minShare - 1e-10) under.push(i);
    if (!under.length) break;
    const deficit = sumList(under.map((i) => minShare - s[i]));
    for (const i of under) s[i] = minShare;
    const uSet = new Set(under);
    const over: number[] = [];
    for (let i = 0; i < n; i++) if (!uSet.has(i)) over.push(i);
    if (!over.length) break;
    const so = sumList(over.map((i) => s[i]));
    const vals = over.map((i) => Math.max(0, s[i] - deficit * (s[i] / so)));
    over.forEach((i, k) => (s[i] = vals[k]));
    if (sumAll(s) <= 0) {
      s = new Float64Array(n).fill(1 / n);
      break;
    }
    const t = sumList(Array.from(s));
    s = s.map((v) => v / t);
  }
  const t = sumList(Array.from(s));
  return s.map((v) => v / t);
}

/** allied_network_waterfill(): shares maximising sum v x - lambda sum x^2 in [lo, hi], sum 1. */
export function waterfill(v: number[], lambda: number, lo: number, hi: number): Float64Array | null {
  const k = v.length;
  if (!k || !Number.isFinite(lambda) || lambda <= 0 || k * lo > 1 + 1e-12 || k * hi < 1 - 1e-12) return null;
  const shareAt = (mu: number) => v.map((x) => Math.min(hi, Math.max(lo, (x - mu) / (2 * lambda))));
  let loMu = Math.min(...v) - 2 * lambda * hi;
  let hiMu = Math.max(...v) - 2 * lambda * lo;
  for (let iter = 0; iter < 100; iter++) {
    const mid = (loMu + hiMu) / 2;
    if (sumList(shareAt(mid)) > 1) loMu = mid;
    else hiMu = mid;
  }
  const x = shareAt((loMu + hiMu) / 2);
  const s = sumList(x);
  return Float64Array.from(x, (y) => y / s);
}

/** allied_network_diversified_shares(): the best k >= n_min first candidates with water-filled shares. */
export function diversifiedShares(v: number[], bonus: number[], lambda: number, minShare: number, maxShare: number, nMin: number): { k: number; shares: Float64Array; value: number } | null {
  let best: { k: number; shares: Float64Array; value: number } | null = null;
  const from = Math.max(1, nMin);
  const to = v.length;
  const ks: number[] = [];
  if (from <= to) for (let k = from; k <= to; k++) ks.push(k);
  else for (let k = from; k >= to; k--) ks.push(k);
  for (const k of ks) {
    const vk = v.slice(0, k);
    if (vk.length < k || vk.some((x) => x !== x)) throw new EngineError('NETWORK', 'diversified shares: fewer candidates than producers.');
    const x = waterfill(vk, lambda, minShare, maxShare);
    if (!x) continue;
    const val = sumList(vk.map((vi, i) => vi * x[i])) + sumList(bonus.slice(0, k)) - lambda * sumList(Array.from(x, (y) => y * y));
    if (best === null || val > best.value + 1e-12) best = { k, shares: x, value: val };
  }
  return best;
}

/** allied_network_hhi(): sum of the squared finite shares (NA when none). */
export function hhi(shares: ArrayLike<number>): number {
  const s: number[] = [];
  for (let i = 0; i < shares.length; i++) if (Number.isFinite(shares[i])) s.push(shares[i] * shares[i]);
  return s.length ? sumList(s) : NaN;
}

export const NODE_EXTRA_COLS = [
  'gdp_usd', 'gdp_imputed', 'size_idx', 'need_idx', 'demand_need_component', 'demand_size_component',
  'policy_imputed', 'node_components_n', 'demand_need_missing',
] as const;

export interface StageNodes {
  iso3c: string[];
  country: (string | null)[];
  producer_score: Float64Array;
  demand_weight: Float64Array;
  dev_potential: Float64Array;
  extra: Record<string, Float64Array | Int32Array | Uint8Array | (string | null)[]>;
}

export interface StageEdges {
  reporter_iso: string[];
  partner_iso: string[];
  edge_weight: Float64Array;
  friendshore_index: Float64Array;
  opportunity_index: Float64Array;
}

export interface GreedyOptions {
  producer_forbidden: string[];
  min_producers: number;
  max_share: number;
  min_share: number;
  min_suppliers_per_consumer: number;
  epsilon_supplier_share: number;
  w_edge: number;
  w_node: number;
  w_dev: number;
  allow_self: boolean;
  diversity_lambda: number;
}

export interface StageSpecialization {
  iso3c: string[];
  country: (string | null)[];
  producer_score: Float64Array;
  connectivity_out: Float64Array;
  dev_potential: Float64Array;
  demand_weight: Float64Array;
  selected: Uint8Array;
  production_share: Float64Array;
  extra: Record<string, Float64Array | Int32Array | Uint8Array | (string | null)[]>;
}

export interface StageFlows {
  reporter_iso: string[];
  partner_iso: string[];
  edge_weight: Float64Array;
  friendshore_index: Float64Array;
  opportunity_index: Float64Array;
  flow_share: Float64Array;
}

export interface StageSolution {
  specialization: StageSpecialization;
  flows: StageFlows;
  objective: number;
  hhi: number;
  n_producers: number;
}

function pick<T extends Float64Array | Int32Array | Uint8Array | (string | null)[]>(x: T, idx: number[]): T {
  if (Array.isArray(x)) return idx.map((i) => x[i]) as T;
  const Ctor = x.constructor as { new (n: number): T };
  const out = new Ctor(idx.length) as Float64Array;
  idx.forEach((i, q) => (out[q] = (x as Float64Array)[i]));
  return out as unknown as T;
}

/** dash_solve_stage_greedy(). */
export function solveStageGreedy(nodes: StageNodes, edges: StageEdges, o: GreedyOptions): StageSolution {
  const lambda = o.diversity_lambda;
  if (!Number.isFinite(lambda) || lambda < 0) throw new EngineError('NETWORK', 'diversity_lambda must be one finite number >= 0.');
  const iso = nodes.iso3c;
  const n = iso.length;
  if (new Set(iso).size !== n) throw new EngineError('NETWORK', 'dash_solve_stage_greedy: nodes_stage must have one row per iso3c.');
  const forb = new Set(o.producer_forbidden);
  let eligible = iso.filter((x, i) => !forb.has(x) && iso.indexOf(x) === i);
  if (!eligible.length) eligible = [...iso];
  const minRequired = Math.ceil(1 / o.max_share);
  const targetProd = Math.max(o.min_producers, minRequired, o.min_suppliers_per_consumer);
  const nProd = Math.min(targetProd, eligible.length);

  // completed edge matrix over the stage's nodes, rows / columns in C-sorted order
  const so = Array.from(new Set(iso)).sort(cmpC);
  const soIdx = new Map(so.map((x, i) => [x, i] as [string, number]));
  const Wn = new Float64Array(n * n).fill(NaN);
  const FSm = new Float64Array(n * n).fill(NaN);
  const OPm = new Float64Array(n * n).fill(NaN);
  const seen = new Set<string>();
  for (let e = 0; e < edges.reporter_iso.length; e++) {
    const k = `${edges.reporter_iso[e]} ${edges.partner_iso[e]}`;
    if (seen.has(k)) throw new EngineError('NETWORK', 'dash_solve_stage_greedy: duplicate (reporter_iso, partner_iso) edges; collapse edges first.');
    seen.add(k);
    const ri = soIdx.get(edges.reporter_iso[e]);
    const pi = soIdx.get(edges.partner_iso[e]);
    if (ri === undefined || pi === undefined) continue;
    const p = ri + n * pi;
    Wn[p] = edges.edge_weight[e];
    FSm[p] = edges.friendshore_index[e];
    OPm[p] = edges.opportunity_index[e];
  }
  let med = medianR(Wn);
  if (!Number.isFinite(med)) med = 0.5;
  const E = Float64Array.from(Wn, (v) => (v !== v ? 0 : v));
  for (let i = 0; i < n; i++) if (Wn[i + n * i] !== Wn[i + n * i]) E[i + n * i] = med;

  const isoIdx = new Map(iso.map((x, i) => [x, i] as [string, number]));
  const dSorted = Float64Array.from(so, (x) => nodes.demand_weight[isoIdx.get(x) as number]);
  const connSorted = new Float64Array(n);
  {
    // rowSums(E * rep(d_sorted, each = n), na.rm = TRUE)
    const hiA = new Float64Array(n);
    const loA = new Float64Array(n);
    const acc = new LdAcc();
    for (let j = 0; j < n; j++) {
      for (let i = 0; i < n; i++) {
        const v = E[i + n * j] * dSorted[j];
        if (v !== v) continue;
        acc.hi = hiA[i];
        acc.lo = loA[i];
        acc.add(v);
        hiA[i] = acc.hi;
        loA[i] = acc.lo;
      }
    }
    connSorted.set(hiA);
  }
  let maxW = 0;
  for (const v of E) if (v > maxW) maxW = v;
  const sumD = sumList(Array.from(nodes.demand_weight).filter((v) => v === v));
  for (let i = 0; i < n; i++) {
    if (connSorted[i] > maxW * sumD + 1e-9) {
      throw new EngineError('NETWORK', 'allied_network_connectivity_out: connectivity exceeds the largest edge weight; edges or demand are duplicated.');
    }
  }
  const ps = nodes.producer_score;
  const dev = nodes.dev_potential;
  const conn0 = Float64Array.from(iso, (x) => {
    const c = connSorted[soIdx.get(x) as number];
    return c !== c ? 0 : c;
  });
  const co = (v: number) => (v !== v ? 0 : v);
  const combined = Float64Array.from(iso, (_, i) => o.w_node * co(ps[i]) + o.w_edge * conn0[i] + o.w_dev * co(dev[i]));
  const ord = orderBy(
    [
      { num: Float64Array.from(ps, (v) => (v !== v ? 1 : 0)) },
      { num: combined, desc: true },
      { num: ps, desc: true },
      { num: conn0, desc: true },
      { str: iso },
    ],
    n,
  );
  const candIso = ord.map((i) => iso[i]);

  const dw = nodes.demand_weight;
  const dPos = new Map<string, boolean>();
  iso.forEach((x, i) => dPos.set(x, co(dw[i]) > 0));
  if (!Array.from(dPos.values()).some(Boolean)) for (const k of dPos.keys()) dPos.set(k, true);
  const reach = new Set<string>();
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (!o.allow_self && i === j) continue;
      if (E[i + n * j] > 0 && dPos.get(so[j])) {
        reach.add(so[i]);
        break;
      }
    }
  }
  const eligSet = new Set(eligible);
  let pool = Array.from(new Set(candIso.filter((x) => eligSet.has(x))));
  pool = [...pool.filter((x) => reach.has(x)), ...pool.filter((x) => !reach.has(x))];

  let selected: string[];
  let target: Float64Array;
  let div: ReturnType<typeof diversifiedShares> = null;
  if (lambda > 0) {
    const k = pool.map((x) => isoIdx.get(x) as number);
    div = diversifiedShares(
      k.map((i) => o.w_node * co(ps[i]) + o.w_edge * conn0[i]),
      k.map((i) => o.w_dev * co(dev[i])),
      lambda, o.min_share, o.max_share, nProd,
    );
  }
  if (div) {
    selected = pool.slice(0, div.k);
    target = div.shares;
  } else {
    selected = pool.slice(0, nProd);
    const selSet = new Set(selected);
    const selOrder = candIso.filter((x) => selSet.has(x));
    const s = selOrder.map((x) => combined[isoIdx.get(x) as number]);
    const mx = Math.max(...s);
    const raw = s.map((v) => Math.exp(v - mx));
    const tot = sumList(raw);
    const shares = capShares(raw.map((v) => v / tot), o.min_share, o.max_share);
    const byIso = new Map(selOrder.map((x, i) => [x, shares[i]] as [string, number]));
    target = Float64Array.from(selected, (x) => byIso.get(x) as number);
  }

  // demand: d / sum(d) (equal when no positive demand)
  const d0 = Float64Array.from(dw, (v) => (Number.isFinite(v) ? v : 0));
  const sd = sumList(Array.from(d0));
  const d = sd > 0 ? d0.map((v) => v / sd) : new Float64Array(n).fill(1 / n);
  const ns = selected.length;
  const K = new Float64Array(ns * n);
  const rsel = selected.map((x) => soIdx.get(x) as number);
  for (let j = 0; j < n; j++) {
    const cj = soIdx.get(iso[j]) as number;
    for (let s = 0; s < ns; s++) {
      const v = E[rsel[s] + n * cj];
      K[s + ns * j] = v !== v ? NaN : Math.max(v, 0);
    }
  }
  const selfCol = selected.map((x) => isoIdx.get(x) as number);
  if (!o.allow_self) selfCol.forEach((j, s) => (K[s + ns * j] = 0));
  const colSum = new Float64Array(n);
  {
    const acc = new LdAcc();
    for (let j = 0; j < n; j++) {
      acc.reset();
      for (let s = 0; s < ns; s++) acc.add(K[s + ns * j]);
      colSum[j] = acc.value();
    }
  }
  const dead: number[] = [];
  for (let j = 0; j < n; j++) if (colSum[j] <= 0 && d[j] > 0) dead.push(j);
  if (dead.length) {
    for (const j of dead) for (let s = 0; s < ns; s++) K[s + ns * j] = 1;
    if (!o.allow_self) {
      const deadSet = new Set(dead);
      selfCol.forEach((j, s) => {
        if (deadSet.has(j)) K[s + ns * j] = 0;
      });
    }
  }
  let f = ras(K, ns, n, target, d);
  f = pruneSmallFlows(f, K, ns, n, target, d, o.min_suppliers_per_consumer, o.epsilon_supplier_share);
  if (lambda > 0) f = pruneSmallFlowsStepwise(f, K, ns, n, target, d, o.min_suppliers_per_consumer, o.epsilon_supplier_share);

  // flows with a positive share, in column-major order (producer fastest)
  const fr: string[] = [];
  const fp: string[] = [];
  const few: number[] = [];
  const ffs: number[] = [];
  const fop: number[] = [];
  const fsh: number[] = [];
  for (let j = 0; j < n; j++) {
    for (let s = 0; s < ns; s++) {
      const v = f[s + ns * j];
      if (!(v > 0)) continue;
      const e = soIdx.get(selected[s])! + n * soIdx.get(iso[j])!;
      fr.push(selected[s]);
      fp.push(iso[j]);
      few.push(E[e]);
      ffs.push(FSm[e]);
      fop.push(OPm[e]);
      fsh.push(v);
    }
  }
  const prod = new Sums(Math.max(ns, n)).rows(f, ns, n, new Float64Array(ns));
  const selIdx = new Map(selected.map((x, i) => [x, i] as [string, number]));
  const selectedFlag = Uint8Array.from(ord, (i) => (selIdx.has(iso[i]) ? 1 : 0));
  const prodShare = Float64Array.from(ord, (i) => {
    const k = selIdx.get(iso[i]);
    return k === undefined ? 0 : prod[k] !== prod[k] ? 0 : prod[k];
  });
  const extra: StageSpecialization['extra'] = {};
  for (const [k, c] of Object.entries(nodes.extra)) extra[k] = pick(c, ord);
  const spec: StageSpecialization = {
    iso3c: ord.map((i) => iso[i]),
    country: ord.map((i) => nodes.country[i]),
    producer_score: pick(ps, ord),
    connectivity_out: pick(conn0, ord),
    dev_potential: pick(dev, ord),
    demand_weight: pick(dw, ord),
    selected: selectedFlag,
    production_share: prodShare,
    extra,
  };
  // objective (missing producer score / development counts as 0)
  const aBy = new Map(spec.iso3c.map((x, i) => [x, co(spec.producer_score[i])] as [string, number]));
  const devBy = new Map(spec.iso3c.map((x, i) => [x, co(spec.dev_potential[i])] as [string, number]));
  const terms: number[] = [];
  for (let q = 0; q < fr.length; q++) {
    const t = fsh[q] * (o.w_node * (aBy.get(fr[q]) as number) + o.w_edge * few[q]);
    if (t === t) terms.push(t);
  }
  let objective = sumList(terms) + o.w_dev * sumList(selected.map((x) => devBy.get(x) as number));
  if (lambda > 0) objective = objective - lambda * sumList(Array.from(prod, (v) => v * v));
  return {
    specialization: spec,
    flows: {
      reporter_iso: fr, partner_iso: fp, edge_weight: Float64Array.from(few), friendshore_index: Float64Array.from(ffs),
      opportunity_index: Float64Array.from(fop), flow_share: Float64Array.from(fsh),
    },
    objective,
    hhi: hhi(prod),
    n_producers: selected.length,
  };
}

/** dash_supplier_stats(): fewest suppliers of a consumer with demand; smallest supplier share. */
export function supplierStats(flows: StageFlows | null, spec: StageSpecialization | null): { min_suppliers: number | null; min_supplier_share: number } {
  if (!flows || !flows.flow_share.length || !spec) return { min_suppliers: null, min_supplier_share: NaN };
  const dIso: string[] = [];
  const dW: number[] = [];
  spec.iso3c.forEach((x, i) => {
    const w = spec.demand_weight[i];
    if ((w !== w ? 0 : w) > 0) {
      dIso.push(x);
      dW.push(w);
    }
  });
  const dIdx = new Map<string, number>();
  dIso.forEach((x, i) => {
    if (!dIdx.has(x)) dIdx.set(x, i);
  });
  const nPer = new Int32Array(dIso.length);
  let minRel = Infinity;
  let any = false;
  for (let q = 0; q < flows.flow_share.length; q++) {
    if (!(flows.flow_share[q] > 1e-12)) continue;
    const k = dIdx.get(flows.partner_iso[q]);
    if (k === undefined) continue;
    any = true;
    nPer[k]++;
    const rel = flows.flow_share[q] / dW[k];
    if (rel < minRel) minRel = rel;
  }
  if (!any) return { min_suppliers: null, min_supplier_share: NaN };
  let mn = Infinity;
  for (const v of nPer) if (v < mn) mn = v;
  return { min_suppliers: mn, min_supplier_share: minRel };
}

/** allied_network_objective_terms(). */
export function objectiveTerms(spec: StageSpecialization, flows: StageFlows | null, wNode: number, wEdge: number, wDev: number, lambda: number) {
  const co = (v: number) => (v !== v ? 0 : v);
  const x = Array.from(spec.production_share, co);
  const a = Array.from(spec.producer_score, co);
  const dev = Array.from(spec.dev_potential, co);
  const edge = !flows || !flows.flow_share.length ? 0 : sumList(Array.from(flows.flow_share, (f, i) => f * co(flows.edge_weight[i])));
  return {
    node: wNode * sumList(a.map((v, i) => v * x[i])),
    edge: wEdge * edge,
    dev: wDev * sumList(dev.filter((_, i) => spec.selected[i] === 1)),
    diversity_penalty: lambda * sumList(x.map((v) => v * v)),
  };
}
