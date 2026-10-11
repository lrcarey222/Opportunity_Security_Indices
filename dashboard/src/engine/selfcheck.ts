// Self-check (spec_react.md 7.5): the engine at the baseline parameters against the baseline tables
// shipped in the bundle (computed by the R engine). Pure; the worker runs it in idle time.

import type { Column, NDArray, Table } from './codec.ts';
import { run, runPartner, type LayerCache } from './engine.ts';
import { networkInputs, networkScore, networkSolve } from './network/index.ts';
import { NA_BOOL, NA_INT } from './numeric.ts';
import { lgl, num, str } from './table.ts';
import type { Bundle, PartnerCells, PolicyResult } from './types.ts';

export interface SelfCheck {
  layer: 'core' | 'partner' | 'network';
  ok: boolean;
  n: number;
  max_abs_diff: number;
  detail?: string;
}

function toNum(c: Column): Float64Array | null {
  if (c instanceof Float64Array) return c;
  if (c instanceof Int32Array) return Float64Array.from(c, (v) => (v === NA_INT ? NaN : v));
  if (c instanceof Uint8Array) return Float64Array.from(c, (v) => (v === NA_BOOL ? NaN : v));
  return null;
}

class Acc {
  n = 0;
  max = 0;
  problems: string[] = [];
  column(path: string, a: Column | undefined, e: Column | undefined, tol: number): void {
    if (e === undefined) return;
    if (a === undefined) {
      this.problems.push(`${path}: missing`);
      return;
    }
    if (a.length !== e.length) {
      this.problems.push(`${path}: ${a.length} vs ${e.length} rows`);
      return;
    }
    this.n += e.length;
    const an = toNum(a);
    const en = toNum(e);
    if (!an || !en) {
      let bad = 0;
      for (let i = 0; i < e.length; i++) if ((a as (string | null)[])[i] !== (e as (string | null)[])[i]) bad++;
      if (bad) this.problems.push(`${path}: ${bad} values differ`);
      return;
    }
    let na = 0;
    let mx = 0;
    for (let i = 0; i < en.length; i++) {
      const x = an[i];
      const y = en[i];
      if (x !== x || y !== y) {
        if ((x !== x) !== (y !== y)) na++;
        continue;
      }
      const d = Math.abs(x - y);
      if (d > mx) mx = d;
    }
    if (mx > this.max) this.max = mx;
    if (na) this.problems.push(`${path}: ${na} NA mismatches`);
    if (mx > tol) this.problems.push(`${path}: max |diff| ${mx.toExponential(2)} > ${tol}`);
  }
  table(path: string, a: Table | undefined, e: Table | undefined, tol: number): void {
    if (!e) return;
    if (!a) {
      this.problems.push(`${path}: missing`);
      return;
    }
    if (a.nrow !== e.nrow) {
      this.problems.push(`${path}: ${a.nrow} vs ${e.nrow} rows`);
      return;
    }
    for (const [k, c] of Object.entries(e.cols)) this.column(`${path}$${k}`, a.cols[k], c, tol);
  }
  result(layer: SelfCheck['layer']): SelfCheck {
    return { layer, ok: this.problems.length === 0, n: this.n, max_abs_diff: this.max, detail: this.problems.length ? this.problems.slice(0, 12).join('; ') : undefined };
  }
}

const get = (o: unknown, path: string): unknown =>
  path.split('.').reduce<unknown>((x, k) => (x && typeof x === 'object' ? (x as Record<string, unknown>)[k] : undefined), o);

/** L1-L3 at the baseline against baseline.core (abs <= 1e-9; NA patterns and strings identical). */
export function selfCheckCore(bundle: Bundle, tol = 1e-9, opts: { cache?: LayerCache } = {}): SelfCheck {
  const acc = new Acc();
  const res = run(bundle, bundle.baselineParams, opts.cache, { partner: false, contributions: false, pin: true });
  const bc = get(bundle.baseline, 'core');
  acc.table('es', res.pillars.es, get(bc, 'es') as Table, tol);
  acc.table('eo', res.pillars.eo, get(bc, 'eo') as Table, tol);
  acc.table('policy.index', res.policy.index, get(bc, 'policy.index') as Table, tol);
  acc.table('policy.eligibility', res.policy.eligibility, get(bc, 'policy.eligibility') as Table, 0);
  acc.table('si.cells', res.si.cells, get(bc, 'si.cells') as Table, tol);
  acc.table('si.excluded', res.si.excluded, get(bc, 'si.excluded') as Table, 0);
  acc.table('allied', res.allied, get(bc, 'allied') as Table, tol);
  return acc.result('core');
}

/** Best-five and PSI at the baseline against the shipped tables (abs <= 1e-12). */
export function selfCheckPartner(bundle: Bundle, tol = 1e-12, opts: { cache?: LayerCache } = {}): SelfCheck {
  const acc = new Acc();
  const p = runPartner(bundle, bundle.baselineParams, opts.cache, { pin: true });
  const bp = get(bundle.baseline, 'partner') as Record<string, unknown> | undefined;
  if (bp) {
    acc.column('top5_fs', p.top5.fs, (bp.top5_fs as NDArray)?.data, tol);
    acc.column('top5_po', p.top5.po, (bp.top5_po as NDArray)?.data, tol);
    acc.column('fs_exists', p.top5.fs_exists, (bp.fs_exists as NDArray)?.data, 0);
    acc.column('po_exists', p.top5.po_exists, (bp.po_exists as NDArray)?.data, 0);
    acc.table('psi_dyad', p.psi.dyad, bp.psi_dyad as Table, tol);
  }
  acc.table('psi_partner', p.psi.partner, get(bundle.baseline, 'psi_partner') as Table, tol);
  return acc.result('partner');
}

/** The greedy network at the baseline against the shipped greedy baseline (same producers; shares <= 1e-6). */
export async function selfCheckNetwork(
  bundle: Bundle,
  tol = 1e-6,
  opts: { shouldAbort?: () => boolean; cells?: PartnerCells; policy?: PolicyResult } = {},
): Promise<SelfCheck> {
  const acc = new Acc();
  const gb = bundle.network?.greedy_baseline as Record<string, unknown> | null | undefined;
  if (!gb) return { layer: 'network', ok: true, n: 0, max_abs_diff: 0, detail: 'no shipped greedy baseline' };
  const params = bundle.baselineParams;
  // the baseline cells and Policy index (the caller's cached ones when given)
  const pol = opts.policy ?? run(bundle, params, undefined, { partner: false, contributions: false }).policy;
  const cells = opts.cells ?? runPartner(bundle, params).cells;
  const sc = networkScore(networkInputs(bundle, params, cells, pol), params);
  const sol = await networkSolve(sc, params, bundle, { shouldAbort: opts.shouldAbort });
  if (!sol) return { layer: 'network', ok: true, n: 0, max_abs_diff: 0, detail: 'aborted' };
  const want = gb.specialization as Table;
  const key = (t: Table, i: number) => `${str(t, 'tech')[i]}|${str(t, 'supply_chain')[i]}|${str(t, 'iso3c')[i]}`;
  const got = new Map<string, number>();
  for (let i = 0; i < sol.specialization.nrow; i++) got.set(key(sol.specialization, i), i);
  const gSel = lgl(sol.specialization, 'selected');
  const gSh = num(sol.specialization, 'production_share');
  const wSel = lgl(want, 'selected');
  const wSh = num(want, 'production_share');
  let mismatch = 0;
  for (let i = 0; i < want.nrow; i++) {
    const j = got.get(key(want, i));
    acc.n++;
    if (j === undefined) {
      mismatch++;
      continue;
    }
    if (gSel[j] !== wSel[i] || gSh[j] > 0 !== wSh[i] > 0) mismatch++;
    const d = Math.abs(gSh[j] - wSh[i]);
    if (d > acc.max) acc.max = d;
  }
  if (mismatch) acc.problems.push(`${mismatch} producer / selection mismatches`);
  if (acc.max > tol) acc.problems.push(`max share |diff| ${acc.max.toExponential(2)} > ${tol}`);
  if (sol.specialization.nrow !== want.nrow) acc.problems.push(`${sol.specialization.nrow} vs ${want.nrow} specialization rows`);
  return acc.result('network');
}
