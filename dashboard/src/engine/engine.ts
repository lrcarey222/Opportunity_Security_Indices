// Orchestration: one scenario through L1-L4, comparisons with the baseline, download tables and a
// per-layer LRU cache (port of R/dashboard/dash_engine.R; spec_react.md 6.7, 6.8). Pure.

import { deepEqual, NA_BOOL, NA_INT } from './numeric.ts';
import { paramsKey } from './params.ts';
import { fsCells, partnerTop5, poCells, psi } from './partner.ts';
import { pillars } from './pillars.ts';
import { policy } from './policy.ts';
import { alliedSelection, si } from './si.ts';
import { lgl, matchIdx, num, rowKeys, str, table } from './table.ts';
import type {
  Bundle, Column, Params, PartnerCells, PillarsResult, PolicyResult, PsiResult, SiResult, Str, Table, Top5Result,
} from './types.ts';

export type CacheLayer = 'pillars' | 'policy' | 'si' | 'fs_cells' | 'po_cells' | 'top5' | 'psi';

/** An LRU cache per layer; pinned keys (the baseline) are never evicted. */
export class LayerCache {
  private maps = new Map<string, Map<string, unknown>>();
  private pinned = new Set<string>();
  constructor(private limits: Partial<Record<CacheLayer, number>> = {}) {}
  private limit(layer: string): number {
    return (this.limits as Record<string, number>)[layer] ?? 8;
  }
  get<T>(layer: CacheLayer, key: string): T | undefined {
    const m = this.maps.get(layer);
    if (!m || !m.has(key)) return undefined;
    const v = m.get(key) as T;
    m.delete(key);
    m.set(key, v);
    return v;
  }
  has(layer: CacheLayer, key: string): boolean {
    return this.maps.get(layer)?.has(key) ?? false;
  }
  set<T>(layer: CacheLayer, key: string, value: T, pin = false): T {
    let m = this.maps.get(layer);
    if (!m) this.maps.set(layer, (m = new Map()));
    m.delete(key);
    m.set(key, value);
    if (pin) this.pinned.add(`${layer}|${key}`);
    const lim = this.limit(layer);
    if (m.size > lim) {
      for (const k of m.keys()) {
        if (m.size <= lim) break;
        if (!this.pinned.has(`${layer}|${k}`)) m.delete(k);
      }
    }
    return value;
  }
  clear(): void {
    this.maps.clear();
    this.pinned.clear();
  }
}

function cached<T>(cache: LayerCache | undefined, layer: CacheLayer, key: string, f: () => T, pin = false): T {
  if (!cache) return f();
  const hit = cache.get<T>(layer, key);
  if (hit !== undefined) return hit;
  return cache.set(layer, key, f(), pin);
}

export interface PartnerResult {
  cells: PartnerCells;
  top5: Top5Result;
  psi: PsiResult;
}

export interface DashResult {
  params: Params;
  pillars: PillarsResult;
  policy: PolicyResult;
  si: SiResult;
  allied: Table;
  partner: PartnerResult | null;
  keys: Record<string, string>;
  notes: string[];
}

export function layerKeys(params: Params): Record<string, string> {
  const keys: Record<string, string> = {
    pillars: paramsKey(params, 'pillars'),
    policy: paramsKey(params, 'policy'),
    si: paramsKey(params, 'si'),
    fs_cells: paramsKey(params, 'fs_cells'),
    po_cells: paramsKey(params, 'po_cells'),
    psi: paramsKey(params, 'psi'),
  };
  keys.top5 = `top5:${keys.fs_cells}|${keys.po_cells}|${params.partner.exclude_techs.join(',')}|${params.partner.top_k}`;
  return keys;
}

/** Notes attached to a result (the R texts; user-facing wording lives in the UI). */
export function resultNotes(bundle: Pick<Bundle, 'baselineParams'>, params: Params): string[] {
  const notes: string[] = [];
  if (params.si.fossil_excluded) notes.push('fossil technologies excluded');
  if (params.si.require_resource_base === false) notes.push('endowment screen off');
  const base = bundle.baselineParams?.advanced;
  if (base && (!deepEqual(params.pillars.es_weights, base.es_weights) || !deepEqual(params.pillars.eo_weights, base.eo_weights))) {
    notes.push('partner layer uses baseline pillars');
  }
  return notes;
}

/** L1-L3 (always) and, with opts.partner, L4 (dash_run()). */
export function run(bundle: Bundle, params: Params, cache?: LayerCache, opts: { partner?: boolean; contributions?: boolean; pin?: boolean } = {}): DashResult {
  const keys = layerKeys(params);
  const pin = opts.pin === true;
  const contributions = opts.contributions !== false;
  // a cached result without contributions does not serve a request for them (one with them serves both)
  let pil = cache?.get<PillarsResult>('pillars', keys.pillars);
  if (!pil || (contributions && !pil.es_contrib)) {
    pil = pillars(bundle, params, { contributions });
    cache?.set('pillars', keys.pillars, pil, pin);
  }
  const pol = cached(cache, 'policy', keys.policy, () => policy(bundle, params), pin);
  const s = cached(cache, 'si', keys.si, () => si(bundle, params, pil, pol), pin);
  const allied = alliedSelection(s, bundle, params);
  const out: DashResult = { params, pillars: pil, policy: pol, si: s, allied, partner: null, keys, notes: resultNotes(bundle, params) };
  if (opts.partner) out.partner = runPartner(bundle, params, cache, { pin });
  return out;
}

/** L4: FS / PO cells, best-five and PSI (cached per layer). */
export function runPartner(bundle: Bundle, params: Params, cache?: LayerCache, opts: { pin?: boolean } = {}): PartnerResult {
  const keys = layerKeys(params);
  const pin = opts.pin === true;
  const fs = cached(cache, 'fs_cells', keys.fs_cells, () => fsCells(bundle, params), pin);
  const po = cached(cache, 'po_cells', keys.po_cells, () => poCells(bundle, params), pin);
  const cells = { fs_index: fs, po_index: po };
  const top5 = cached(cache, 'top5', keys.top5, () => partnerTop5(bundle, cells, params), pin);
  const p = cached(cache, 'psi', `${keys.psi}|${keys.top5}`, () => psi(bundle, top5, params), pin);
  return { cells, top5, psi: p };
}

// ---- Comparisons -----------------------------------------------------------------------------------

function keysOf(t: Table, keys: string[]): string[] {
  return rowKeys(keys.map((k) => t.cols[k] as ArrayLike<unknown>), t.nrow);
}

/** dash_join_base(): base columns matched by key, with a suffix. */
export function joinBase(x: Table, b: Table, keys: string[], cols: string[], suffix = '_base'): Table {
  const m = matchIdx(keysOf(x, keys), keysOf(b, keys));
  const out: Record<string, Column> = { ...x.cols };
  for (const c of cols) {
    const src = b.cols[c];
    if (Array.isArray(src)) out[c + suffix] = Array.from(m, (k) => (k >= 0 ? src[k] : null));
    else if (src instanceof Float64Array) out[c + suffix] = Float64Array.from(m, (k) => (k >= 0 ? src[k] : NaN));
    else if (src instanceof Int32Array) out[c + suffix] = Int32Array.from(m, (k) => (k >= 0 ? src[k] : NA_INT));
    else out[c + suffix] = Uint8Array.from(m, (k) => (k >= 0 ? (src as Uint8Array)[k] : NA_BOOL));
  }
  return { nrow: x.nrow, cols: out };
}

function delta(a: Float64Array, b: Float64Array): Float64Array {
  return Float64Array.from(a, (v, i) => v - b[i]);
}

function intDelta(a: Int32Array, b: Int32Array): Int32Array {
  return Int32Array.from(a, (v, i) => (v === NA_INT || b[i] === NA_INT ? NA_INT : v - b[i]));
}

export interface CompareResult {
  si: Table;
  es: Table;
  eo: Table;
  policy: Table;
  allied: Table;
  psi_partner?: Table;
}

/** dash_compare(): scenario against the baseline result. */
export function compare(result: Pick<DashResult, 'si' | 'pillars' | 'policy' | 'allied' | 'partner'>, base: Pick<DashResult, 'si' | 'pillars' | 'policy' | 'allied' | 'partner'>): CompareResult {
  const k3 = ['Country', 'tech', 'supply_chain'];
  const s = joinBase(result.si.cells, base.si.cells, k3, ['strategic_index', 'rank_in_country', 'in_sample']);
  s.cols.delta = delta(num(s, 'strategic_index'), num(s, 'strategic_index_base'));
  s.cols.rank_delta = intDelta(s.cols.rank_in_country_base as Int32Array, s.cols.rank_in_country as Int32Array);
  const es = joinBase(result.pillars.es, base.pillars.es, k3, ['value', 'rankable']);
  es.cols.delta = delta(num(es, 'value'), num(es, 'value_base'));
  const eo = joinBase(result.pillars.eo, base.pillars.eo, k3, ['value', 'rankable']);
  eo.cols.delta = delta(num(eo, 'value'), num(eo, 'value_base'));
  const pol = joinBase(result.policy.index, base.policy.index, k3, ['value']);
  pol.cols.delta = delta(num(pol, 'value'), num(pol, 'value_base'));
  const al = joinBase(result.allied, base.allied, ['Country'], ['sector', 'strategic_index']);
  const sec = str(al, 'sector');
  const secB = str(al, 'sector_base');
  al.cols.changed = Uint8Array.from(sec, (v, i) => (secB[i] === null ? 1 : v === null ? NA_BOOL : v !== secB[i] ? 1 : 0));
  const out: CompareResult = { si: s, es, eo, policy: pol, allied: al };
  if (result.partner && base.partner) {
    const pp = joinBase(result.partner.psi.partner, base.partner.psi.partner, ['partner_iso'], ['psi', 'rank', 'friendshore', 'opportunity']);
    pp.cols.delta = delta(num(pp, 'psi'), num(pp, 'psi_base'));
    pp.cols.rank_delta = intDelta(pp.cols.rank_base as Int32Array, pp.cols.rank as Int32Array);
    out.psi_partner = pp;
  }
  return out;
}

/** dash_compare_dyads(): one reporter's dyads against the baseline. */
export function compareDyads(result: PsiResult, base: PsiResult, reporterIso: string): Table {
  const pick = (t: Table) => {
    const r = str(t, 'reporter_iso');
    const idx: number[] = [];
    for (let i = 0; i < t.nrow; i++) if (r[i] === reporterIso) idx.push(i);
    const cols: Record<string, Column> = {};
    for (const [k, c] of Object.entries(t.cols)) {
      if (Array.isArray(c)) cols[k] = idx.map((i) => c[i]);
      else {
        const Ctor = c.constructor as { new (n: number): typeof c };
        const o = new Ctor(idx.length);
        idx.forEach((i, q) => (o[q] = c[i]));
        cols[k] = o;
      }
    }
    return { nrow: idx.length, cols } as Table;
  };
  const d = joinBase(pick(result.dyad), pick(base.dyad), ['partner_iso'], ['psi', 'rank', 'friendshore', 'opportunity']);
  d.cols.delta = delta(num(d, 'psi'), num(d, 'psi_base'));
  d.cols.rank_delta = intDelta(d.cols.rank_base as Int32Array, d.cols.rank as Int32Array);
  return d;
}

// ---- Download tables ---------------------------------------------------------------------------------

/** dash_params_long(): parameters as (section, parameter, value) rows. */
export function paramsLong(params: Params): Table {
  const section: Str = [];
  const parameter: Str = [];
  const value: Str = [];
  const walk = (sec: string, prefix: string, x: unknown) => {
    if (x !== null && typeof x === 'object' && !Array.isArray(x)) {
      for (const [k, v] of Object.entries(x)) walk(sec, prefix ? `${prefix}.${k}` : k, v);
      return;
    }
    const vals = Array.isArray(x) ? x : [x];
    vals.forEach((v, i) => {
      section.push(sec);
      parameter.push(Array.isArray(x) && vals.length > 1 ? `${prefix}${i + 1}` : prefix);
      value.push(v === null || v === undefined ? null : typeof v === 'boolean' ? (v ? 'TRUE' : 'FALSE') : String(v));
    });
  };
  for (const sec of ['dials', 'm', 'pillars', 'policy', 'si', 'partner'] as const) walk(sec, '', (params as unknown as Record<string, unknown>)[sec]);
  return table({ section, parameter, value });
}

/** dash_scenario_tables(): tidy tables of a scenario for download. */
export function scenarioTables(result: DashResult, cmp?: CompareResult | null, bundle?: Bundle | null): Record<string, Table> {
  const out: Record<string, Table> = { parameters: paramsLong(result.params) };
  if (bundle) {
    const m = bundle.manifest;
    out.data_vintage = table({
      item: ['bundle_created', 'git_commit', 'osi_branch', 'notes'],
      value: [m.created ?? null, m.git_commit ?? null, m.osi_branch ?? null, (m.notes ?? []).join(' | ')],
    });
  }
  out.strategic_index = cmp ? cmp.si : result.si.cells;
  out.strategic_excluded = result.si.excluded;
  out.allied_selection = cmp ? cmp.allied : result.allied;
  out.energy_security = cmp ? cmp.es : result.pillars.es;
  out.economic_opportunity = cmp ? cmp.eo : result.pillars.eo;
  if (result.pillars.es_contrib) out.es_contributions = result.pillars.es_contrib;
  if (result.pillars.eo_contrib) out.eo_contributions = result.pillars.eo_contrib;
  out.policy_index = cmp ? cmp.policy : result.policy.index;
  if (result.partner) {
    out.psi_partner = cmp?.psi_partner ?? result.partner.psi.partner;
    out.psi_dyad = result.partner.psi.dyad;
  }
  return out;
}

export { lgl };
