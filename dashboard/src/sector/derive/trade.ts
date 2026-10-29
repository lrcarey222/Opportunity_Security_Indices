// Trade on the reconciled Comtrade dyad matrix (spec_sector.md 3.8, 4.3 S4). Pure. Every trade number
// on screen is a sum over the matrix, so leagues, the Sankey, import mixes and the snapshot agree.
//
//   * flows: the positive dyads of one cell and year (status importer-declared or mirror x1.06);
//   * zeros are rebuilt, not shipped: a pair absent from trade.flows whose importer declared in that
//     year is an observed zero, otherwise it is unobserved;
//   * EU bloc: intra-EU flows are removed first, then the 27 members become one row (EUU).

import type { SectorTradeChunk, SplitPart } from '../contract.ts';
import type { GeoInfo, SectorModel } from '../data/model.ts';
import { partOf } from './groups.ts';

export type TradeYear = 2020 | 2024;

export interface Flow {
  /** exporter / importer geo indices (EUU's index for the bloc in bloc mode) */
  e: number;
  i: number;
  v: number;
  s: 'importer' | 'mirror';
}

/** The positive flows of one item (cell) and year. */
export function flowsOf(trade: SectorTradeChunk, item: number, year: TradeYear): Flow[] {
  const t = trade['trade.flows'];
  const c = t.cols;
  const vs = year === 2024 ? c.v2024 : c.v2020;
  const ss = year === 2024 ? c.s2024 : c.s2020;
  const out: Flow[] = [];
  // rows are sorted by item: find the item's block
  let lo = 0;
  let hi = t.nrow;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (c.item[mid] < item) lo = mid + 1;
    else hi = mid;
  }
  for (let r = lo; r < t.nrow && c.item[r] === item; r++) {
    const v = vs[r];
    if (!(v > 0)) continue;
    const s = ss[r];
    out.push({ e: c.exporter[r], i: c.importer[r], v, s: s === 'mirror' ? 'mirror' : 'importer' });
  }
  return out;
}

/** Items (cells) present in a trade chunk. */
export function tradeItems(trade: SectorTradeChunk): number[] {
  const c = trade['trade.flows'].cols.item;
  const out: number[] = [];
  for (let r = 0; r < c.length; r++) if (out[out.length - 1] !== c[r]) out.push(c[r]);
  return out;
}

/** Bloc mode: members -> one EUU node, intra-EU flows removed. */
export function blocFlows(flows: readonly Flow[], model: SectorModel): Flow[] {
  const eu = model.eu;
  if (!eu) return [...flows];
  const isM = (g: number) => !!model.geos[g]?.eu_member;
  const merged = new Map<string, Flow>();
  for (const f of flows) {
    const e = isM(f.e) ? eu.idx : f.e;
    const i = isM(f.i) ? eu.idx : f.i;
    if (e === eu.idx && i === eu.idx) continue; // intra-EU
    const k = `${e}|${i}`;
    const m = merged.get(k);
    if (m) {
      m.v += f.v;
      if (f.s === 'mirror') m.s = 'mirror';
    } else merged.set(k, { e, i, v: f.v, s: f.s });
  }
  return [...merged.values()];
}

export interface TradeTotals {
  geo: number;
  exports: number;
  imports: number;
  net: number;
  /** exports to the world that came from partners' (importers') declarations vs mirrors */
  exportsMirror: number;
  importsMirror: number;
}

/** Exports, imports and net per economy (every exporter and importer that appears). */
export function countryTotals(flows: readonly Flow[]): Map<number, TradeTotals> {
  const m = new Map<number, TradeTotals>();
  const get = (g: number) => {
    let t = m.get(g);
    if (!t) m.set(g, (t = { geo: g, exports: 0, imports: 0, net: 0, exportsMirror: 0, importsMirror: 0 }));
    return t;
  };
  for (const f of flows) {
    const e = get(f.e);
    const i = get(f.i);
    e.exports += f.v;
    i.imports += f.v;
    if (f.s === 'mirror') {
      e.exportsMirror += f.v;
      i.importsMirror += f.v;
    }
  }
  for (const t of m.values()) t.net = t.exports - t.imports;
  return m;
}

/** World total of the matrix (the sum of every positive flow; basis sum_all). */
export function matrixTotal(flows: readonly Flow[]): number {
  let s = 0;
  for (const f of flows) s += f.v;
  return s;
}

export interface LeagueRow {
  geo: number;
  value: number;
  share: number;
  rank: number;
}

/** The exporters or importers ranked by value (largest first; ties by geo index). */
export function league(totals: Map<number, TradeTotals>, side: 'exports' | 'imports', W: number): LeagueRow[] {
  const rows = [...totals.values()].filter((t) => t[side] > 0).sort((a, z) => z[side] - a[side] || a.geo - z.geo);
  let rank = 0;
  let prev = NaN;
  return rows.map((t, k) => {
    if (t[side] !== prev) rank = k + 1;
    prev = t[side];
    return { geo: t.geo, value: t[side], share: W > 0 ? t[side] / W : NaN, rank };
  });
}

/** The largest traders by exports + imports, with net = exports - imports (diverging bars). */
export function netTrade(totals: Map<number, TradeTotals>, top = 25): TradeTotals[] {
  return [...totals.values()].sort((a, z) => z.exports + z.imports - (a.exports + a.imports) || a.geo - z.geo).slice(0, top);
}

export const OTHER_EXPORTERS = -1;
export const OTHER_IMPORTERS = -2;

export interface SankeyLink {
  /** geo index, or OTHER_EXPORTERS / OTHER_IMPORTERS */
  e: number;
  i: number;
  v: number;
  share: number;
  mirror: boolean;
}

/**
 * Who supplies whom (4.3 S4b): the top `topE` exporters + "Other exporters" on the left, the top `topI`
 * importers + "Other importers" on the right. A link under `fold` of the world total folds into its
 * exporter's link to "Other importers" (so the exporters' totals on the left stay exact).
 */
export function sankeyLinks(flows: readonly Flow[], opts: { topE?: number; topI?: number; fold?: number } = {}): { links: SankeyLink[]; total: number; folded: number } {
  const topE = opts.topE ?? 8;
  const topI = opts.topI ?? 10;
  const fold = opts.fold ?? 0.005;
  const totals = countryTotals(flows);
  const total = matrixTotal(flows);
  const E = new Set([...totals.values()].filter((t) => t.exports > 0).sort((a, z) => z.exports - a.exports || a.geo - z.geo).slice(0, topE).map((t) => t.geo));
  const I = new Set([...totals.values()].filter((t) => t.imports > 0).sort((a, z) => z.imports - a.imports || a.geo - z.geo).slice(0, topI).map((t) => t.geo));
  const add = (m: Map<string, SankeyLink>, e: number, i: number, v: number, mirror: boolean) => {
    const k = `${e}|${i}`;
    const l = m.get(k);
    if (l) {
      l.v += v;
      l.mirror = l.mirror || mirror;
    } else m.set(k, { e, i, v, share: 0, mirror });
  };
  const first = new Map<string, SankeyLink>();
  for (const f of flows) add(first, E.has(f.e) ? f.e : OTHER_EXPORTERS, I.has(f.i) ? f.i : OTHER_IMPORTERS, f.v, f.s === 'mirror');
  const agg = new Map<string, SankeyLink>();
  let folded = 0;
  for (const l of first.values()) {
    if (total > 0 && l.v / total < fold && l.i !== OTHER_IMPORTERS) {
      folded += l.v;
      add(agg, l.e, OTHER_IMPORTERS, l.v, l.mirror);
    } else add(agg, l.e, l.i, l.v, l.mirror);
  }
  const links = [...agg.values()].map((l) => ({ ...l, share: total > 0 ? l.v / total : 0 })).sort((a, z) => z.v - a.v || a.e - z.e || a.i - z.i);
  return { links, total, folded };
}

/** One economy's partners: where its imports come from or where its exports go, largest first. */
export function partners(flows: readonly Flow[], geo: number, dir: 'imports' | 'exports'): { geo: number; value: number; share: number; mirror: boolean }[] {
  const m = new Map<number, { geo: number; value: number; share: number; mirror: boolean }>();
  let total = 0;
  for (const f of flows) {
    const mine = dir === 'imports' ? f.i === geo : f.e === geo;
    if (!mine) continue;
    const p = dir === 'imports' ? f.e : f.i;
    total += f.v;
    const r = m.get(p);
    if (r) {
      r.value += f.v;
      r.mirror = r.mirror || f.s === 'mirror';
    } else m.set(p, { geo: p, value: f.v, share: 0, mirror: f.s === 'mirror' });
  }
  return [...m.values()].map((r) => ({ ...r, share: total > 0 ? r.value / total : 0 })).sort((a, z) => z.value - a.value || a.geo - z.geo);
}

export interface SupplierMix {
  importer: number;
  total: number;
  parts: Record<SplitPart, number>;
  china: number;
  /** HHI of the importer's suppliers (economy level) */
  hhi: number;
}

/** Each importer's imports split by supplier group (4.3 S4c); parts sum to 1. */
export function supplierMixes(flows: readonly Flow[], geos: readonly GeoInfo[], importers?: ReadonlySet<number>): SupplierMix[] {
  const byImp = new Map<number, Map<number, number>>();
  for (const f of flows) {
    if (importers && !importers.has(f.i)) continue;
    let m = byImp.get(f.i);
    if (!m) byImp.set(f.i, (m = new Map()));
    m.set(f.e, (m.get(f.e) ?? 0) + f.v);
  }
  const out: SupplierMix[] = [];
  for (const [imp, sup] of byImp) {
    let total = 0;
    for (const v of sup.values()) total += v;
    if (!(total > 0)) continue;
    const parts: Record<SplitPart, number> = { china: 0, core: 0, partner: 0, rest: 0, unattributed: 0 };
    let hhi = 0;
    for (const [e, v] of sup) {
      const s = v / total;
      parts[partOf(geos[e])] += s;
      hhi += s * s;
    }
    out.push({ importer: imp, total, parts, china: parts.china, hhi });
  }
  return out.sort((a, z) => z.china - a.china || z.total - a.total || a.importer - z.importer);
}

/** Has `geo` declared its imports (or exports) of this item in this year? */
export function declared(trade: SectorTradeChunk, item: number, year: number, geo: number, side: 'imports' | 'exports' = 'imports'): boolean | null {
  const d = trade['trade.declared'];
  const c = d.cols;
  for (let r = 0; r < d.nrow; r++) {
    if (c.item[r] === item && c.year[r] === year && c.geo[r] === geo) return (side === 'imports' ? c.imports_declared[r] : c.exports_declared[r]) === 1;
  }
  return null;
}

/** The importers that declared in a year (set of geo indices). */
export function declaredImporters(trade: SectorTradeChunk, item: number, year: number): Set<number> {
  const d = trade['trade.declared'];
  const c = d.cols;
  const out = new Set<number>();
  for (let r = 0; r < d.nrow; r++) if (c.item[r] === item && c.year[r] === year && c.imports_declared[r] === 1) out.add(c.geo[r]);
  return out;
}

/** A pair's state: a positive flow, an observed zero (the importer declared), or unobserved. */
export function pairState(flows: readonly Flow[], declaredSet: ReadonlySet<number>, e: number, i: number): 'flow' | 'zero' | 'unobserved' {
  if (flows.some((f) => f.e === e && f.i === i)) return 'flow';
  return declaredSet.has(i) ? 'zero' : 'unobserved';
}

export interface TradeQuality {
  total: number;
  importerShare: number;
  mirrorShare: number;
  /** importers without a declaration in the year, largest (by mirrored imports) first */
  nonDeclarers: { geo: number; imports: number }[];
  declaredCount: number;
}

/** The data-quality strip (4.3 S4f). */
export function tradeQuality(flows: readonly Flow[], declaredSet: ReadonlySet<number>): TradeQuality {
  const total = matrixTotal(flows);
  let mirror = 0;
  const imp = new Map<number, number>();
  for (const f of flows) {
    if (f.s === 'mirror') mirror += f.v;
    imp.set(f.i, (imp.get(f.i) ?? 0) + f.v);
  }
  const nonDeclarers = [...imp.entries()]
    .filter(([g]) => !declaredSet.has(g))
    .map(([geo, imports]) => ({ geo, imports }))
    .sort((a, z) => z.imports - a.imports || a.geo - z.geo);
  return { total, importerShare: total > 0 ? (total - mirror) / total : NaN, mirrorShare: total > 0 ? mirror / total : NaN, nonDeclarers, declaredCount: declaredSet.size };
}

/** Exporter-group shares of a geo's imports in two years (2020 vs 2024 dumbbell). */
export function importGroupShares(flows: readonly Flow[], geos: readonly GeoInfo[], importer: number): Record<SplitPart, number> | null {
  const m = supplierMixes(flows, geos, new Set([importer]));
  return m[0]?.parts ?? null;
}

/** The matrix HHI of exporters (economy level; the same as concentration() on country totals). */
export function exporterHhi(flows: readonly Flow[]): number {
  const t = countryTotals(flows);
  const W = matrixTotal(flows);
  let h = 0;
  for (const x of t.values()) if (x.exports > 0) h += (x.exports / W) ** 2;
  return h;
}

// ---- excluded codes (schema 2, design C6) -------------------------------------------------------

/** One code the reference master keeps out of a technology's baskets, with the reason. */
export interface ExcludedCode {
  sub: string | null;
  hs6: string;
  reason: string;
}

/** What the excluded list is: per technology, because the reference master gives those rows no stage. */
export const EXCLUDED_WORDS = 'Listed for the technology as a whole: the reference list of codes gives these no supply-chain stage, so none is assigned.';

/** The excluded codes of a technology (trade.excluded rows whose tech is its label), by sub-component then code. */
export function excludedCodes(trade: SectorTradeChunk, techLabel: string): ExcludedCode[] {
  const t = trade['trade.excluded'];
  const out: ExcludedCode[] = [];
  for (let i = 0; i < t.nrow; i++) if (t.cols.tech[i] === techLabel) out.push({ sub: t.cols.sub_sector[i], hs6: t.cols.hs6[i] ?? '', reason: t.cols.reason[i] ?? '' });
  return out.sort((a, z) => (a.sub ?? '').localeCompare(z.sub ?? '') || a.hs6.localeCompare(z.hs6));
}
