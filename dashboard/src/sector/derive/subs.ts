// Trade by sub-component (user edit 2a-iv; spec_country.md C5.4): the groups of HS6 codes a cell's
// basket is built from (trade.codes.sub_sector), with each economy's exports and imports of one group
// (trade.cty_sub of the trade chunk, on the cell's reconciled basis, so a cell's sub-components add up
// to its trade.exports / trade.imports). Pure.
//
//   * names and codes come from trade.codes (no dims table); the order is by world exports in the
//     latest year (largest first), then by name;
//   * rows of one (sub-component, year, side) as FactRows, so every Sector card (league, map,
//     concentration) works on them unchanged; the world total is the WLD row;
//   * EU bloc: the members' rows become one EUU row, minus the intra-EU flows of that sub-component
//     and year (trade.sub_intra), and the world total loses the same intra-EU flows;
//   * sub-component values are never added across cells (a code shared with another cell counts in
//     both).

import type { SectorTradeChunk } from '../contract.ts';
import type { FactRow } from '../data/facts.ts';
import type { SectorModel } from '../data/model.ts';
import { declaredImporters, type TradeTotals, type TradeYear } from './trade.ts';

export type SubSide = 'exports' | 'imports';

export interface SubComponent {
  name: string;
  /** HS6 codes of the group, sorted */
  codes: string[];
  /** world exports of the group in the latest year (NaN when the chunk has no trade.cty_sub) */
  world: number;
  /** share of the cell's world exports in that year */
  share: number;
  year: number | null;
}

/** Does the chunk carry trade by sub-component? */
export function hasSubTrade(trade: SectorTradeChunk | null | undefined): boolean {
  const t = trade?.['trade.cty_sub'];
  return !!t && t.nrow > 0;
}

/** The row range of one item in a table sorted by item (binary search); [0, 0) when absent. */
function itemRange(items: ArrayLike<number>, n: number, item: number): [number, number] {
  let lo = 0;
  let hi = n;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (items[mid] < item) lo = mid + 1;
    else hi = mid;
  }
  let end = lo;
  while (end < n && items[end] === item) end++;
  return [lo, end];
}

/** The latest year with sub-component rows for an item, or null. */
export function subLatestYear(trade: SectorTradeChunk, item: number): number | null {
  const t = trade['trade.cty_sub'];
  if (!t) return null;
  const [a, b] = itemRange(t.cols.item, t.nrow, item);
  let y: number | null = null;
  for (let r = a; r < b; r++) if (y === null || t.cols.year[r] > y) y = t.cols.year[r];
  return y;
}

/** The sub-components of a cell: names and codes from trade.codes, world exports from trade.cty_sub. */
export function subComponents(trade: SectorTradeChunk, model: Pick<SectorModel, 'world'>, item: number): SubComponent[] {
  const codes = trade['trade.codes'];
  const byName = new Map<string, Set<string>>();
  for (let i = 0; i < codes.nrow; i++) {
    if (codes.cols.item[i] !== item) continue;
    const name = codes.cols.sub_sector[i] ?? '';
    if (!name) continue;
    let s = byName.get(name);
    if (!s) byName.set(name, (s = new Set()));
    if (codes.cols.hs6[i]) s.add(codes.cols.hs6[i] as string);
  }
  const year = subLatestYear(trade, item);
  const world = new Map<string, number>();
  const t = trade['trade.cty_sub'];
  const wld = model.world?.idx ?? -1;
  if (t && year !== null) {
    const [a, b] = itemRange(t.cols.item, t.nrow, item);
    for (let r = a; r < b; r++) if (t.cols.year[r] === year && t.cols.geo[r] === wld) world.set(t.cols.sub_sector[r] ?? '', t.cols.exports[r]);
  }
  let total = 0;
  for (const v of world.values()) if (Number.isFinite(v)) total += v;
  const out: SubComponent[] = [...byName.entries()].map(([name, s]) => {
    const w = t ? (world.get(name) ?? 0) : NaN;
    return { name, codes: [...s].sort(), world: w, share: total > 0 && Number.isFinite(w) ? w / total : NaN, year };
  });
  return out.sort((x, z) => (Number.isFinite(z.world) ? z.world : -1) - (Number.isFinite(x.world) ? x.world : -1) || x.name.localeCompare(z.name, 'en'));
}

/** The intra-EU flows of a sub-component and year (0 when the table or the row is absent). */
export function subIntra(trade: SectorTradeChunk, item: number, sub: string, year: number): number {
  const t = trade['trade.sub_intra'];
  if (!t) return 0;
  for (let r = 0; r < t.nrow; r++) if (t.cols.item[r] === item && t.cols.year[r] === year && t.cols.sub_sector[r] === sub) return t.cols.intra[r];
  return 0;
}

export interface SubSlice {
  rows: FactRow[];
  world: { value: number } | null;
  /** 'summed': EU members merged into EUU (intra-EU flows removed) */
  mode: 'members' | 'summed';
}

/**
 * One (cell, sub-component, year, side) as fact rows: every economy with a value > 0 (status mirror for
 * the imports of an economy that did not declare the cell that year), and the world total.
 */
export function subSlice(trade: SectorTradeChunk, model: Pick<SectorModel, 'geos' | 'world' | 'eu'>, item: number, sub: string, year: TradeYear | number, side: SubSide, eu: 'members' | 'bloc' = 'members'): SubSlice {
  const t = trade['trade.cty_sub'];
  if (!t) return { rows: [], world: null, mode: 'members' };
  const [a, b] = itemRange(t.cols.item, t.nrow, item);
  const wld = model.world?.idx ?? -1;
  const decl = side === 'imports' ? declaredImporters(trade, item, year) : null;
  const vals = side === 'exports' ? t.cols.exports : t.cols.imports;
  let W: number | null = null;
  const rows: FactRow[] = [];
  for (let r = a; r < b; r++) {
    if (t.cols.year[r] !== year || t.cols.sub_sector[r] !== sub) continue;
    const g = t.cols.geo[r];
    const v = vals[r];
    if (g === wld) {
      W = t.cols.exports[r];
      continue;
    }
    if (!(v > 0)) continue;
    const geo = model.geos[g];
    if (!geo || geo.kind !== 'country') continue;
    rows.push({ geo: g, value: v, status: decl && decl.size && !decl.has(g) ? 'mirror' : 'obs', obs_year: null });
  }
  rows.sort((x, z) => x.geo - z.geo);
  if (eu === 'bloc' && model.eu) {
    const intra = subIntra(trade, item, sub, year);
    let sum = 0;
    let any = false;
    let mirror = false;
    const out: FactRow[] = [];
    for (const r of rows) {
      if (model.geos[r.geo]?.eu_member) {
        sum += r.value;
        any = true;
        if (r.status === 'mirror') mirror = true;
      } else out.push(r);
    }
    if (any) {
      const v = Math.max(0, sum - intra);
      if (v > 0) out.push({ geo: model.eu.idx, value: v, status: mirror ? 'mirror' : 'obs', obs_year: null });
      out.sort((x, z) => x.geo - z.geo);
    }
    return { rows: out, world: W === null ? null : { value: Math.max(0, W - intra) }, mode: 'summed' };
  }
  return { rows, world: W === null ? null : { value: W }, mode: 'members' };
}

/** Net trade of one sub-component and year (exports - imports), for economies with either side > 0. */
export function subNet(trade: SectorTradeChunk, model: Pick<SectorModel, 'geos' | 'world' | 'eu'>, item: number, sub: string, year: number, eu: 'members' | 'bloc' = 'members'): { geo: number; exports: number; imports: number; net: number }[] {
  const ex = subSlice(trade, model, item, sub, year, 'exports', eu).rows;
  const im = subSlice(trade, model, item, sub, year, 'imports', eu).rows;
  const m = new Map<number, { geo: number; exports: number; imports: number; net: number }>();
  for (const r of ex) m.set(r.geo, { geo: r.geo, exports: r.value, imports: 0, net: r.value });
  for (const r of im) {
    const x = m.get(r.geo) ?? { geo: r.geo, exports: 0, imports: 0, net: 0 };
    x.imports = r.value;
    x.net = x.exports - x.imports;
    m.set(r.geo, x);
  }
  return [...m.values()];
}

/**
 * Exports, imports and net per economy for one sub-component and year (the shape of countryTotals(),
 * so the trade cards rank a sub-component exactly as they rank the whole basket), and the world total.
 */
export function subTotals(trade: SectorTradeChunk, model: Pick<SectorModel, 'geos' | 'world' | 'eu'>, item: number, sub: string, year: number, eu: 'members' | 'bloc' = 'members'): { totals: Map<number, TradeTotals>; W: number } {
  const ex = subSlice(trade, model, item, sub, year, 'exports', eu);
  const im = subSlice(trade, model, item, sub, year, 'imports', eu);
  const m = new Map<number, TradeTotals>();
  const get = (g: number) => {
    let t = m.get(g);
    if (!t) m.set(g, (t = { geo: g, exports: 0, imports: 0, net: 0, exportsMirror: 0, importsMirror: 0 }));
    return t;
  };
  for (const r of ex.rows) get(r.geo).exports = r.value;
  for (const r of im.rows) {
    const t = get(r.geo);
    t.imports = r.value;
    if (r.status === 'mirror') t.importsMirror = r.value;
  }
  for (const t of m.values()) t.net = t.exports - t.imports;
  return { totals: m, W: ex.world?.value ?? 0 };
}

/** A sub-component name valid for the cell, else null (an unknown `sub` falls back to the whole basket). */
export function validSub(subs: readonly SubComponent[], sub: string | null | undefined): string | null {
  if (!sub) return null;
  return subs.some((s) => s.name === sub) ? sub : null;
}
