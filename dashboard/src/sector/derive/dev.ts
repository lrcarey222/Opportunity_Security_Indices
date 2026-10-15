// Development & finance flows (sector-dev; development data change 2026-10-15): the OECD CRS aid
// flows (donor -> recipient, development.oda) and the IMF bilateral direct-investment positions
// (investor -> destination, development.fdi_out) as flow lists for the Sankey, the partner lists of
// one economy and the tables. Pure. Values are the raw ones shipped (USD millions); nothing here is a
// score. Conduit financial centres stay in the data, flagged; `hideConduits` drops them as the
// Friendshore measure does.

import type { DevConduit, SectorDevChunk } from '../contract.ts';

/** The measures laid out as flows (explore layout 'devflows'). */
export const DEV_FLOW_VARS: readonly string[] = ['development.oda', 'development.fdi_out'];
export function isDevFlowVar(id: string): boolean {
  return DEV_FLOW_VARS.includes(id);
}

/** One flow: `from` funds or invests in `to` (dims.geo rows). */
export interface DevFlow {
  from: number;
  to: number;
  /** USD millions (aid at constant prices; positions at current prices) */
  v: number;
  /** positions: which side is a conduit financial centre */
  conduit: DevConduit;
  /** positions: first and last year reported in the window, and how many years */
  y0: number | null;
  y1: number | null;
  n: number | null;
}

/** The aid years of one aid-sector item, ascending. */
export function odaYears(c: SectorDevChunk, item: number): number[] {
  const t = c['dev.oda'];
  const out = new Set<number>();
  for (let i = 0; i < t.nrow; i++) if (t.cols.item[i] === item) out.add(t.cols.year[i]);
  return [...out].sort((a, z) => a - z);
}

/** Aid flows of one aid-sector item and year (every year summed when `year` is null). */
export function odaFlows(c: SectorDevChunk, item: number, year: number | null): DevFlow[] {
  const t = c['dev.oda'];
  const acc = new Map<string, DevFlow>();
  for (let i = 0; i < t.nrow; i++) {
    if (t.cols.item[i] !== item || (year !== null && t.cols.year[i] !== year)) continue;
    const from = t.cols.donor[i];
    const to = t.cols.recipient[i];
    const k = `${from}|${to}`;
    const f = acc.get(k);
    if (f) f.v += t.cols.value[i];
    else acc.set(k, { from, to, v: t.cols.value[i], conduit: 'none', y0: null, y1: null, n: null });
  }
  return [...acc.values()];
}

/** The direct-investment positions; with `hideConduits`, those the Friendshore measure drops. */
export function dipFlows(c: SectorDevChunk, hideConduits: boolean, scope: 'both' | 'counterpart' = 'both'): DevFlow[] {
  const t = c['dev.dip'];
  const out: DevFlow[] = [];
  for (let i = 0; i < t.nrow; i++) {
    const conduit = (t.cols.conduit[i] ?? 'none') as DevConduit;
    if (hideConduits && conduitDropped(conduit, scope)) continue;
    out.push({ from: t.cols.reporter[i], to: t.cols.counterpart[i], v: t.cols.value[i], conduit, y0: t.cols.year_min[i], y1: t.cols.year_max[i], n: t.cols.n_years[i] });
  }
  return out;
}

/** Whether the Friendshore measure treats a position as unobserved (config/partnership.yml conduit_scope). */
export function conduitDropped(conduit: DevConduit, scope: 'both' | 'counterpart' = 'both'): boolean {
  return scope === 'counterpart' ? conduit === 'counterpart' || conduit === 'both' : conduit !== 'none';
}

/** The conduit centres of the chunk (dims.geo rows). */
export function conduitGeos(c: SectorDevChunk): Set<number> {
  return new Set(Array.from(c['dev.conduits'].cols.geo));
}

/** The year window of the positions ("2020-2024"). */
export function dipWindow(c: SectorDevChunk): string | null {
  const t = c['dev.dip'];
  if (!t.nrow) return null;
  let lo = Infinity;
  let hi = -Infinity;
  for (let i = 0; i < t.nrow; i++) {
    lo = Math.min(lo, t.cols.year_min[i]);
    hi = Math.max(hi, t.cols.year_max[i]);
  }
  return lo === hi ? String(lo) : `${lo}-${hi}`;
}

/** The sum of the positive flows (the Sankey's 100%). */
export function positiveTotal(flows: readonly DevFlow[]): number {
  let s = 0;
  for (const f of flows) if (f.v > 0) s += f.v;
  return s;
}

/** Totals per economy: what it gives (or invests) and what it receives, positive flows only. */
export function devTotals(flows: readonly DevFlow[]): Map<number, { geo: number; out: number; in: number }> {
  const m = new Map<number, { geo: number; out: number; in: number }>();
  const row = (g: number) => {
    let r = m.get(g);
    if (!r) m.set(g, (r = { geo: g, out: 0, in: 0 }));
    return r;
  };
  for (const f of flows) {
    if (!(f.v > 0)) continue;
    row(f.from).out += f.v;
    row(f.to).in += f.v;
  }
  return m;
}

export const OTHER_FROM = -1;
export const OTHER_TO = -2;

export interface DevLink {
  from: number;
  to: number;
  v: number;
  /** of the positive total */
  share: number;
  /** positions: the folded link holds a conduit position */
  conduit: boolean;
}

/**
 * Sankey links: the top `topFrom` sources to the top `topTo` destinations by positive total; the
 * rest fold into "Other" (OTHER_FROM / OTHER_TO), and links below `fold` of the total fold into the
 * Other destination of their source. Negative flows (mining aid net of its fossil sub-rows) are left
 * out of the picture; the tables list them.
 */
export function devSankey(flows: readonly DevFlow[], opts: { topFrom: number; topTo: number; fold: number }): { links: DevLink[]; total: number } {
  const total = positiveTotal(flows);
  const tot = [...devTotals(flows).values()];
  const topF = new Set(tot.filter((t) => t.out > 0).sort((a, z) => z.out - a.out).slice(0, opts.topFrom).map((t) => t.geo));
  const topT = new Set(tot.filter((t) => t.in > 0).sort((a, z) => z.in - a.in).slice(0, opts.topTo).map((t) => t.geo));
  const acc = new Map<string, DevLink>();
  for (const f of flows) {
    if (!(f.v > 0)) continue;
    const from = topF.has(f.from) ? f.from : OTHER_FROM;
    let to = topT.has(f.to) ? f.to : OTHER_TO;
    if (total > 0 && f.v / total < opts.fold) to = OTHER_TO;
    const k = `${from}|${to}`;
    const l = acc.get(k);
    if (l) {
      l.v += f.v;
      l.conduit ||= f.conduit !== 'none';
    } else acc.set(k, { from, to, v: f.v, share: 0, conduit: f.conduit !== 'none' });
  }
  const links = [...acc.values()];
  for (const l of links) l.share = total > 0 ? l.v / total : NaN;
  links.sort((a, z) => z.v - a.v);
  return { links, total };
}

export interface DevPartner {
  geo: number;
  v: number;
  /** of the economy's positive total on that side */
  share: number;
  conduit: DevConduit;
}

/** One economy's partners: `out` = whom it funds / invests in, `in` = who funds / invests in it. */
export function devPartners(flows: readonly DevFlow[], geo: number, side: 'out' | 'in'): DevPartner[] {
  const rows = flows.filter((f) => (side === 'out' ? f.from === geo : f.to === geo));
  let pos = 0;
  for (const f of rows) if (f.v > 0) pos += f.v;
  return rows
    .map((f) => ({ geo: side === 'out' ? f.to : f.from, v: f.v, share: pos > 0 ? f.v / pos : NaN, conduit: f.conduit }))
    .sort((a, z) => z.v - a.v);
}

/** Plain words for a conduit flag. */
export function conduitWords(c: DevConduit): string {
  switch (c) {
    case 'counterpart':
      return 'held in a conduit centre';
    case 'investor':
      return 'investor is a conduit centre';
    case 'both':
      return 'both are conduit centres';
    default:
      return '';
  }
}
