// The Development lens's Today panel (2026-10-19): official development aid from the Sector bundle's
// sector-dev chunk (OECD CRS gross disbursements, USD millions at constant prices, donor -> recipient by aid
// sector and year), summed over the aid sectors in scope (energy for Downstream, industry for Midstream,
// mining for Upstream) and over the years. For an economy: what it receives by donor and what it gives by
// recipient; without one, the largest donor -> recipient flows. Export credits are not in this build. Pure.

import type { SectorDevChunk } from '../sector/contract.ts';
import { odaFlows, odaYears } from '../sector/derive/dev.ts';

export interface OdaFlow {
  /** dims.geo rows of the Sector bundle */
  from: number;
  to: number;
  /** USD millions, constant prices, summed over the aid sectors and years in scope */
  v: number;
}

export interface OdaView {
  /** the first and last aid year in scope (null without flows) */
  years: [number, number] | null;
  /** with an economy: its donors (largest first) and its recipients */
  received: { geo: number; v: number }[];
  given: { geo: number; v: number }[];
  /** every donor -> recipient flow in scope, largest first (positive flows only) */
  flows: OdaFlow[];
}

const sumBy = (rows: OdaFlow[], key: 'from' | 'to') => {
  const m = new Map<number, number>();
  for (const r of rows) m.set(r[key], (m.get(r[key]) ?? 0) + r.v);
  return [...m.entries()].filter(([, v]) => v > 0).map(([geo, v]) => ({ geo, v })).sort((a, b) => b.v - a.v || a.geo - b.geo);
};

/** Aid flows of the aid-sector items in scope (every year), with an economy's donors and recipients (`geo`: its dims.geo row, or null). */
export function odaView(c: SectorDevChunk, items: readonly number[], geo: number | null): OdaView {
  const acc = new Map<string, OdaFlow>();
  let y0 = Infinity;
  let y1 = -Infinity;
  for (const item of items) {
    for (const y of odaYears(c, item)) {
      y0 = Math.min(y0, y);
      y1 = Math.max(y1, y);
    }
    for (const f of odaFlows(c, item, null)) {
      if (f.from === f.to) continue;
      const k = `${f.from}|${f.to}`;
      const x = acc.get(k);
      if (x) x.v += f.v;
      else acc.set(k, { from: f.from, to: f.to, v: f.v });
    }
  }
  const all = [...acc.values()];
  return {
    years: Number.isFinite(y0) ? [y0, y1] : null,
    received: geo === null ? [] : sumBy(all.filter((f) => f.to === geo), 'from'),
    given: geo === null ? [] : sumBy(all.filter((f) => f.from === geo), 'to'),
    flows: all.filter((f) => f.v > 0).sort((a, b) => b.v - a.v || a.from - b.from || a.to - b.to),
  };
}

/** The partners an economy's aid names (donors and recipients together, largest first), as dims.geo rows. */
export function odaPartners(v: OdaView): number[] {
  const m = new Map<number, number>();
  for (const r of [...v.received, ...v.given]) m.set(r.geo, (m.get(r.geo) ?? 0) + r.v);
  return [...m.entries()].sort((a, b) => b[1] - a[1] || a[0] - b[0]).map(([g]) => g);
}

/** The recipients of the largest flows (no economy chosen), by total received. */
export function odaRecipients(v: OdaView): number[] {
  const m = new Map<number, number>();
  for (const f of v.flows) m.set(f.to, (m.get(f.to) ?? 0) + f.v);
  return [...m.entries()].sort((a, b) => b[1] - a[1] || a[0] - b[0]).map(([g]) => g);
}
