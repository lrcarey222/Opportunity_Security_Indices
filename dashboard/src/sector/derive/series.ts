// Time series (spec_sector.md 4.3 S5, S7, S8, S9; the sector-series chunk). Pure. Lines of the top
// economies plus a neutral "Rest of world"; group areas (China | core | partners | rest | not
// attributed) against the source's world total; window cuts (10 y | 25 y | All).

import type { SplitPart } from '../contract.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { SectorModel } from '../data/model.ts';
import { isHolder } from './concentration.ts';
import { partOf } from './groups.ts';

export interface LinePoint {
  x: number;
  y: number;
  est?: boolean;
}

export interface Line {
  id: string;
  label: string;
  /** a split group, or neutral for the rest of world / world lines */
  group: SplitPart | 'neutral';
  geo: number | null;
  points: LinePoint[];
}

/** The years of (var, item) inside a window ending at the latest year. */
export function windowYears(years: readonly number[], window: number | 'all'): number[] {
  if (window === 'all' || !years.length) return [...years];
  const last = years[years.length - 1];
  return years.filter((y) => y > last - window);
}

/**
 * Lines of the top `n` holders by their latest value, plus "Rest of world" = W - sum of the top
 * (neutral) when a world total exists. `extra` geos (the highlight) are added when not in the top.
 */
export function topLines(idx: FactsIndex, model: SectorModel, v: number, item: number, opts: { n?: number; years?: readonly number[]; extra?: number[]; members?: boolean; rest?: boolean } = {}): Line[] {
  const n = opts.n ?? 6;
  const years = opts.years ?? idx.yearsOf(v, item);
  if (!years.length) return [];
  const last = years[years.length - 1];
  const latest = idx.rows(v, item, last).filter((r) => isHolder(model.geos[r.geo]) && Number.isFinite(r.value) && (opts.members === false || model.geos[r.geo].kind !== 'eu'));
  latest.sort((a, z) => z.value - a.value || a.geo - z.geo);
  const top = latest.slice(0, n).map((r) => r.geo);
  for (const g of opts.extra ?? []) if (g >= 0 && !top.includes(g)) top.push(g);
  const lines: Line[] = top.map((g) => {
    const geo = model.geos[g];
    return {
      id: geo.code,
      label: geo.name,
      group: partOf(geo),
      geo: g,
      points: years.flatMap((y): LinePoint[] => {
        const f = idx.value(v, item, y, g);
        return f && Number.isFinite(f.value) ? [{ x: y, y: f.value, est: f.status === 'est' }] : [];
      }),
    };
  });
  const restPts: LinePoint[] = [];
  for (const y of years) {
    const w = idx.world(v, item, y);
    if (!w || !Number.isFinite(w.value)) continue;
    let s = 0;
    for (const g of top.slice(0, n)) {
      const f = idx.value(v, item, y, g);
      if (f && Number.isFinite(f.value)) s += f.value;
    }
    restPts.push({ x: y, y: Math.max(0, w.value - s) });
  }
  // rest: false leaves it out (a line chart next to a group-shares chart: the residual set the y scale
  // and squeezed the economies' lines into the bottom third)
  if (restPts.length && opts.rest !== false) lines.push({ id: 'ROW', label: 'Rest of world', group: 'neutral', geo: null, points: restPts });
  return lines;
}

export interface AreaPoint {
  x: number;
  group: SplitPart;
  value: number;
  est: boolean;
}

/**
 * Values by group per year (stacked area). The not-attributed band is W - sum of holders when a world
 * total exists (EI "Other", IEA "Rest of world"), else absent. EU-level rows are excluded (members mode).
 */
export function groupArea(idx: FactsIndex, model: SectorModel, v: number, item: number, years?: readonly number[]): AreaPoint[] {
  const ys = years ?? idx.yearsOf(v, item);
  const out: AreaPoint[] = [];
  for (const y of ys) {
    const sums: Record<SplitPart, number> = { china: 0, core: 0, partner: 0, rest: 0, unattributed: 0 };
    let est = false;
    let listed = 0;
    let resid = 0;
    for (const r of idx.rows(v, item, y)) {
      const g = model.geos[r.geo];
      if (!Number.isFinite(r.value)) continue;
      if (r.status === 'est') est = true;
      if (g && isHolder(g) && g.kind !== 'eu') {
        // a mixed region (group na) is not attributed to a group
        sums[partOf(g)] += r.value;
        listed += r.value;
      } else if (g && g.kind === 'residual') resid += r.value;
    }
    const w = idx.world(v, item, y);
    if (w && Number.isFinite(w.value) && resid === 0) resid = Math.max(0, w.value - listed);
    sums.unattributed += resid;
    for (const k of ['china', 'core', 'partner', 'rest', 'unattributed'] as SplitPart[]) out.push({ x: y, group: k, value: sums[k], est });
  }
  return out;
}

/** Normalise an area series to shares of each year's total. */
export function normalizeArea(points: readonly AreaPoint[]): AreaPoint[] {
  const tot = new Map<number, number>();
  for (const p of points) tot.set(p.x, (tot.get(p.x) ?? 0) + p.value);
  return points.map((p) => ({ ...p, value: (tot.get(p.x) ?? 0) > 0 ? p.value / (tot.get(p.x) as number) : 0 }));
}

/** Year-over-year change of the last point of a monthly series (same month a year earlier). */
export function monthlyYoy(points: readonly { year: number; month: number; value: number }[]): number | null {
  if (!points.length) return null;
  const last = points[points.length - 1];
  const prev = points.find((p) => p.year === last.year - 1 && p.month === last.month);
  return prev && prev.value > 0 && Number.isFinite(last.value) ? last.value / prev.value - 1 : null;
}

/** Index a line to its first year = 100. */
export function indexLine(l: Line): Line {
  const base = l.points.find((p) => p.y > 0)?.y;
  if (!base) return l;
  return { ...l, points: l.points.map((p) => ({ ...p, y: (100 * p.y) / base })) };
}
