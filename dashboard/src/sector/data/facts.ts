// Slices of facts.obs / series.obs (spec_sector.md 8.6 rules): every selector is O(slice); rows are
// materialised only for one (var, item, year) slice at a time, never for the whole table. Pure.

import type { SectorFactsChunk, SectorSeriesChunk, StatusCode, WorldBasis } from '../contract.ts';
import { NA_INT } from '../../engine/numeric.ts';

/**
 * One fact of a slice. `value` is NaN for nq / na_growth (and nsr where a zero-fill would read as a
 * figure); an nsr row of the net balance or self-sufficiency may carry the value that an assumed zero
 * production gives (contract: facts.notes `production_assumed_zero`).
 */
export interface FactRow {
  geo: number;
  value: number;
  status: StatusCode;
  /** the per-row observation year (year_field vars), else null */
  obs_year: number | null;
  /** dict.caveats ids of the row's facts.notes (absent when it has none) */
  notes?: readonly string[];
}

export interface WorldRow {
  value: number;
  basis: WorldBasis;
}

type ObsLike = SectorFactsChunk['facts.obs'];
type IndexLike = SectorFactsChunk['facts.index'];
type WorldLike = SectorFactsChunk['facts.world'];
type NotesLike = NonNullable<SectorFactsChunk['facts.notes']>;

/** Index over an obs / index / world triple (sector-facts or the annual part of sector-series). */
export class FactsIndex {
  private readonly obs: ObsLike;
  private readonly slices = new Map<string, { start: number; n: number }>();
  private readonly years = new Map<string, number[]>();
  private readonly itemsByVar = new Map<number, number[]>();
  private readonly worlds = new Map<string, WorldRow>();
  private readonly rowCache = new Map<string, FactRow[]>();
  /** facts.notes by `var|item|year|geo` */
  private readonly noteMap = new Map<string, string[]>();

  constructor(obs: ObsLike, index: IndexLike, world: WorldLike, notes?: NotesLike | null) {
    this.obs = obs;
    if (notes) {
      const n = notes.cols;
      for (let i = 0; i < notes.nrow; i++) {
        const id = n.note[i];
        if (!id) continue;
        const k = `${n.var[i]}|${n.item[i]}|${n.year[i]}|${n.geo[i]}`;
        const xs = this.noteMap.get(k);
        if (xs) {
          if (!xs.includes(id)) xs.push(id);
        } else this.noteMap.set(k, [id]);
      }
    }
    const x = index.cols;
    for (let s = 0; s < index.nrow; s++) {
      const v = x.var[s];
      const item = x.item[s];
      const year = x.year[s];
      this.slices.set(`${v}|${item}|${year}`, { start: x.start[s], n: x.n[s] });
      const k = `${v}|${item}`;
      let ys = this.years.get(k);
      if (!ys) this.years.set(k, (ys = []));
      ys.push(year);
      let its = this.itemsByVar.get(v);
      if (!its) this.itemsByVar.set(v, (its = []));
      if (its[its.length - 1] !== item && !its.includes(item)) its.push(item);
    }
    for (const ys of this.years.values()) ys.sort((a, z) => a - z);
    const w = world.cols;
    for (let i = 0; i < world.nrow; i++) this.worlds.set(`${w.var[i]}|${w.item[i]}|${w.year[i]}`, { value: w.value[i], basis: (w.basis[i] ?? 'none') as WorldBasis });
  }

  /** Years with data for (var, item), ascending. */
  yearsOf(v: number, item: number): number[] {
    return this.years.get(`${v}|${item}`) ?? [];
  }

  /** The latest year of (var, item) (optionally at most `max`), or null. */
  latestYear(v: number, item: number, max?: number): number | null {
    const ys = this.yearsOf(v, item);
    for (let i = ys.length - 1; i >= 0; i--) if (max === undefined || ys[i] <= max) return ys[i];
    return null;
  }

  /** Items of a var with any data. */
  itemsOf(v: number): number[] {
    return this.itemsByVar.get(v) ?? [];
  }

  has(v: number, item: number, year: number): boolean {
    return this.slices.has(`${v}|${item}|${year}`);
  }

  /** The facts.notes ids of one fact (dict.caveats ids), or an empty list. */
  notesOf(v: number, item: number, year: number, geo: number): readonly string[] {
    return this.noteMap.get(`${v}|${item}|${year}|${geo}`) ?? NO_NOTES;
  }

  /** Does the chunk carry any facts.notes row? */
  get hasNotes(): boolean {
    return this.noteMap.size > 0;
  }

  private withNotes(row: FactRow, v: number, item: number, year: number): FactRow {
    if (!this.noteMap.size) return row;
    const ns = this.noteMap.get(`${v}|${item}|${year}|${row.geo}`);
    return ns ? { ...row, notes: ns } : row;
  }

  /** The facts of one (var, item, year), sorted by geo index (the obs order). Cached per slice. */
  rows(v: number, item: number, year: number): FactRow[] {
    const key = `${v}|${item}|${year}`;
    const hit = this.rowCache.get(key);
    if (hit) return hit;
    const s = this.slices.get(key);
    const out: FactRow[] = [];
    if (s) {
      const c = this.obs.cols;
      for (let r = s.start; r < s.start + s.n; r++) {
        out.push(this.withNotes({ geo: c.geo[r], value: c.value[r], status: (c.status[r] ?? 'obs') as StatusCode, obs_year: c.obs_year[r] === NA_INT ? null : c.obs_year[r] }, v, item, year));
      }
    }
    this.rowCache.set(key, out);
    return out;
  }

  /** One geo's fact in a slice, or null. */
  value(v: number, item: number, year: number, geo: number): FactRow | null {
    const s = this.slices.get(`${v}|${item}|${year}`);
    if (!s) return null;
    const c = this.obs.cols;
    // obs rows of a slice are sorted by geo: binary search
    let lo = s.start;
    let hi = s.start + s.n - 1;
    while (lo <= hi) {
      const mid = (lo + hi) >> 1;
      const g = c.geo[mid];
      if (g === geo) return this.withNotes({ geo, value: c.value[mid], status: (c.status[mid] ?? 'obs') as StatusCode, obs_year: c.obs_year[mid] === NA_INT ? null : c.obs_year[mid] }, v, item, year);
      if (g < geo) lo = mid + 1;
      else hi = mid - 1;
    }
    // not sorted by geo (defensive): linear scan
    for (let r = s.start; r < s.start + s.n; r++) if (c.geo[r] === geo) return this.withNotes({ geo, value: c.value[r], status: (c.status[r] ?? 'obs') as StatusCode, obs_year: c.obs_year[r] === NA_INT ? null : c.obs_year[r] }, v, item, year);
    return null;
  }

  /** The world total W of (var, item, year), or null. */
  world(v: number, item: number, year: number): WorldRow | null {
    return this.worlds.get(`${v}|${item}|${year}`) ?? null;
  }

  /** A geo's values over every year of (var, item): [{year, value, status}]. */
  seriesOf(v: number, item: number, geo: number): { year: number; value: number; status: StatusCode }[] {
    const out: { year: number; value: number; status: StatusCode }[] = [];
    for (const y of this.yearsOf(v, item)) {
      const f = this.value(v, item, y, geo);
      if (f) out.push({ year: y, value: f.value, status: f.status });
    }
    return out;
  }
}

const NO_NOTES: readonly string[] = Object.freeze([]) as readonly string[];

const factsCache = new WeakMap<object, FactsIndex>();

export function factsIndexOf(chunk: SectorFactsChunk): FactsIndex {
  let f = factsCache.get(chunk);
  if (!f) {
    f = new FactsIndex(chunk['facts.obs'], chunk['facts.index'], chunk['facts.world'], chunk['facts.notes'] ?? null);
    factsCache.set(chunk, f);
  }
  return f;
}

export function seriesIndexOf(chunk: SectorSeriesChunk): FactsIndex {
  let f = factsCache.get(chunk);
  if (!f) {
    f = new FactsIndex(chunk['series.obs'], chunk['series.index'], chunk['series.world']);
    factsCache.set(chunk, f);
  }
  return f;
}

/** IMF monthly prices of one commodity item: [{year, month, value, status}] in time order. */
export function monthlyOf(chunk: SectorSeriesChunk, v: number, item: number): { year: number; month: number; t: number; value: number; status: StatusCode }[] {
  const m = chunk['series.monthly'];
  const c = m.cols;
  const out: { year: number; month: number; t: number; value: number; status: StatusCode }[] = [];
  for (let i = 0; i < m.nrow; i++) {
    if (c.var[i] !== v || c.item[i] !== item) continue;
    out.push({ year: c.year[i], month: c.month[i], t: c.year[i] + (c.month[i] - 0.5) / 12, value: c.value[i], status: (c.status[i] ?? 'obs') as StatusCode });
  }
  return out;
}
