// Climate TRACE sector emissions mapped to technology x stage (sector-climate; climate data change
// 2026-10-17). Pure: indexes the chunk, and derives the rows the Sector Profile's Climate lens and
// section and the Country Profile's Policy & climate section show. Values are shown as shipped
// (tonnes CO2e, 100-year GWP); nothing is summed across sources of different match quality, and a
// context source (a material the sector's products are made of) is never counted as the sector's own.

import type { ClimateMatch, SectorClimateChunk } from '../contract.ts';
import type { GeoInfo, SectorModel } from '../data/model.ts';
import { fmtSig } from '../format.ts';
import { nzeData, type NzeData } from './nze.ts';

export interface ClimateSrc {
  idx: number;
  id: string;
  subsector: string;
  sector: string;
  label: string;
  /** null: the whole subsector (country totals); else the asset group summed (assets basis) */
  group: string | null;
  basis: 'country' | 'assets';
  /** the latest year's world total repeats the year before (an estimate carried forward) */
  repeated: boolean;
}

export interface ClimateLink {
  src: ClimateSrc;
  role: 'own' | 'context';
  /** null for context */
  match: Exclude<ClimateMatch, 'none'> | null;
}

export interface ClimateLifecycle {
  tech: string;
  basis: 'ipcc' | 'proxy';
  gMin: number | null;
  gMax: number | null;
  rows: string | null;
  proxyOf: string[];
}

export interface IntensityCell {
  value: number;
  n: number;
  emissions: number;
  activity: number;
  unit: string;
  activityUnit: string;
}

export interface ClimateData {
  srcs: ClimateSrc[];
  years: number[];
  latest: number;
  first: number;
  /** cell item idx -> its sources in map order */
  links: Map<number, ClimateLink[]>;
  /** cell item idx -> match quality and note */
  cells: Map<number, { match: ClimateMatch; note: string }>;
  lifecycle: Map<string, ClimateLifecycle>;
  /** src idx -> year -> geo idx -> t CO2e */
  em: Map<number, Map<number, Map<number, number>>>;
  /** "subsector|group" -> year -> geo idx -> intensity */
  intensity: Map<string, Map<number, Map<number, IntensityCell>>>;
  /** the IEA pathways (climate.nze_*; null when the build has none) */
  nze: NzeData | null;
}

const num = (x: number): number | null => (Number.isFinite(x) ? x : null);

/** Index a decoded sector-climate chunk. */
export function climateData(c: SectorClimateChunk): ClimateData {
  const s = c['climate.src'];
  const srcs: ClimateSrc[] = [];
  for (let i = 0; i < s.nrow; i++) {
    srcs.push({
      idx: i, id: s.cols.id[i] ?? '', subsector: s.cols.subsector[i] ?? '', sector: s.cols.sector[i] ?? '', label: s.cols.label[i] ?? '',
      group: s.cols.asset_group[i], basis: s.cols.basis[i] === 'assets' ? 'assets' : 'country', repeated: s.cols.repeated[i] === 1,
    });
  }
  const links = new Map<number, ClimateLink[]>();
  const m = c['climate.map'];
  for (let i = 0; i < m.nrow; i++) {
    const item = m.cols.item[i];
    const l = links.get(item) ?? [];
    l.push({ src: srcs[m.cols.src[i]], role: m.cols.role[i] === 'context' ? 'context' : 'own', match: (m.cols.match[i] as ClimateLink['match']) ?? null });
    links.set(item, l);
  }
  const cells = new Map<number, { match: ClimateMatch; note: string }>();
  const cc = c['climate.cells'];
  for (let i = 0; i < cc.nrow; i++) cells.set(cc.cols.item[i], { match: (cc.cols.match[i] ?? 'none') as ClimateMatch, note: cc.cols.note[i] ?? '' });
  const lifecycle = new Map<string, ClimateLifecycle>();
  const lc = c['climate.lifecycle'];
  for (let i = 0; i < lc.nrow; i++) {
    const tech = lc.cols.tech[i] ?? '';
    lifecycle.set(tech, {
      tech, basis: lc.cols.basis[i] === 'proxy' ? 'proxy' : 'ipcc', gMin: num(lc.cols.g_min[i]), gMax: num(lc.cols.g_max[i]), rows: lc.cols.rows[i],
      proxyOf: (lc.cols.proxy_of[i] ?? '').split('|').filter(Boolean),
    });
  }
  const em = new Map<number, Map<number, Map<number, number>>>();
  const years = new Set<number>();
  const e = c['climate.em'];
  for (let i = 0; i < e.nrow; i++) {
    const src = e.cols.src[i];
    const y = e.cols.year[i];
    years.add(y);
    let bySrc = em.get(src);
    if (!bySrc) em.set(src, (bySrc = new Map()));
    let byYear = bySrc.get(y);
    if (!byYear) bySrc.set(y, (byYear = new Map()));
    byYear.set(e.cols.geo[i], e.cols.value[i]);
  }
  const intensity = new Map<string, Map<number, Map<number, IntensityCell>>>();
  const it = c['climate.intensity'];
  for (let i = 0; i < it.nrow; i++) {
    const k = `${it.cols.subsector[i]}|${it.cols.asset_group[i]}`;
    let byK = intensity.get(k);
    if (!byK) intensity.set(k, (byK = new Map()));
    const y = it.cols.year[i];
    let byYear = byK.get(y);
    if (!byYear) byK.set(y, (byYear = new Map()));
    byYear.set(it.cols.geo[i], {
      value: it.cols.value[i], n: it.cols.n[i], emissions: it.cols.emissions[i], activity: it.cols.activity[i], unit: it.cols.unit[i] ?? '', activityUnit: it.cols.activity_unit[i] ?? '',
    });
  }
  const ys = [...years].sort((a, z) => a - z);
  return { srcs, years: ys, latest: ys[ys.length - 1] ?? NaN, first: ys[0] ?? NaN, links, cells, lifecycle, em, intensity, nze: nzeData(c) };
}

/** The cell's match quality (none when the chunk has no row for it). */
export function cellMatch(d: ClimateData, cellIdx: number): { match: ClimateMatch; note: string } {
  return d.cells.get(cellIdx) ?? { match: 'none', note: '' };
}

export const ownLinks = (d: ClimateData, cellIdx: number): ClimateLink[] => (d.links.get(cellIdx) ?? []).filter((l) => l.role === 'own');
export const contextLinks = (d: ClimateData, cellIdx: number): ClimateLink[] => (d.links.get(cellIdx) ?? []).filter((l) => l.role === 'context');

/** A source's value for one geo and year (null when not shipped). */
export function emOf(d: ClimateData, src: number, geo: number, year: number): number | null {
  const v = d.em.get(src)?.get(year)?.get(geo);
  return v === undefined || !Number.isFinite(v) ? null : v;
}

export interface EmitterRow {
  g: GeoInfo;
  value: number;
  /** share of the world total (null when the world total is not positive) */
  share: number | null;
  /** value in the first year, for the change since then */
  first: number | null;
  change: number | null;
  rank: number;
}

/**
 * Economies with emissions above zero from one source in `year`, largest first (Climate TRACE gives every
 * country a value; a zero means it estimates none, and such economies are not ranked). The EU has no
 * Climate TRACE row; its members have their own.
 */
export function emitters(d: ClimateData, model: SectorModel, src: number, year: number): { rows: EmitterRow[]; world: number | null; worldFirst: number | null } {
  const byGeo = d.em.get(src)?.get(year);
  const firstBy = d.em.get(src)?.get(d.first);
  const w = model.world ? (byGeo?.get(model.world.idx) ?? null) : null;
  const wf = model.world ? (firstBy?.get(model.world.idx) ?? null) : null;
  const rows: EmitterRow[] = [];
  if (byGeo) {
    for (const [gi, v] of byGeo) {
      const g = model.geos[gi];
      if (!g || g.kind !== 'country' || !Number.isFinite(v) || v <= 0) continue;
      const f = firstBy?.get(gi);
      const first = f === undefined || !Number.isFinite(f) ? null : f;
      rows.push({ g, value: v, share: w !== null && w > 0 ? v / w : null, first, change: first !== null && first > 0 ? v / first - 1 : null, rank: 0 });
    }
  }
  rows.sort((a, z) => z.value - a.value || a.g.name.localeCompare(z.g.name, 'en'));
  let r = 0;
  let prev = NaN;
  rows.forEach((x, i) => {
    if (x.value !== prev) r = i + 1;
    prev = x.value;
    x.rank = r;
  });
  return { rows, world: w, worldFirst: wf };
}

/** The world total of a source by year (the trend). */
export function worldTrend(d: ClimateData, model: SectorModel, src: number): { year: number; value: number }[] {
  if (!model.world) return [];
  const out: { year: number; value: number }[] = [];
  for (const y of d.years) {
    const v = d.em.get(src)?.get(y)?.get(model.world.idx);
    if (v !== undefined && Number.isFinite(v)) out.push({ year: y, value: v });
  }
  return out;
}

/** The intensity groups of a source: its own group (assets basis), or every group of its subsector (country basis). */
export function intensityKeys(d: ClimateData, s: ClimateSrc): { key: string; group: string }[] {
  const out: { key: string; group: string }[] = [];
  for (const k of d.intensity.keys()) {
    const [sub, group] = k.split('|');
    if (sub !== s.subsector) continue;
    if (s.group !== null && group !== s.group) continue;
    out.push({ key: k, group });
  }
  return out.sort((a, z) => a.group.localeCompare(z.group, 'en'));
}

export function intensityOf(d: ClimateData, key: string, geo: number, year: number): IntensityCell | null {
  return d.intensity.get(key)?.get(year)?.get(geo) ?? null;
}

/** Tonnes CO2e in words: "1.62 Gt", "87.1 Mt", "450 kt", "820 t". */
export function fmtTonnes(t: number | null | undefined): string {
  if (t === null || t === undefined || !Number.isFinite(t)) return '–';
  const a = Math.abs(t);
  if (a >= 1e9) return `${fmtSig(t / 1e9)} Gt`;
  if (a >= 1e6) return `${fmtSig(t / 1e6)} Mt`;
  if (a >= 1e3) return `${fmtSig(t / 1e3)} kt`;
  return `${fmtSig(t)} t`;
}

/** An intensity value with its unit: "0.95 t CO2e per MWh". */
export function fmtIntensity(x: IntensityCell | null): string {
  if (!x || !Number.isFinite(x.value)) return '–';
  return `${fmtSig(x.value)} ${x.unit}`;
}

/** The IPCC lifecycle range in words: "820 gCO2e/kWh", "27–48 gCO2e/kWh". */
export function fmtLifecycle(l: ClimateLifecycle | null | undefined): string | null {
  if (!l || l.basis !== 'ipcc' || l.gMin === null || l.gMax === null) return null;
  return l.gMin === l.gMax ? `${fmtSig(l.gMin)} gCO2e/kWh` : `${fmtSig(l.gMin)}–${fmtSig(l.gMax)} gCO2e/kWh`;
}

/** Plain words for a match quality. */
export const MATCH_WORDS: Record<ClimateMatch, { label: string; text: string }> = {
  direct: { label: 'Direct match', text: 'The Climate TRACE subsector is this sector’s own activity.' },
  partial: { label: 'Partial match', text: 'The Climate TRACE subsector covers this sector’s activity but also other activity, or only part of it.' },
  proxy: { label: 'Proxy', text: 'A related activity, shown for scale: not this sector’s own emissions.' },
  none: { label: 'No match', text: 'Climate TRACE has no subsector for this sector; the technology’s IPCC lifecycle intensity is shown instead.' },
};

/** The country's rows: every source the map uses, with its value, share, rank and the cells it maps to. */
export interface CountryClimateRow {
  src: ClimateSrc;
  value: number | null;
  share: number | null;
  rank: number | null;
  n: number;
  change: number | null;
  cells: { idx: number; label: string; role: 'own' | 'context'; match: Exclude<ClimateMatch, 'none'> | null }[];
}

export function countryClimateRows(d: ClimateData, model: SectorModel, g: GeoInfo): CountryClimateRow[] {
  const out: CountryClimateRow[] = [];
  for (const s of d.srcs) {
    const e = emitters(d, model, s.idx, d.latest);
    const mine = e.rows.find((r) => r.g.idx === g.idx) ?? null;
    const own = emOf(d, s.idx, g.idx, d.latest);
    const cells: CountryClimateRow['cells'] = [];
    for (const [ci, ls] of d.links) {
      for (const l of ls) if (l.src.idx === s.idx) cells.push({ idx: ci, label: model.items[ci] ? model.cellLabel(model.items[ci]) : String(ci), role: l.role, match: l.match });
    }
    cells.sort((a, z) => (model.items[a.idx]?.sort ?? 0) - (model.items[z.idx]?.sort ?? 0));
    out.push({ src: s, value: own, share: mine?.share ?? (own === 0 ? 0 : null), rank: mine?.rank ?? null, n: e.rows.length, change: mine?.change ?? null, cells });
  }
  return out;
}
