// Climate sources (the lazy sector-climate-src chunk; user request 2026-10-17: seven files added to
// data/raw for the Climate section). Pure: indexes the tables and derives the rows the Country Profile's
// Policy & climate section (an economy's fossil CO2, its transition indicators against the median
// economy, its WEO region's pathway) and the Sector Profile's Climate section (2030 milestones of the
// 2021 Net Zero roadmap, energy investment by region, World fossil CO2 by fuel) show. Values are shown
// as shipped; the only computed figures are changes and ratios between shipped values, the low-carbon
// share of power (a sum of shares of the same generation) and the renewables share of a region's
// generation (a ratio of two published values). GCP and EI values are the publishers' estimates of the
// past (observed facts); IEA values are scenario projections or the IEA's estimates, labelled as such.

import type { ClimateMatch, NzeScenario, SectorClimateSrcChunk, SrcFuel, SrcGeoMatch, SrcMeasure } from '../contract.ts';
import { fmtSig, lc } from '../format.ts';

export interface EiInd {
  id: string;
  label: string;
  unit: string;
  better: 'lower' | 'higher' | 'none';
  lowCarbon: boolean;
}

export interface Nze21Series {
  idx: number;
  id: string;
  label: string;
  unit: string;
  /** the WEO 2025 World series (climate.nze_series id) it is paired with */
  weo: string;
  match: 'direct' | 'partial';
  note: string | null;
}

export interface WeiRow {
  idx: number;
  id: string;
  label: string;
  name: string;
  parent: string | null;
  unit: string;
}

export interface WeiPoint {
  year: number;
  value: number;
  estimate: boolean;
}

export interface SrcLink {
  src: 'wei' | 'gcp';
  key: string;
  role: 'own' | 'demand' | 'context';
  match: Exclude<ClimateMatch, 'none'> | null;
}

export interface ClimateSrc {
  /** geo idx -> measure -> year -> Mt CO2 */
  gcp: Map<number, Map<SrcMeasure, Map<number, number>>>;
  /** fuel -> year -> Gt CO2 (World) */
  gcpFuel: Map<SrcFuel, Map<number, number>>;
  /** ind -> year -> geo idx -> value */
  ei: Map<string, Map<number, Map<number, number>>>;
  eiInd: EiInd[];
  /** ind -> year -> median and n */
  eiMedian: Map<string, Map<number, { value: number; n: number }>>;
  weoSeries: { idx: number; id: string; label: string; unit: string }[];
  /** region -> series idx -> scenario -> year -> value */
  weo: Map<string, Map<number, Map<NzeScenario, Map<number, number>>>>;
  weoRegionKind: Map<string, string>;
  /** geo idx -> its WEO region */
  weoGeo: Map<number, { region: string; match: SrcGeoMatch }>;
  nze21: Nze21Series[];
  /** series idx -> year -> value */
  nze21Values: Map<number, Map<number, number>>;
  weiRows: WeiRow[];
  /** row idx -> region -> points by year */
  wei: Map<number, Map<string, WeiPoint[]>>;
  weiRegions: { region: string; label: string; kind: string }[];
  links: Map<number, SrcLink[]>;
  cells: Map<number, { match: ClimateMatch; note: string }>;
  meta: Map<string, string | null>;
}

/** The year of the 2021 roadmap's milestones. */
export const NZE21_YEAR = 2030;

/** Index a decoded sector-climate-src chunk. */
export function climateSrcData(c: SectorClimateSrcChunk): ClimateSrc {
  const g = c['csrc.gcp'];
  const gcp = new Map<number, Map<SrcMeasure, Map<number, number>>>();
  for (let i = 0; i < g.nrow; i++) {
    const geo = g.cols.geo[i];
    const m = g.cols.measure[i] as SrcMeasure;
    let a = gcp.get(geo);
    if (!a) gcp.set(geo, (a = new Map()));
    let b = a.get(m);
    if (!b) a.set(m, (b = new Map()));
    b.set(g.cols.year[i], g.cols.value[i]);
  }
  const f = c['csrc.gcp_fuel'];
  const gcpFuel = new Map<SrcFuel, Map<number, number>>();
  for (let i = 0; i < f.nrow; i++) {
    const k = f.cols.fuel[i] as SrcFuel;
    let a = gcpFuel.get(k);
    if (!a) gcpFuel.set(k, (a = new Map()));
    a.set(f.cols.year[i], f.cols.value[i]);
  }
  const e = c['csrc.ei'];
  const ei = new Map<string, Map<number, Map<number, number>>>();
  for (let i = 0; i < e.nrow; i++) {
    const k = e.cols.ind[i] ?? '';
    let a = ei.get(k);
    if (!a) ei.set(k, (a = new Map()));
    let b = a.get(e.cols.year[i]);
    if (!b) a.set(e.cols.year[i], (b = new Map()));
    b.set(e.cols.geo[i], e.cols.value[i]);
  }
  const it = c['csrc.ei_ind'];
  const eiInd: EiInd[] = [];
  for (let i = 0; i < it.nrow; i++) {
    eiInd.push({ id: it.cols.id[i] ?? '', label: it.cols.label[i] ?? '', unit: it.cols.unit[i] ?? '', better: (it.cols.better[i] ?? 'none') as EiInd['better'], lowCarbon: it.cols.low_carbon[i] === 1 });
  }
  const md = c['csrc.ei_median'];
  const eiMedian = new Map<string, Map<number, { value: number; n: number }>>();
  for (let i = 0; i < md.nrow; i++) {
    const k = md.cols.ind[i] ?? '';
    let a = eiMedian.get(k);
    if (!a) eiMedian.set(k, (a = new Map()));
    a.set(md.cols.year[i], { value: md.cols.value[i], n: md.cols.n[i] });
  }
  const ws = c['csrc.weo_series'];
  const weoSeries = Array.from({ length: ws.nrow }, (_, i) => ({ idx: i, id: ws.cols.id[i] ?? '', label: ws.cols.label[i] ?? '', unit: ws.cols.unit[i] ?? '' }));
  const wv = c['csrc.weo_values'];
  const weo = new Map<string, Map<number, Map<NzeScenario, Map<number, number>>>>();
  for (let i = 0; i < wv.nrow; i++) {
    const r = wv.cols.region[i] ?? '';
    const s = wv.cols.series[i];
    const sc = wv.cols.scenario[i] as NzeScenario;
    let a = weo.get(r);
    if (!a) weo.set(r, (a = new Map()));
    let b = a.get(s);
    if (!b) a.set(s, (b = new Map()));
    let d = b.get(sc);
    if (!d) b.set(sc, (d = new Map()));
    d.set(wv.cols.year[i], wv.cols.value[i]);
  }
  const wr = c['csrc.weo_regions'];
  const weoRegionKind = new Map<string, string>();
  for (let i = 0; i < wr.nrow; i++) weoRegionKind.set(wr.cols.region[i] ?? '', wr.cols.kind[i] ?? 'region');
  const wg = c['csrc.weo_geo'];
  const weoGeo = new Map<number, { region: string; match: SrcGeoMatch }>();
  for (let i = 0; i < wg.nrow; i++) weoGeo.set(wg.cols.geo[i], { region: wg.cols.region[i] ?? '', match: (wg.cols.match[i] ?? 'member') as SrcGeoMatch });
  const ns = c['csrc.nze21_series'];
  const nze21: Nze21Series[] = Array.from({ length: ns.nrow }, (_, i) => ({
    idx: i, id: ns.cols.id[i] ?? '', label: ns.cols.label[i] ?? '', unit: ns.cols.unit[i] ?? '', weo: ns.cols.weo[i] ?? '',
    match: (ns.cols.match[i] ?? 'direct') as Nze21Series['match'], note: ns.cols.note[i],
  }));
  const nv = c['csrc.nze21_values'];
  const nze21Values = new Map<number, Map<number, number>>();
  for (let i = 0; i < nv.nrow; i++) {
    let a = nze21Values.get(nv.cols.series[i]);
    if (!a) nze21Values.set(nv.cols.series[i], (a = new Map()));
    a.set(nv.cols.year[i], nv.cols.value[i]);
  }
  const rr = c['csrc.wei_rows'];
  const weiRows: WeiRow[] = Array.from({ length: rr.nrow }, (_, i) => ({
    idx: i, id: rr.cols.id[i] ?? '', label: rr.cols.label[i] ?? '', name: rr.cols.name[i] ?? '', parent: rr.cols.parent[i], unit: rr.cols.unit[i] ?? '',
  }));
  const iv = c['csrc.wei_values'];
  const wei = new Map<number, Map<string, WeiPoint[]>>();
  for (let i = 0; i < iv.nrow; i++) {
    let a = wei.get(iv.cols.row[i]);
    if (!a) wei.set(iv.cols.row[i], (a = new Map()));
    const r = iv.cols.region[i] ?? '';
    let b = a.get(r);
    if (!b) a.set(r, (b = []));
    b.push({ year: iv.cols.year[i], value: iv.cols.value[i], estimate: iv.cols.estimate[i] === 1 });
  }
  for (const a of wei.values()) for (const b of a.values()) b.sort((x, z) => x.year - z.year);
  const rg = c['csrc.wei_regions'];
  const weiRegions = Array.from({ length: rg.nrow }, (_, i) => ({ region: rg.cols.region[i] ?? '', label: rg.cols.label[i] ?? '', kind: rg.cols.kind[i] ?? 'region' }));
  const mp = c['csrc.map'];
  const links = new Map<number, SrcLink[]>();
  for (let i = 0; i < mp.nrow; i++) {
    const l = links.get(mp.cols.item[i]) ?? [];
    l.push({ src: (mp.cols.src[i] ?? 'wei') as SrcLink['src'], key: mp.cols.key[i] ?? '', role: (mp.cols.role[i] ?? 'context') as SrcLink['role'], match: (mp.cols.match[i] as SrcLink['match']) ?? null });
    links.set(mp.cols.item[i], l);
  }
  const cc = c['csrc.cells'];
  const cells = new Map<number, { match: ClimateMatch; note: string }>();
  for (let i = 0; i < cc.nrow; i++) cells.set(cc.cols.item[i], { match: (cc.cols.match[i] ?? 'none') as ClimateMatch, note: cc.cols.note[i] ?? '' });
  const mt = c['csrc.meta'];
  const meta = new Map<string, string | null>();
  for (let i = 0; i < mt.nrow; i++) meta.set(mt.cols.key[i] ?? '', mt.cols.value[i]);
  return { gcp, gcpFuel, ei, eiInd, eiMedian, weoSeries, weo, weoRegionKind, weoGeo, nze21, nze21Values, weiRows, wei, weiRegions, links, cells, meta };
}

/** A signed percentage change: "−17%", "+4%". */
export function pctChange(now: number | null, then: number | null): number | null {
  return now !== null && then !== null && then > 0 ? now / then - 1 : null;
}
export function fmtPct(c: number | null): string {
  if (c === null || !Number.isFinite(c)) return '–';
  const p = Math.round(c * 100);
  return `${p > 0 ? '+' : p < 0 ? '−' : ''}${Math.abs(p)}%`;
}
/** "961 Mt", "1,140 Mt", "38.6 Gt". */
export function fmtMt(v: number | null | undefined): string {
  if (v === null || v === undefined || !Number.isFinite(v)) return '–';
  return Math.abs(v) >= 10_000 ? `${fmtSig(v / 1000)} Gt` : `${fmtSig(v)} Mt`;
}

// ---- Country: fossil CO2 (GCP) -------------------------------------------------------------------------

export interface GcpCountry {
  /** [year, Mt CO2] ascending */
  territorial: [number, number][];
  consumption: [number, number][];
  transfers: [number, number][];
  last: { year: number; value: number } | null;
  lastCons: { year: number; value: number; transfer: number | null } | null;
  /** territorial: latest against 1990 and against 2015 */
  since1990: number | null;
  since2015: number | null;
  /** latest territorial as a share of the World total (which also counts aviation and shipping bunkers) */
  worldShare: number | null;
}

const series = (m: Map<number, number> | undefined): [number, number][] => [...(m ?? new Map<number, number>())].sort((a, z) => a[0] - z[0]);

export function gcpCountry(d: ClimateSrc, geo: number, world: number | null): GcpCountry | null {
  const g = d.gcp.get(geo);
  if (!g) return null;
  const territorial = series(g.get('territorial'));
  const consumption = series(g.get('consumption'));
  const transfers = series(g.get('transfers'));
  const lt = territorial[territorial.length - 1];
  const lc_ = consumption[consumption.length - 1];
  const at = (s: [number, number][], y: number) => s.find((x) => x[0] === y)?.[1] ?? null;
  const w = world === null ? null : at(series(d.gcp.get(world)?.get('territorial')), lt?.[0] ?? NaN);
  return {
    territorial, consumption, transfers,
    last: lt ? { year: lt[0], value: lt[1] } : null,
    lastCons: lc_ ? { year: lc_[0], value: lc_[1], transfer: at(transfers, lc_[0]) } : null,
    since1990: lt ? pctChange(lt[1], at(territorial, 1990)) : null,
    since2015: lt ? pctChange(lt[1], at(territorial, 2015)) : null,
    worldShare: lt && w !== null && w > 0 ? lt[1] / w : null,
  };
}

/** The card's headline: territorial CO2, its change, and what the consumption-based figure adds. */
export function gcpHeadline(c: GcpCountry, name: string): string | null {
  if (!c.last) return null;
  const parts = [`${name}’s fossil CO2 emissions were ${fmtMt(c.last.value)} in ${c.last.year}`];
  const ch: string[] = [];
  if (c.since1990 !== null) ch.push(`${fmtPct(c.since1990)} since 1990`);
  if (c.since2015 !== null) ch.push(`${fmtPct(c.since2015)} since 2015`);
  let s = `${parts[0]}${ch.length ? ` (${ch.join(', ')})` : ''}`;
  if (c.worldShare !== null) s += `, ${(c.worldShare * 100).toFixed(c.worldShare < 0.01 ? 2 : 1)}% of the world’s`;
  s += '.';
  if (c.lastCons && c.lastCons.transfer !== null) {
    // The GCP publishes transfers as territorial minus consumption-based emissions: negative for a net
    // importer of the CO2 embodied in goods.
    const t = c.lastCons.transfer;
    const way = t < 0 ? `more than it emits at home: the CO2 in its imports exceeds that in its exports by ${fmtMt(-t)}` : t > 0 ? `less than it emits at home: the CO2 in its exports exceeds that in its imports by ${fmtMt(t)}` : 'the same as it emits at home';
    s += ` Counting the CO2 in what it buys from abroad instead (consumption-based), its emissions were ${fmtMt(c.lastCons.value)} in ${c.lastCons.year}, ${way}.`;
  }
  return s;
}

// ---- Country: transition indicators (EI) -----------------------------------------------------------------

export interface EiRow {
  ind: EiInd;
  year: number;
  value: number | null;
  median: number | null;
  n: number | null;
  /** the economy against the median: 'better' / 'worse' / 'level' (null when the direction is none or the indicator is a national total) */
  vsMedian: 'better' | 'worse' | 'level' | null;
  prevYear: number | null;
  prev: number | null;
  change: number | null;
}

/** The latest year of an indicator, and the one before it in the tracker (2017 for 2024). */
function eiYears(d: ClimateSrc, id: string): [number | null, number | null] {
  const ys = [...(d.ei.get(id)?.keys() ?? [])].sort((a, z) => a - z);
  return [ys[ys.length - 1] ?? null, ys[ys.length - 2] ?? null];
}

/**
 * The tracker's indicators for one economy (latest year, the median economy that year, the previous
 * tracker year and the change), plus the power mix's low-carbon share (a sum of the low-carbon sources'
 * shares of the same year's generation). Indicators the tracker does not report for the economy are left
 * out; the low-carbon share only when every low-carbon source is reported.
 */
/**
 * Tracker indicators that are national totals (emissions in Mt, fuel use in EJ). They grow with the size of
 * the economy, so being above or below the median economy says nothing about its transition: they are not
 * ranked against the median (their change over time is still shown).
 */
export const EI_TOTALS: ReadonlySet<string> = new Set(['ghg', 'fossil']);

export function eiRows(d: ClimateSrc, geo: number): EiRow[] {
  const out: EiRow[] = [];
  const cmp = (ind: EiInd, v: number | null, m: number | null): EiRow['vsMedian'] => {
    if (ind.better === 'none' || EI_TOTALS.has(ind.id) || v === null || m === null) return null;
    if (Math.abs(v - m) <= 1e-9 * Math.max(1, Math.abs(m))) return 'level';
    return (ind.better === 'lower') === v < m ? 'better' : 'worse';
  };
  for (const ind of d.eiInd) {
    if (ind.id.startsWith('power_')) continue;
    const [y, py] = eiYears(d, ind.id);
    if (y === null) continue;
    const v = d.ei.get(ind.id)?.get(y)?.get(geo) ?? null;
    if (v === null) continue;
    const m = d.eiMedian.get(ind.id)?.get(y) ?? null;
    const prev = py === null ? null : (d.ei.get(ind.id)?.get(py)?.get(geo) ?? null);
    out.push({ ind, year: y, value: v, median: m?.value ?? null, n: m?.n ?? null, vsMedian: cmp(ind, v, m?.value ?? null), prevYear: py, prev, change: ind.unit === 'share' ? null : pctChange(v, prev) });
  }
  const low = d.eiInd.filter((x) => x.lowCarbon);
  if (low.length) {
    const [y] = eiYears(d, low[0].id);
    if (y !== null) {
      const vals = low.map((x) => d.ei.get(x.id)?.get(y)?.get(geo) ?? null);
      if (vals.every((v) => v !== null)) {
        const ind: EiInd = { id: 'power_low_carbon', label: 'Low-carbon share of power generation', unit: 'share', better: 'higher', lowCarbon: false };
        const v = (vals as number[]).reduce((a, b) => a + b, 0);
        // The median economy of the sum is not the sum of the medians: computed from every listed economy.
        const all = d.ei.get(low[0].id)?.get(y);
        const sums: number[] = [];
        for (const g of all?.keys() ?? []) {
          const xs = low.map((x) => d.ei.get(x.id)?.get(y)?.get(g) ?? null);
          if (xs.every((q) => q !== null)) sums.push((xs as number[]).reduce((a, b) => a + b, 0));
        }
        const m = median(sums);
        out.push({ ind, year: y, value: v, median: m, n: sums.length || null, vsMedian: cmp(ind, v, m), prevYear: null, prev: null, change: null });
      }
    }
  }
  return out;
}

export function median(xs: number[]): number | null {
  if (!xs.length) return null;
  const s = [...xs].sort((a, z) => a - z);
  const h = Math.floor(s.length / 2);
  return s.length % 2 ? s[h] : (s[h - 1] + s[h]) / 2;
}

/** An EI value in its unit: "7.8%", "62.7 t CO2e per TJ", "14.2 EJ". */
export function fmtEi(v: number | null, unit: string): string {
  if (v === null || !Number.isFinite(v)) return '–';
  if (unit === 'share') return `${(v * 100).toFixed(v < 0.1 ? 1 : 0)}%`;
  return `${fmtSig(v)} ${unit}`;
}

// ---- Country: the WEO region's pathway -------------------------------------------------------------------

export interface WeoRegionView {
  region: string;
  kind: string;
  match: SrcGeoMatch;
  /** the region's sentence of how the economy maps */
  mapping: string;
}

export function weoRegionOf(d: ClimateSrc, geo: number, name: string): WeoRegionView | null {
  const g = d.weoGeo.get(geo);
  if (!g || !d.weo.has(g.region)) return null;
  const kind = d.weoRegionKind.get(g.region) ?? 'region';
  const mapping =
    g.match === 'own'
      ? g.region === 'China'
        ? `The IEA models ${name} as its own region (with Hong Kong).`
        : `The IEA models ${name} as its own region.`
      : g.match === 'member'
        ? kind === 'union'
          ? `${name} is a member of the ${g.region}, so this is the ${g.region}’s pathway, not ${name}’s.`
          : `${name} is one of the economies of the IEA’s ${g.region} region, so this is the region’s pathway, not ${name}’s.`
        : `The IEA does not name ${name} in a region; it is counted with the ${g.region} region’s “other” economies (assigned here by its UN sub-region), so this is the region’s pathway, not ${name}’s.`;
  return { region: g.region, kind, match: g.match, mapping };
}

export interface WeoRow {
  id: string;
  label: string;
  unit: string;
  base: number | null;
  baseYear: number | null;
  stated35: number | null;
  current35: number | null;
  stated50: number | null;
  current50: number | null;
}

const at = (m: Map<number, number> | undefined, y: number): number | null => {
  const v = m?.get(y);
  return v === undefined || !Number.isFinite(v) ? null : v;
};

/** The region's series: the last historical year, then Stated and Current Policies in 2035 and 2050. */
export function weoRegionRows(d: ClimateSrc, region: string): WeoRow[] {
  const r = d.weo.get(region);
  if (!r) return [];
  const out: WeoRow[] = [];
  for (const s of d.weoSeries) {
    const by = r.get(s.idx);
    if (!by) continue;
    const hist = by.get('historical');
    const by_ = hist ? Math.max(...hist.keys()) : null;
    out.push({
      id: s.id, label: s.label, unit: s.unit, baseYear: by_, base: by_ === null ? null : at(hist, by_),
      stated35: at(by.get('stated'), 2035), current35: at(by.get('current'), 2035), stated50: at(by.get('stated'), 2050), current50: at(by.get('current'), 2050),
    });
  }
  // The renewables share of generation: a ratio of two published values of the same scenario and year.
  const rn = out.find((x) => x.id === 'gen_renewables');
  const tt = out.find((x) => x.id === 'gen_total');
  if (rn && tt) {
    const q = (a: number | null, b: number | null) => (a !== null && b !== null && b > 0 ? a / b : null);
    out.push({
      id: 'renewables_share', label: 'Renewables’ share of electricity generation', unit: 'share', baseYear: rn.baseYear, base: q(rn.base, tt.base),
      stated35: q(rn.stated35, tt.stated35), current35: q(rn.current35, tt.current35), stated50: q(rn.stated50, tt.stated50), current50: q(rn.current50, tt.current50),
    });
  }
  return out;
}

/** One series of a region as lines: Historical, then Stated and Current Policies from the last historical year. */
export function weoRegionLines(d: ClimateSrc, region: string, id: string): { scenario: NzeScenario; points: { x: number; y: number; projection: boolean }[] }[] {
  const s = d.weoSeries.find((x) => x.id === id);
  const by = s ? d.weo.get(region)?.get(s.idx) : undefined;
  if (!by) return [];
  const hist = series(by.get('historical')).map(([x, y]) => ({ x, y, projection: false }));
  const last = hist[hist.length - 1];
  const out: { scenario: NzeScenario; points: { x: number; y: number; projection: boolean }[] }[] = [];
  if (hist.length) out.push({ scenario: 'historical', points: hist });
  for (const sc of ['stated', 'current'] as const) {
    const pts = series(by.get(sc)).map(([x, y]) => ({ x, y, projection: true }));
    if (pts.length) out.push({ scenario: sc, points: last ? [last, ...pts] : pts });
  }
  return out;
}

/** The regional sentence: energy-sector CO2 at the last estimate and in 2035 and 2050 under both scenarios. */
/** The region in prose: "Japan" (its own region), "the European Union", "the Southeast Asia region". */
export function regionWords(d: ClimateSrc, region: string): string {
  const k = d.weoRegionKind.get(region);
  return k === 'country' || k === 'world' ? region : k === 'union' ? `the ${region}` : `the ${region} region`;
}

export function weoRegionLine(d: ClimateSrc, region: string): string | null {
  const r = weoRegionRows(d, region).find((x) => x.id === 'co2_energy');
  if (!r || r.base === null || r.stated35 === null) return null;
  const ch = pctChange(r.stated35, r.base);
  const w = regionWords(d, region);
  return `${w.charAt(0).toUpperCase()}${w.slice(1)}’s energy-sector CO2 was ${fmtMt(r.base)} in ${r.baseYear} (IEA estimate). The IEA’s scenarios project ${fmtMt(r.stated35)} in 2035 under Stated Policies (${fmtPct(ch)})${r.current35 !== null ? ` and ${fmtMt(r.current35)} under Current Policies` : ''}; in 2050, ${fmtMt(r.stated50)} and ${fmtMt(r.current50)}.`;
}

// ---- Sector: 2030 milestones of the 2021 Net Zero roadmap ----------------------------------------------------

export interface Nze21Milestone {
  series: Nze21Series;
  value: number | null;
  /** the roadmap's own base year (2020) value */
  base: number | null;
  baseYear: number | null;
}

/** The 2021 roadmap's milestone for a WEO 2025 series id (null when no 2021 row is paired with it). */
export function nze21For(d: ClimateSrc, weoId: string, year = NZE21_YEAR): Nze21Milestone | null {
  const s = d.nze21.find((x) => x.weo === weoId);
  if (!s) return null;
  const v = d.nze21Values.get(s.idx);
  const ys = [...(v?.keys() ?? [])].filter((y) => y < year).sort((a, z) => a - z);
  const by = ys[ys.length - 1] ?? null;
  return { series: s, value: at(v, year), base: by === null ? null : at(v, by), baseYear: by };
}

/** "4,960 GW" in the milestone's own unit. */
export const fmtUnit = (v: number | null, unit: string): string => (v === null || !Number.isFinite(v) ? '–' : `${fmtSig(v)} ${unit}`);

// ---- Sector: energy investment (WEI) -------------------------------------------------------------------------

export const srcLinks = (d: ClimateSrc, cellIdx: number): SrcLink[] => d.links.get(cellIdx) ?? [];
export const srcCell = (d: ClimateSrc, cellIdx: number): { match: ClimateMatch; note: string } => d.cells.get(cellIdx) ?? { match: 'none', note: '' };
export const weiRow = (d: ClimateSrc, id: string): WeiRow | undefined => d.weiRows.find((r) => r.id === id);

export function weiPoints(d: ClimateSrc, rowId: string, region: string): WeiPoint[] {
  const r = weiRow(d, rowId);
  return r ? (d.wei.get(r.idx)?.get(region) ?? []) : [];
}

export interface WeiSummary {
  row: WeiRow;
  /** the last year that is not an estimate, and the estimate year after it (null when none) */
  year: number | null;
  value: number | null;
  est: WeiPoint | null;
  first: WeiPoint | null;
  /** latest against the first year (2015) */
  change: number | null;
  /** the geographic regions in the latest year, largest first (China is inside Asia Pacific) */
  byRegion: { region: string; label: string; value: number; share: number | null }[];
}

export function weiSummary(d: ClimateSrc, rowId: string): WeiSummary | null {
  const row = weiRow(d, rowId);
  if (!row) return null;
  const w = weiPoints(d, rowId, 'World');
  const obs = w.filter((p) => !p.estimate);
  const last = obs[obs.length - 1] ?? null;
  const est = w.find((p) => p.estimate && last !== null && p.year > last.year) ?? null;
  const first = w[0] ?? null;
  const byRegion = d.weiRegions
    .filter((r) => r.kind === 'region')
    .map((r) => {
      const v = last ? weiPoints(d, rowId, r.region).find((p) => p.year === last.year)?.value : undefined;
      return { region: r.region, label: r.label, value: v ?? NaN, share: v !== undefined && last && last.value > 0 ? v / last.value : null };
    })
    .filter((r) => Number.isFinite(r.value))
    .sort((a, z) => z.value - a.value);
  return { row, year: last?.year ?? null, value: last?.value ?? null, est, first, change: last && first ? pctChange(last.value, first.value) : null, byRegion };
}

/** "$407 bn". */
export const fmtBn = (v: number | null | undefined): string => (v === null || v === undefined || !Number.isFinite(v) ? '–' : `$${fmtSig(v)} bn`);

/** The section's investment sentence for the cell's first WEI row (null without one). */
export function weiHeadline(d: ClimateSrc, cellIdx: number): { text: string; link: SrcLink; s: WeiSummary } | null {
  const link = srcLinks(d, cellIdx).find((l) => l.src === 'wei');
  if (!link) return null;
  const s = weiSummary(d, link.key);
  if (!s || s.value === null || s.year === null) return null;
  const what = `${lc(s.row.name)}${link.match === 'proxy' ? ' (a proxy)' : ''}`;
  const pre = link.role === 'demand' ? 'Investment in the deployment this sector supplies: ' : '';
  const top = s.byRegion[0];
  // A change above +200% reads as a multiple ("62 times its 2015 level"), not a four-digit percentage.
  const since = s.change === null || !s.first ? '' : s.change > 2 ? ` (${fmtSig(s.change + 1, 2)} times its ${s.first.year} level)` : ` (${fmtPct(s.change)} since ${s.first.year})`;
  let text = `${pre}${what} drew ${fmtBn(s.value)} of investment worldwide in ${s.year}${since}`;
  if (top && top.share !== null) text += `; ${top.label} ${(top.share * 100).toFixed(0)}% of it`;
  if (s.est) text += `. The IEA estimates ${fmtBn(s.est.value)} for ${s.est.year}`;
  text += '.';
  return { text: text.charAt(0).toUpperCase() + text.slice(1), link, s };
}

/** World clean-energy and fossil-fuel-supply investment in the latest year (the section's context line). */
export function weiContext(d: ClimateSrc): string | null {
  const c = weiSummary(d, 'clean');
  const f = weiSummary(d, 'fossil_fuels');
  const t = weiSummary(d, 'total');
  if (!c || !f || c.value === null || f.value === null || c.year === null) return null;
  return `World energy investment in ${c.year}: ${fmtBn(c.value)} in clean energy${t?.value ? ` of ${fmtBn(t.value)} in all` : ''}, against ${fmtBn(f.value)} in fossil fuel supply (${fmtSig(c.value / f.value, 2)} to 1).`;
}

// ---- Sector: World fossil CO2 by fuel (GCP) ---------------------------------------------------------------

export const FUEL_WORDS: Record<SrcFuel, string> = { total: 'All fossil CO2', coal: 'Coal', oil: 'Oil', gas: 'Gas', cement: 'Cement', flaring: 'Flaring', other: 'Other' };

export interface FuelRow {
  fuel: SrcFuel;
  points: [number, number][];
  last: { year: number; value: number } | null;
  share: number | null;
  since2015: number | null;
}

/** The cell's GCP fuels (context): World CO2 by fuel, its share of all fossil CO2 and the change since 2015. */
export function fuelRows(d: ClimateSrc, cellIdx: number): FuelRow[] {
  const fuels = srcLinks(d, cellIdx).filter((l) => l.src === 'gcp').map((l) => l.key as SrcFuel);
  const total = d.gcpFuel.get('total');
  return fuels.map((f) => {
    const points = series(d.gcpFuel.get(f));
    const l = points[points.length - 1];
    const t = l ? total?.get(l[0]) : undefined;
    return { fuel: f, points, last: l ? { year: l[0], value: l[1] } : null, share: l && t ? l[1] / t : null, since2015: l ? pctChange(l[1], at(d.gcpFuel.get(f), 2015)) : null };
  });
}
