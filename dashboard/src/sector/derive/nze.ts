// IEA scenario pathways (World) mapped to technology x stage: the optional climate.nze_* tables of
// sector-climate (user request 2026-10-17: IEA Net Zero data for the Climate section). Pure: indexes the
// tables and derives the rows the Sector Profile's Climate lens and section and the Country Profile's
// Policy & climate section show. Values are shown as shipped (World, the WEO unit); the only computed
// figures are ratios and changes between shipped values, and sums of one mineral's demand over the
// technologies a cell names (same unit, same scenario, same year). Every value is an IEA scenario
// projection or the IEA's historical estimate, never observed data for an economy.

import { NZE_MINERAL_SCENARIOS, NZE_SCENARIOS, type ClimateMatch, type NzeMineralScenario, type NzeScenario, type SectorClimateChunk } from '../contract.ts';
import { fmtSig, lc, listText } from '../format.ts';

export interface NzeSeries {
  idx: number;
  id: string;
  grp: 'deployment' | 'power' | 'fuel' | 'emissions';
  label: string;
  unit: string;
  category: string;
  product: string;
  flow: string;
}

export interface NzeLink {
  series: NzeSeries;
  role: 'own' | 'demand' | 'context';
  /** null for context */
  match: Exclude<ClimateMatch, 'none'> | null;
}

export interface NzeMineralRow {
  sheet: '1' | '3.1' | '3.2';
  mineral: string;
  tech: string;
  scenario: NzeMineralScenario;
  year: number;
  kt: number;
  slug: string | null;
}

export interface NzeData {
  series: NzeSeries[];
  byId: Map<string, NzeSeries>;
  /** series idx -> scenario -> year -> value */
  values: Map<number, Map<NzeScenario, Map<number, number>>>;
  /** cell item idx -> its series in map order */
  links: Map<number, NzeLink[]>;
  cells: Map<number, { match: ClimateMatch; note: string; mineralTechs: string[] }>;
  minerals: NzeMineralRow[];
  meta: Map<string, string | null>;
  /** the last historical year (2024 in WEO 2025) */
  baseYear: number;
  /** the projection years, ascending */
  projYears: number[];
  /** "World Energy Outlook 2025" */
  weo: string;
  /** the mineral dataset's base year (shared by its scenarios) */
  mineralBase: number;
}

/** The series the Country Profile's world context line reads (config/iea_pathways_map.yml world_context). */
export const NZE_WORLD_CONTEXT = 'co2_energy';
/** The comparison year of the headlines (the first projection year the WEO 2025 free dataset publishes). */
export const NZE_YEAR = 2035;

export const SCENARIO_WORDS: Record<NzeScenario, string> = {
  historical: 'Historical (IEA estimate)',
  stated: 'Stated Policies',
  current: 'Current Policies',
  nze: 'Net Zero Emissions by 2050',
};
export const MINERAL_SCENARIO_WORDS: Record<NzeMineralScenario, string> = {
  current: 'Current Policies',
  stated: 'Stated Policies',
  high_demand: 'High Demand',
};

/** Plain words for a match quality of the IEA pathways. */
export const NZE_MATCH_WORDS: Record<ClimateMatch, { label: string; text: string }> = {
  direct: { label: 'Direct match', text: 'The WEO series measures this sector’s own activity.' },
  partial: { label: 'Partial match', text: 'The WEO series covers this sector’s activity but also other activity, only part of it, or the stock its output adds to.' },
  proxy: { label: 'Proxy', text: 'A related quantity stands in for this sector: shown for direction and scale, not as its own figure.' },
  none: { label: 'No match', text: 'The IEA’s World Energy Outlook free dataset has no series for this sector.' },
};

/** Index the climate.nze_* tables of a decoded sector-climate chunk (null when the build has none). */
export function nzeData(c: SectorClimateChunk): NzeData | null {
  const st = c['climate.nze_series'];
  const vt = c['climate.nze_values'];
  const mt = c['climate.nze_map'];
  const ct = c['climate.nze_cells'];
  const nt = c['climate.nze_minerals'];
  const kt = c['climate.nze_meta'];
  if (!st || !vt || !mt || !ct || !nt || !kt) return null;
  const series: NzeSeries[] = [];
  for (let i = 0; i < st.nrow; i++) {
    series.push({
      idx: i, id: st.cols.id[i] ?? '', grp: (st.cols.grp[i] ?? 'deployment') as NzeSeries['grp'], label: st.cols.label[i] ?? '', unit: st.cols.unit[i] ?? '',
      category: st.cols.category[i] ?? '', product: st.cols.product[i] ?? '', flow: st.cols.flow[i] ?? '',
    });
  }
  const values = new Map<number, Map<NzeScenario, Map<number, number>>>();
  let baseYear = -Infinity;
  const proj = new Set<number>();
  for (let i = 0; i < vt.nrow; i++) {
    const s = vt.cols.series[i];
    const sc = vt.cols.scenario[i] as NzeScenario;
    const y = vt.cols.year[i];
    let bySc = values.get(s);
    if (!bySc) values.set(s, (bySc = new Map()));
    let byY = bySc.get(sc);
    if (!byY) bySc.set(sc, (byY = new Map()));
    byY.set(y, vt.cols.value[i]);
    if (sc === 'historical') baseYear = Math.max(baseYear, y);
    else proj.add(y);
  }
  const links = new Map<number, NzeLink[]>();
  for (let i = 0; i < mt.nrow; i++) {
    const item = mt.cols.item[i];
    const l = links.get(item) ?? [];
    l.push({ series: series[mt.cols.series[i]], role: (mt.cols.role[i] ?? 'context') as NzeLink['role'], match: (mt.cols.match[i] as NzeLink['match']) ?? null });
    links.set(item, l);
  }
  const cells = new Map<number, { match: ClimateMatch; note: string; mineralTechs: string[] }>();
  for (let i = 0; i < ct.nrow; i++) {
    cells.set(ct.cols.item[i], { match: (ct.cols.match[i] ?? 'none') as ClimateMatch, note: ct.cols.note[i] ?? '', mineralTechs: (ct.cols.mineral_techs[i] ?? '').split('|').filter(Boolean) });
  }
  const minerals: NzeMineralRow[] = [];
  for (let i = 0; i < nt.nrow; i++) {
    minerals.push({
      sheet: (nt.cols.sheet[i] ?? '3.1') as NzeMineralRow['sheet'], mineral: nt.cols.mineral[i] ?? '', tech: nt.cols.tech[i] ?? '',
      scenario: (nt.cols.scenario[i] ?? 'stated') as NzeMineralScenario, year: nt.cols.year[i], kt: nt.cols.kt[i], slug: nt.cols.slug[i],
    });
  }
  const meta = new Map<string, string | null>();
  for (let i = 0; i < kt.nrow; i++) meta.set(kt.cols.key[i] ?? '', kt.cols.value[i]);
  const mb = Number(meta.get('minerals_base_year'));
  return {
    series, byId: new Map(series.map((s) => [s.id, s])), values, links, cells, minerals, meta,
    baseYear: Number.isFinite(baseYear) ? baseYear : NaN, projYears: [...proj].sort((a, z) => a - z), weo: meta.get('weo') ?? 'World Energy Outlook',
    mineralBase: Number.isFinite(mb) ? mb : NaN,
  };
}

export function nzeValue(d: NzeData, series: number, scenario: NzeScenario, year: number): number | null {
  const v = d.values.get(series)?.get(scenario)?.get(year);
  return v === undefined || !Number.isFinite(v) ? null : v;
}

/** The cell's match quality (none when the tables have no row for it). */
export function nzeCell(d: NzeData, cellIdx: number): { match: ClimateMatch; note: string; mineralTechs: string[] } {
  return d.cells.get(cellIdx) ?? { match: 'none', note: '', mineralTechs: [] };
}

export const nzeLinks = (d: NzeData, cellIdx: number): NzeLink[] => d.links.get(cellIdx) ?? [];
/** The cell's own and demand series, in map order (own first). */
export const nzePrimary = (d: NzeData, cellIdx: number): NzeLink[] => {
  const l = nzeLinks(d, cellIdx).filter((x) => x.role !== 'context');
  return [...l.filter((x) => x.role === 'own'), ...l.filter((x) => x.role === 'demand')];
};

export interface PathPoint {
  x: number;
  y: number;
  projection: boolean;
}

/** One series as lines: Historical, then each scenario from the last historical year on. */
export function pathwayLines(d: NzeData, s: NzeSeries): { scenario: NzeScenario; points: PathPoint[] }[] {
  const by = d.values.get(s.idx);
  if (!by) return [];
  const hist = [...(by.get('historical') ?? new Map<number, number>())].sort((a, z) => a[0] - z[0]).map(([x, y]) => ({ x, y, projection: false }));
  const last = hist[hist.length - 1];
  const out: { scenario: NzeScenario; points: PathPoint[] }[] = [];
  if (hist.length) out.push({ scenario: 'historical', points: hist });
  for (const sc of NZE_SCENARIOS) {
    if (sc === 'historical') continue;
    const pts = [...(by.get(sc) ?? new Map<number, number>())].sort((a, z) => a[0] - z[0]).map(([x, y]) => ({ x, y, projection: true }));
    if (!pts.length) continue;
    out.push({ scenario: sc, points: last ? [last, ...pts] : pts });
  }
  return out;
}

export interface NzeCompare {
  base: number | null;
  stated: number | null;
  current: number | null;
  nze: number | null;
  /** nze / stated (null when stated is not positive) */
  ratio: number | null;
  /** nze / base - 1 */
  change: number | null;
}

/** The scenarios of a series in one year, against the last historical year. */
export function nzeCompare(d: NzeData, s: NzeSeries, year = NZE_YEAR): NzeCompare {
  const base = nzeValue(d, s.idx, 'historical', d.baseYear);
  const stated = nzeValue(d, s.idx, 'stated', year);
  const current = nzeValue(d, s.idx, 'current', year);
  const nze = nzeValue(d, s.idx, 'nze', year);
  return {
    base, stated, current, nze,
    ratio: nze !== null && stated !== null && stated > 0 ? nze / stated : null,
    change: nze !== null && base !== null && base > 0 ? nze / base - 1 : null,
  };
}

/** A value with its WEO unit: "6,830 GW", "45.2 EJ", "312 gCO2 per kWh". */
export function fmtNze(v: number | null | undefined, unit: string): string {
  if (v === null || v === undefined || !Number.isFinite(v)) return '–';
  return `${fmtSig(v)} ${unit}`;
}

/** "2.1 times", "1.04 times". */
export const fmtTimes = (r: number | null): string => (r === null ? '–' : `${fmtSig(r, r >= 10 ? 3 : 2)} times`);

/** A signed percentage of a change: "−55%", "+120%". */
export function fmtPctChange(c: number | null): string {
  if (c === null || !Number.isFinite(c)) return '–';
  const p = Math.round(c * 100);
  return `${p > 0 ? '+' : p < 0 ? '−' : ''}${Math.abs(p)}%`;
}

/** The lens line of a cell (null when the cell has no own or demand series). */
export function nzeHeadline(d: NzeData, cellIdx: number, year = NZE_YEAR): { text: string; link: NzeLink; cmp: NzeCompare } | null {
  const link = nzePrimary(d, cellIdx)[0];
  if (!link) return null;
  const s = link.series;
  const cmp = nzeCompare(d, s, year);
  if (cmp.nze === null || cmp.stated === null) return null;
  const what = `${lc(s.label)}${link.match === 'proxy' ? ' (a proxy)' : ''}`;
  const pre = link.role === 'demand' ? 'Demand this sector serves: ' : '';
  const base = cmp.base !== null ? `${fmtNze(cmp.base, s.unit)} in ${d.baseYear} (IEA estimate)` : null;
  let text: string;
  if (cmp.nze >= cmp.stated) {
    text = `${pre}the Net Zero pathway needs ${fmtNze(cmp.nze, s.unit)} of ${what} by ${year}, ${fmtTimes(cmp.ratio)} Stated Policies (${fmtNze(cmp.stated, s.unit)})${base ? `; ${base}` : ''}.`;
  } else {
    text = `${pre}on the Net Zero pathway, ${what} falls to ${fmtNze(cmp.nze, s.unit)} by ${year}${cmp.change !== null ? ` (${fmtPctChange(cmp.change)} from ${d.baseYear})` : ''}, against ${fmtNze(cmp.stated, s.unit)} under Stated Policies.`;
  }
  return { text: text.charAt(0).toUpperCase() + text.slice(1), link, cmp };
}

export interface MineralPath {
  mineral: string;
  slug: string | null;
  /** scenario -> year -> kt (summed over the cell's technologies) */
  by: Map<NzeMineralScenario, Map<number, number>>;
  /** the technologies of the cell the mineral is reported for */
  techs: string[];
  /** kt in the base year */
  base: number | null;
}

/**
 * A cell's mineral demand by scenario: sheet 3.1 rows of the cell's technologies, one mineral's kt summed
 * over them per scenario and year. Largest Stated Policies demand in `year` first.
 */
export function mineralPaths(d: NzeData, techs: string[], year = NZE_YEAR): MineralPath[] {
  if (!techs.length) return [];
  const want = new Set(techs);
  const by = new Map<string, MineralPath>();
  for (const r of d.minerals) {
    if (r.sheet !== '3.1' || !want.has(r.tech) || !Number.isFinite(r.kt)) continue;
    let m = by.get(r.mineral);
    if (!m) by.set(r.mineral, (m = { mineral: r.mineral, slug: r.slug, by: new Map(), techs: [], base: null }));
    if (!m.techs.includes(r.tech)) m.techs.push(r.tech);
    let s = m.by.get(r.scenario);
    if (!s) m.by.set(r.scenario, (s = new Map()));
    s.set(r.year, (s.get(r.year) ?? 0) + r.kt);
  }
  const out = [...by.values()];
  for (const m of out) {
    const b = m.by.get('stated')?.get(d.mineralBase);
    m.base = b === undefined ? null : b;
  }
  const key = (m: MineralPath) => m.by.get('stated')?.get(year) ?? -Infinity;
  return out.filter((m) => [...m.by.values()].some((s) => [...s.values()].some((v) => v > 0))).sort((a, z) => key(z) - key(a) || a.mineral.localeCompare(z.mineral, 'en'));
}

export const mineralAt = (m: MineralPath, scenario: NzeMineralScenario, year: number): number | null => {
  const v = m.by.get(scenario)?.get(year);
  return v === undefined ? null : v;
};

/** The mineral scenarios in display order. */
export const MINERAL_SCENARIOS: readonly NzeMineralScenario[] = NZE_MINERAL_SCENARIOS;

/** The technologies in words: "electric vehicles and grid battery storage". */
export const techWords = (techs: string[]): string => listText(techs.map((t) => lc(t)));

/** The Country Profile's world context line (null without the series). */
export function nzeWorldLine(d: NzeData, year = NZE_YEAR): string | null {
  const s = d.byId.get(NZE_WORLD_CONTEXT);
  if (!s) return null;
  const c = nzeCompare(d, s, year);
  const n50 = nzeValue(d, s.idx, 'nze', 2050);
  if (c.base === null || c.stated === null || c.nze === null) return null;
  const gt = (v: number | null) => (v === null ? '–' : s.unit === 'Mt CO2' ? `${fmtSig(v / 1000)} Gt` : fmtNze(v, s.unit));
  return `World context, not this economy’s: energy-sector CO2 was ${gt(c.base)} in ${d.baseYear} (IEA estimate). The IEA’s scenarios project ${gt(c.stated)} in ${year} under Stated Policies, ${gt(c.current)} under Current Policies and ${gt(c.nze)} on the Net Zero pathway${n50 === null ? '' : n50 === 0 ? ', reaching net zero in 2050' : ` (${gt(n50)} in 2050)`}.`;
}
