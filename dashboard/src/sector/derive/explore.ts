// The theme explorer (spec_sector.md 4.6). Pure: which measures a theme offers, the measure / item /
// year chosen from the URL (invalid values fall back to the defaults), the layout a measure takes by
// its native level, whether "share of world" is legal, the economies scope, the KPI strip, the result
// line, the table rows (with the roster's absentees as "not covered") and the across-sectors rows.
// Nothing here is a weight or a score.

import { noteLine } from './notes.ts';
import { stageSlugOf, STAGE_SLUGS, techSlugOf, type StageSlug } from '../contract.ts';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, SummaryInfo, VarInfo } from '../data/model.ts';
import type { GeoScope } from '../url.ts';
import { concentration, type Concentration } from './concentration.ts';
import { isFdiVar } from './fdi.ts';
import { isCoalition, partOf } from './groups.ts';
import { stateOf, type StateOf } from './states.ts';

/** How a measure is laid out (4.6 "Layout substitutions by native level", plus the special cases). */
export type ExploreLayout =
  /** KPI strip, map + ranking, distribution, across sectors, over time, table, notes */
  | 'country'
  /** regional bars (IEA EV regions) */
  | 'region'
  /** entity share bars, now and 2035 (IEA manufacturing) */
  | 'entity'
  /** the price / world board */
  | 'world'
  /** the 11 x 3 cell grid (TRL) */
  | 'cell_global'
  /** flows panel (trade.flow) */
  | 'dyad'
  /** policy records and CAT ratings (the policy chunk) */
  | 'policy'
  /** area-integrated potential: scatter against quality, never ranked */
  | 'scatter'
  /** IEA mineral demand by technology (dims.mineral_demand) */
  | 'mineral_demand'
  /** CIM manufacturing capacity by product (series.capacity; native units, never summed) */
  | 'capacity'
  /** CM-FDI cross-border projects (sector-fdi): destinations, flows, parent groups, years, projects */
  | 'fdi';

/** Area-integrated potentials and the quality measure they are plotted against (3.2: scatter only). */
export const SCATTER_PAIRS: Readonly<Record<string, string>> = {
  'resources.pv_area': 'resources.pv_quality',
  'resources.wind_area': 'resources.wind_density',
};

export function layoutOf(v: VarInfo): ExploreLayout {
  if (v.id in SCATTER_PAIRS) return 'scatter';
  if (v.id === 'consumption.mineral_demand') return 'mineral_demand';
  if (v.id === 'manufacturing.capacity') return 'capacity';
  if (isFdiVar(v.id)) return 'fdi';
  if (v.theme === 'policy' || v.id.startsWith('policy.')) return 'policy';
  switch (v.native_level) {
    case 'region':
      return 'region';
    case 'entity':
      return 'entity';
    case 'world':
      return 'world';
    case 'cell_global':
      return 'cell_global';
    case 'dyad':
      return 'dyad';
    default:
      return 'country';
  }
}

/** The banner of a layout substitution (4.6 table), or null. */
export function layoutBanner(v: VarInfo): string | null {
  switch (layoutOf(v)) {
    case 'region':
      return 'Reported for regions; countries in a region share its value, so countries are not ranked.';
    case 'entity':
      return 'IEA reports these shares for five economies and the EU.';
    case 'world':
      return v.theme === 'prices' ? 'World prices: the same for every country.' : 'A world value: the same for every country.';
    case 'cell_global':
      return 'A property of the technology, the same in every country.';
    case 'dyad':
      return 'Bilateral flows of the technology-attributed HS6 basket: an HS6 code can count in more than one sector.';
    case 'scatter':
      return 'Area-integrated potential is the mean quality times land area, so it ranks mostly by size: shown against quality, never ranked.';
    case 'fdi':
      return 'Project-level data: cross-border manufacturing projects by the parent company’s headquarters and the destination, counted as published (capital where disclosed). Not a country total: domestic investment is not covered.';
    default:
      return null;
  }
}

/**
 * Trade by sub-component (spec_country C5.4) is a view of exports / imports ("View by: Sub-component"),
 * not a measure of its own: these vars are never offered in the measure select, and a link naming one
 * opens its whole-basket measure.
 */
export const SUB_VAR_BASE: Readonly<Record<string, string>> = { 'trade.sub_exports': 'trade.exports', 'trade.sub_imports': 'trade.imports' };

/** Vars of a theme that ship data in this bundle (dictionary order), without the sub-component views. */
export function themeVars(model: SectorModel, theme: string): VarInfo[] {
  return model.varsOfTheme(theme).filter((v) => !Object.hasOwn(SUB_VAR_BASE, v.id) && (model.availByVar.get(v.idx) ?? []).length > 0);
}

/** The measure: `m` when it belongs to the theme and has data (a sub-component var: its whole-basket var), else the theme default, else the first. */
export function pickVar(model: SectorModel, theme: string, m: string | null): VarInfo | null {
  const vars = themeVars(model, theme);
  const byId = (id: string | null | undefined) => (id ? (vars.find((v) => v.id === id) ?? null) : null);
  return byId(m) ?? byId(m && Object.hasOwn(SUB_VAR_BASE, m) ? SUB_VAR_BASE[m] : null) ?? byId(model.themes.find((t) => t.slug === theme)?.default_var) ?? vars[0] ?? null;
}

/** Items with data for a var, in item order. */
export function itemsOf(model: SectorModel, v: VarInfo): ItemInfo[] {
  return model.itemsWithData(v);
}

/** The var's default item: the first primary_for cell with data, else the largest world total (meta.summary), else the first. */
export function defaultItem(model: SectorModel, v: VarInfo): ItemInfo | null {
  const items = itemsOf(model, v);
  if (!items.length) return null;
  for (const slug of v.primary_for) {
    const it = items.find((x) => x.slug === slug);
    if (it) return it;
  }
  let best: { it: ItemInfo; W: number } | null = null;
  for (const it of items) {
    const s = model.summaryLatest(it.idx, v.idx, 'members');
    if (s && Number.isFinite(s.W) && (!best || s.W > best.W)) best = { it, W: s.W };
  }
  return best?.it ?? items[0];
}

/** The item from the URL (cell: tech + stage in the Strategic vocabulary; others: item slug), when valid, else the default. */
export function pickItem(model: SectorModel, v: VarInfo, q: { tech: string | null; stage: string | null; item: string | null }): ItemInfo | null {
  const items = itemsOf(model, v);
  if (v.item_axis === 'cell') {
    const t = q.tech ? techSlugOf(q.tech) : null;
    const s = q.stage ? stageSlugOf(q.stage) : null;
    if (t && s) {
      const it = items.find((x) => x.techSlug === t && x.stageSlug === s);
      if (it) return it;
    }
    if (t && !s) {
      const it = items.find((x) => x.techSlug === t);
      if (it) return it;
    }
  } else if (q.item) {
    const it = items.find((x) => x.slug === q.item);
    if (it) return it;
  }
  return defaultItem(model, v);
}

/** The year: `y` when valid, else the latest observed year (projection years only when chosen). */
export function pickYear(v: VarInfo, years: readonly number[], y: number | null): number | null {
  if (!years.length) return null;
  if (y !== null && years.includes(y)) return y;
  const obs = years.filter((x) => v.projection_from === null || x < v.projection_from);
  return (obs.length ? obs : years)[(obs.length ? obs : years).length - 1];
}

/** Is "share of world" legal for a measure (a world basis on an additive measure)? */
export function shareLegal(v: VarInfo): boolean {
  return v.world_basis !== 'none' && (v.agg === 'sum' || v.world_basis === 'one');
}

/** Is a measure signed (balances, growth), so maps diverge with poles in words? */
export function isDiverging(v: VarInfo): boolean {
  return v.id === 'trade.net' || v.id === 'consumption.balance' || v.unit.unit_kind === 'pct_change' || /growth|change|yoy/.test(v.id);
}

/** Pole words of a signed measure. */
export function poles(v: VarInfo): { negLabel: string; posLabel: string } {
  if (v.id === 'trade.net') return { negLabel: 'net importer ←', posLabel: '→ net exporter' };
  if (v.id === 'consumption.balance') return { negLabel: 'net importer ←', posLabel: '→ net exporter' };
  return { negLabel: 'falling ←', posLabel: '→ rising' };
}

/** The economies scope of the ranking, distribution and table (`geo`). */
export function inScope(g: GeoInfo, scope: GeoScope): boolean {
  if (scope === 'all') return true;
  const coal = isCoalition(g) || g.kind === 'eu';
  return scope === 'coalition' ? coal : coal || g.code === 'CHN';
}

export const SCOPE_LABEL: Record<GeoScope, string> = { all: 'All', coalition: 'Coalition', focus: 'Coalition + China' };

const median = (xs: number[]): number => {
  if (!xs.length) return NaN;
  const s = [...xs].sort((a, z) => a - z);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};

export interface Kpis {
  conc: Concentration;
  /** coalition members (core + partners; the EU row in bloc mode) with a value, and the roster of members */
  coalitionWith: number;
  coalitionTotal: number;
  china: { share: number | null; rank: number | null; value: number | null };
  top3: { geo: number; share: number | null }[];
  highlight: { value: number | null; share: number | null; rank: number | null; state: StateOf } | null;
  /** agg none: the medians */
  median: number;
  coalitionMedian: number;
}

/** The KPI strip of one slice (4.6 section 1): world total, coalition, China, top 3, HHI, highlight, medians. */
export function kpis(model: SectorModel, v: VarInfo, rows: readonly FactRow[], world: { value: number } | null, highlight: GeoInfo | null, year?: number): Kpis {
  const conc = concentration(rows, model.geos, world, v.world_basis);
  const members = model.geos.filter((g) => (g.kind === 'country' && isCoalition(g)) || (g.kind === 'eu' && rows.some((r) => r.geo === g.idx)));
  const have = new Set(rows.filter((r) => Number.isFinite(r.value)).map((r) => r.geo));
  const coalitionWith = members.filter((g) => have.has(g.idx)).length;
  const chn = model.geoByCode.get('CHN');
  const cRow = chn ? conc.rows.find((r) => r.geo === chn.idx) : undefined;
  const vals = rows.filter((r) => model.geos[r.geo]?.kind === 'country' && Number.isFinite(r.value));
  let hl: Kpis['highlight'] = null;
  if (highlight) {
    const f = rows.find((r) => r.geo === highlight.idx) ?? null;
    const p = conc.rows.find((r) => r.geo === highlight.idx);
    hl = { value: f && Number.isFinite(f.value) ? f.value : null, share: p?.share ?? null, rank: p?.rank ?? null, state: stateOf(f, highlight, v, model.rosterOf(v), year) };
  }
  return {
    conc,
    coalitionWith,
    coalitionTotal: members.length,
    china: { share: cRow?.share ?? null, rank: cRow?.rank ?? null, value: cRow?.value ?? null },
    top3: conc.rows.slice(0, 3).map((r) => ({ geo: r.geo, share: r.share })),
    highlight: hl,
    median: median(vals.map((r) => r.value)),
    coalitionMedian: median(vals.filter((r) => isCoalition(model.geos[r.geo])).map((r) => r.value)),
  };
}

export interface ExploreRow {
  g: GeoInfo;
  value: number | null;
  status: string;
  state: StateOf;
  share: number | null;
  rank: number | null;
  /** the observation year (per-row years) or the slice year */
  year: number | null;
  /** the fact's notes in words (facts.notes; the assumed-zero note), "" when none */
  note: string;
}

/**
 * Table rows of a slice: every economy row of the scope (countries, and the EU row in bloc mode),
 * ranked within the scope; with `absent`, the economies of the roster without a row (or outside it)
 * as "not covered" / "none recorded".
 */
export function exploreRows(model: SectorModel, v: VarInfo, rows: readonly FactRow[], world: { value: number } | null, year: number | null, scope: GeoScope, absent: boolean): ExploreRow[] {
  const scoped = rows.filter((r) => {
    const g = model.geos[r.geo];
    return !!g && (g.kind === 'country' || g.kind === 'eu') && inScope(g, scope);
  });
  const conc = concentration(scoped, model.geos, world, v.world_basis);
  const pos = new Map(conc.rows.map((r) => [r.geo, r]));
  const roster = model.rosterOf(v);
  const out: ExploreRow[] = scoped.map((r) => {
    const g = model.geos[r.geo];
    const p = pos.get(r.geo);
    return { g, value: Number.isFinite(r.value) ? r.value : null, status: r.status, state: stateOf(r, g, v, roster, year ?? undefined), share: p?.share ?? null, rank: p?.rank ?? null, year: r.obs_year ?? year, note: noteLine(model, v.id, r) };
  });
  if (absent) {
    const seen = new Set(scoped.map((r) => r.geo));
    for (const g of model.countries) {
      if (seen.has(g.idx) || !inScope(g, scope)) continue;
      out.push({ g, value: null, status: '', state: stateOf(null, g, v, roster, year ?? undefined), share: null, rank: null, year, note: '' });
    }
  }
  return out.sort((a, z) => (a.rank ?? Infinity) - (z.rank ?? Infinity) || a.g.name.localeCompare(z.g.name, 'en'));
}

/** "142 economies with data · 3 report zero · 75 not covered". */
export function resultLine(model: SectorModel, v: VarInfo, rows: readonly FactRow[], scope: GeoScope): string {
  const roster = model.rosterOf(v);
  const geos = model.countries.filter((g) => inScope(g, scope));
  const byGeo = new Map(rows.map((r) => [r.geo, r]));
  let withData = 0;
  let zero = 0;
  let noFigure = 0;
  let notCovered = 0;
  let none = 0;
  for (const g of geos) {
    const r = byGeo.get(g.idx);
    if (!r) {
      if (roster && roster.has(g.idx)) none++;
      else notCovered++;
      continue;
    }
    if (v.agg === 'sum' && (r.status === 'zero' || r.value === 0)) zero++;
    else if (Number.isFinite(r.value)) withData++;
    else noFigure++;
  }
  const parts = [`${withData + zero} economies with data`];
  if (zero) parts.push(`${zero} report zero`);
  if (noFigure) parts.push(`${noFigure} listed without a figure`);
  if (none) parts.push(`${none} with none recorded`);
  if (notCovered) parts.push(`${notCovered} not covered`);
  return parts.join(' · ');
}

export interface AcrossRow {
  item: ItemInfo;
  s: SummaryInfo;
  /** the stage section of a cell item ('minerals' / 'fuels' for other axes) */
  section: string;
}

/** Across sectors (4.6 section 4): one summary per item of the measure's axis, at `year` when it exists, else the latest observed. */
export function acrossRows(model: SectorModel, v: VarInfo, eu: 'members' | 'bloc', year: number | null): AcrossRow[] {
  if (!shareLegal(v) || v.agg !== 'sum') return [];
  const out: AcrossRow[] = [];
  for (const it of itemsOf(model, v)) {
    const s = (year !== null ? model.summaryOf(it.idx, v.idx, year, eu) : null) ?? model.summaryLatest(it.idx, v.idx, eu);
    if (!s || !Number.isFinite(s.W)) continue;
    out.push({ item: it, s, section: it.axis === 'cell' ? (it.stageSlug ?? 'other') : `${it.axis}s` });
  }
  const order = [...STAGE_SLUGS, 'minerals', 'fuels', 'commoditys', 'activitys'] as string[];
  return out.sort((a, z) => order.indexOf(a.section) - order.indexOf(z.section) || a.item.sort - z.item.sort);
}

/** A group's mark for a geo (an economy is never "not attributed"). */
export const geoMark = (g: GeoInfo): 'china' | 'core' | 'partner' | 'rest' => {
  const p = partOf(g);
  return p === 'unattributed' ? 'rest' : p;
};

/** The cells of the TRL grid: TECH x STAGE, value and whether the technology-wide fallback is used (named in the caveat). */
export function fallbackCells(model: SectorModel, v: VarInfo): Set<string> {
  const text = model.caveatTexts(v).join(' ');
  const out = new Set<string>();
  for (const c of model.cells) if (c.tech && c.stage && text.includes(`${c.tech} ${c.stage}`)) out.add(c.slug);
  return out;
}

/** Stage of a cell slug's section, for headings. */
export const stageOrder = (s: string): number => (STAGE_SLUGS as readonly string[]).indexOf(s as StageSlug);
