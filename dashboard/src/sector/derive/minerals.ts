// Minerals (spec_sector.md 4.5): the index rows (USGS mine production, IEA mining and refining top-3,
// China and coalition shares, the technologies that use each mineral), the IEA demand by technology,
// the share lines of the projected years (slope chart), the share-vs-share scatter of USGS production
// against USGS reserves and the IMF commodity of a mineral. Pure. Shares are always within one
// measure, one source and one year; projected years carry `projection`.

import { techSlugOf, TECH_LABELS, type SplitPart, type StageSlug, type TechSlug } from '../contract.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { GeoInfo, ItemInfo, MineralDemandRow, SectorModel, SummaryInfo, VarInfo } from '../data/model.ts';
import { concentration, isHolder } from './concentration.ts';
import { partOf } from './groups.ts';
import { slice } from './summary.ts';

export const MINE_VAR = 'production.mine_usgs';
export const MINING_VAR = 'production.mining';
export const REFINING_VAR = 'production.refining';
export const RESERVES_VAR = 'resources.mineral_reserves';
export const CRITMIN_EXPORTS_VAR = 'trade.critmin_exports';
export const CRITMIN_DEP_VAR = 'trade.critmin_dependence';
export const DEMAND_VAR = 'consumption.mineral_demand';

/**
 * The mineral step a stage shows (design C3, 2026-10-28): the mining of the minerals a technology uses at
 * its Extraction cell, their refining at its Refining cell (the item links reach both; the measures'
 * link_stage picks one).
 */
export const MINERAL_STEP: Partial<Record<StageSlug, { id: typeof MINING_VAR | typeof REFINING_VAR; words: 'mining' | 'refining' }>> = {
  extraction: { id: MINING_VAR, words: 'mining' },
  refining: { id: REFINING_VAR, words: 'refining' },
};

/**
 * The stages a mineral's trade is measured at (design C3): ores and concentrates at Extraction, refined
 * products at Refining, scrap at Recovery (R SECTOR_CRITMIN_STAGES), with the words a heading uses.
 */
export const CRITMIN_STAGE_WORDS: Partial<Record<StageSlug, string>> = {
  extraction: 'ores and concentrates',
  refining: 'refined products',
  recovery: 'scrap and waste',
};

/** A mineral's items on the mineral_stage axis ("lithium-refining"), in chain order (dims.items sort). */
export function mineralStageItems(model: SectorModel, mineral: ItemInfo): ItemInfo[] {
  return model.items.filter((x) => x.axis === 'mineral_stage' && x.slug.startsWith(`${mineral.slug}-`) && x.stageSlug !== null && x.slug === `${mineral.slug}-${x.stageSlug}`).sort((a, z) => a.sort - z.sort);
}

/** Mineral items in dictionary order. */
export function mineralItems(model: SectorModel): ItemInfo[] {
  return model.items.filter((x) => x.axis === 'mineral').sort((a, z) => a.sort - z.sort);
}

/** Is `year` a projection of `v` (dictionary projection_from)? */
export const isProjection = (v: VarInfo, year: number): boolean => v.projection_from !== null && year >= v.projection_from;

/** The latest observed (non-projected) meta.summary row of (var, item) in an EU mode. */
export function observedSummary(model: SectorModel, v: VarInfo | null, item: ItemInfo, eu: 'members' | 'bloc'): SummaryInfo | null {
  if (!v) return null;
  let best: SummaryInfo | null = null;
  for (const s of model.summary) {
    if (s.item !== item.idx || s.var !== v.idx || s.eu_mode !== eu || isProjection(v, s.year)) continue;
    if (!best || s.year > best.year) best = s;
  }
  return best;
}

/** The summary years of (var, item) in an EU mode, ascending (projections included). */
export function summaryYears(model: SectorModel, v: VarInfo | null, item: ItemInfo, eu: 'members' | 'bloc'): number[] {
  if (!v) return [];
  return [...new Set(model.summary.filter((s) => s.item === item.idx && s.var === v.idx && s.eu_mode === eu).map((s) => s.year))].sort((a, z) => a - z);
}

export interface TechUse {
  slug: TechSlug;
  label: string;
  /** the technology's share of observed clean-energy demand for the mineral (IEA), or the link weight */
  share: number | null;
}

/** The observed (non-scenario) demand rows of a mineral, latest year. */
function observedDemand(rows: readonly MineralDemandRow[]): MineralDemandRow[] {
  const obs = rows.filter((d) => !/steps|aps|nze/i.test(d.scenario));
  if (!obs.length) return [];
  const y = Math.max(...obs.map((d) => d.year));
  return obs.filter((d) => d.year === y);
}

/**
 * The technologies that use a mineral: the item links (IEA demand shares >= 1%, weight = the share),
 * merged with dims.mineral_demand (tech = the Strategic label). Largest share first.
 */
export function techsOfMineral(model: SectorModel, mineral: ItemInfo): TechUse[] {
  const out = new Map<TechSlug, TechUse>();
  for (const { cell, weight } of model.cellsOfItem(mineral.idx)) {
    const t = cell.techSlug;
    if (!t) continue;
    const prev = out.get(t);
    const w = Number.isFinite(weight) ? weight : null;
    if (!prev || (w !== null && (prev.share === null || w > prev.share))) out.set(t, { slug: t, label: TECH_LABELS[t], share: w });
  }
  const obs = observedDemand(model.mineralDemand.filter((d) => d.mineral === mineral.idx));
  const total = obs.find((d) => d.tech === null && /^total/i.test(d.label));
  for (const d of obs) {
    if (!d.tech) continue;
    const t = techSlugOf(d.tech);
    if (!t) continue;
    const share = total && total.kt > 0 && Number.isFinite(d.kt) ? d.kt / total.kt : null;
    const prev = out.get(t);
    if (!prev) out.set(t, { slug: t, label: TECH_LABELS[t], share });
    else if (share !== null) prev.share = share;
  }
  return [...out.values()].sort((a, z) => (z.share ?? -1) - (a.share ?? -1) || a.label.localeCompare(z.label, 'en'));
}

export interface MineralIndexRow {
  item: ItemInfo;
  /** USGS mine production (the default production source) */
  mine: SummaryInfo | null;
  /** IEA mining, latest observed year */
  mining: SummaryInfo | null;
  /** IEA refining, latest observed year */
  refining: SummaryInfo | null;
  /** USGS reserves */
  reserves: SummaryInfo | null;
  techs: TechUse[];
  /** observed clean-energy demand (kt), for ordering; NaN when unknown */
  demand: number;
}

/** One row per mineral, from meta.summary and dims (no facts needed). */
export function mineralIndexRows(model: SectorModel, eu: 'members' | 'bloc'): MineralIndexRow[] {
  const v = (id: string) => model.varById.get(id) ?? null;
  const mine = v(MINE_VAR);
  const mining = v(MINING_VAR);
  const refining = v(REFINING_VAR);
  const reserves = v(RESERVES_VAR);
  return mineralItems(model).map((item) => {
    const obs = observedDemand(model.mineralDemand.filter((d) => d.mineral === item.idx));
    const total = obs.find((d) => d.tech === null && /^total/i.test(d.label));
    const demand = total ? total.kt : obs.filter((d) => d.tech !== null).reduce((a, d) => a + (Number.isFinite(d.kt) ? d.kt : 0), 0) || NaN;
    return {
      item,
      mine: observedSummary(model, mine, item, eu),
      mining: observedSummary(model, mining, item, eu),
      refining: observedSummary(model, refining, item, eu),
      reserves: observedSummary(model, reserves, item, eu),
      techs: techsOfMineral(model, item),
      demand,
    };
  });
}

export interface DemandRow {
  tech: TechSlug | null;
  label: string;
  /** observed demand, kt */
  observed: number;
  observedYear: number;
  /** the latest STEPS projection, kt (NaN when none) */
  projected: number;
  projectedYear: number | null;
  scenario: string | null;
  /** share of observed total clean-energy demand */
  share: number | null;
}

/** IEA demand for a mineral by technology: observed vs the latest STEPS year (projection). */
export function demandByTech(model: SectorModel, mineral: ItemInfo): { rows: DemandRow[]; total: DemandRow | null } {
  const mine = model.mineralDemand.filter((d) => d.mineral === mineral.idx);
  const obs = observedDemand(mine);
  const steps = mine.filter((d) => /steps/i.test(d.scenario));
  const projYear = steps.length ? Math.max(...steps.map((d) => d.year)) : null;
  const totalObs = obs.find((d) => d.tech === null && /^total/i.test(d.label)) ?? null;
  const key = (d: MineralDemandRow) => d.tech ?? `label:${d.label}`;
  const rows: DemandRow[] = [];
  let total: DemandRow | null = null;
  for (const d of obs) {
    const p = projYear !== null ? steps.find((x) => x.year === projYear && key(x) === key(d)) : undefined;
    const row: DemandRow = {
      tech: d.tech ? techSlugOf(d.tech) : null,
      label: d.label,
      observed: d.kt,
      observedYear: d.year,
      projected: p ? p.kt : NaN,
      projectedYear: p ? p.year : null,
      scenario: p ? p.scenario : null,
      share: totalObs && totalObs.kt > 0 && Number.isFinite(d.kt) ? d.kt / totalObs.kt : null,
    };
    if (d.tech === null && /^total/i.test(d.label)) total = row;
    else rows.push(row);
  }
  rows.sort((a, z) => z.observed - a.observed);
  return { rows, total };
}

export interface SharePoint {
  x: number;
  y: number;
  projection?: boolean;
}

export interface ShareLine {
  id: string;
  label: string;
  /** China and the highlight in group colours; every other line neutral (gray) */
  group: SplitPart | 'neutral';
  geo: number;
  points: SharePoint[];
}

/**
 * Shares of the world total over the years of (var, item): the top `top` holders of the first and of
 * the last year, plus China and the highlight. Each line is the holder's value / W of that year.
 */
export function shareLines(model: SectorModel, facts: FactsIndex, v: VarInfo, item: ItemInfo, eu: 'members' | 'bloc', opts: { top?: number; highlight?: GeoInfo | null } = {}): ShareLine[] {
  const years = facts.yearsOf(v.idx, item.idx);
  if (!years.length) return [];
  const top = opts.top ?? 3;
  const concs = new Map(
    years.map((y) => {
      const s = slice(model, facts, v, item.idx, y, eu);
      return [y, concentration(s.rows, model.geos, s.world, s.basis)] as const;
    }),
  );
  const pick = new Set<number>();
  for (const y of [years[0], years[years.length - 1]]) for (const r of (concs.get(y)?.rows ?? []).slice(0, top)) if (r.value > 0) pick.add(r.geo);
  const china = model.geoByCode.get('CHN');
  if (china && [...concs.values()].some((c) => c.rows.some((r) => r.geo === china.idx))) pick.add(china.idx);
  if (opts.highlight && [...concs.values()].some((c) => c.rows.some((r) => r.geo === opts.highlight!.idx))) pick.add(opts.highlight.idx);
  const lines: ShareLine[] = [];
  for (const g of pick) {
    const geo = model.geos[g];
    if (!geo) continue;
    const coloured = geo.code === 'CHN' || geo.idx === opts.highlight?.idx;
    const points: SharePoint[] = [];
    for (const y of years) {
      const r = concs.get(y)?.rows.find((x) => x.geo === g);
      if (r && r.share !== null) points.push({ x: y, y: r.share, projection: isProjection(v, y) || undefined });
    }
    if (points.length) lines.push({ id: geo.code, label: geo.name, group: coloured ? partOf(geo) : 'neutral', geo: g, points });
  }
  const last = years[years.length - 1];
  const lastShare = (l: ShareLine) => l.points.find((p) => p.x === last)?.y ?? -1;
  return lines.sort((a, z) => lastShare(z) - lastShare(a) || a.label.localeCompare(z.label, 'en'));
}

export interface ScatterPoint {
  geo: number;
  code: string;
  name: string;
  group: Exclude<SplitPart, 'unattributed'>;
  /** share of world mine production */
  x: number;
  /** share of world reserves */
  y: number;
}

/**
 * Share of world production (x) against share of world reserves (y), both against their source's
 * world totals. Only holders with a finite value in both slices (zeros included); `only` counts the
 * holders with a value in one slice only (listed, never plotted at 0).
 */
export function shareScatter(model: SectorModel, facts: FactsIndex, prod: VarInfo, res: VarInfo, item: ItemInfo, prodYear: number, resYear: number, eu: 'members' | 'bloc'): { points: ScatterPoint[]; only: { production: number; reserves: number } } {
  const sp = slice(model, facts, prod, item.idx, prodYear, eu);
  const sr = slice(model, facts, res, item.idx, resYear, eu);
  const cp = concentration(sp.rows, model.geos, sp.world, sp.basis);
  const cr = concentration(sr.rows, model.geos, sr.world, sr.basis);
  const px = new Map(cp.rows.map((r) => [r.geo, r.share]));
  const ry = new Map(cr.rows.map((r) => [r.geo, r.share]));
  const points: ScatterPoint[] = [];
  let onlyP = 0;
  let onlyR = 0;
  for (const [g, x] of px) {
    const y = ry.get(g);
    if (x === null) continue;
    if (y === undefined || y === null) {
      onlyP++;
      continue;
    }
    const geo = model.geos[g];
    if (!isHolder(geo)) continue;
    const p = partOf(geo);
    points.push({ geo: g, code: geo.code, name: geo.name, group: p === 'unattributed' ? 'rest' : p, x, y });
  }
  for (const [g, y] of ry) if (y !== null && !px.has(g)) onlyR++;
  points.sort((a, z) => z.x + z.y - (a.x + a.y) || a.geo - z.geo);
  return { points, only: { production: onlyP, reserves: onlyR } };
}

/** The IMF commodity of a mineral: the commodity item with the same slug, else one whose slug starts with it. */
export function commodityOf(model: SectorModel, mineral: ItemInfo): ItemInfo | null {
  const same = model.item('commodity', mineral.slug);
  if (same) return same;
  return model.items.find((x) => x.axis === 'commodity' && (x.slug.startsWith(`${mineral.slug}-`) || x.label.toLowerCase().split(/[ ,]/)[0] === mineral.label.toLowerCase())) ?? null;
}
