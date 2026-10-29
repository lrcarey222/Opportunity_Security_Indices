// Concentration across sectors (spec_sector.md 4.7). Pure, from meta.summary only (no facts): the
// board (technology rows with mineral sub-rows x chain-ordered columns, one summary per tile), the
// share split of one family by stage (plus a minerals panel), the table of every item x measure, and
// the trends (matrix HHI 2020 -> 2024 per trade cell; IEA top-3 base year -> last projection per
// mineral and step). Every number is within one measure, one source and one year; nothing is added
// across tiles.

import { FAMILY_SECTION, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug, type TechSlug } from '../contract.ts';
import type { ItemInfo, SectorModel, SummaryInfo, VarInfo } from '../data/model.ts';
import { familyMeasure, measureItem, primaryMeasure } from './cells.ts';
import { lensValue } from './text.ts';
import type { FamilyId } from '../contract.ts';

export type BoardColKey =
  | 'reserves'
  | 'mining'
  | 'exports_extraction'
  | 'refining'
  | 'exports_refining'
  | 'investment_refining'
  | 'exports_components'
  | 'exports_assembly'
  | 'capacity'
  | 'investment_manufacturing'
  | 'generation'
  | 'investment_deployment'
  | 'exports_recovery';

export interface BoardColDef {
  key: BoardColKey;
  label: string;
  stage: StageSlug;
  /** candidate vars of a technology row (first with a summary wins), measured on the stage's cell or its linked fuel */
  vars: readonly string[];
  /**
   * candidate vars of a mineral sub-row; a var on the mineral_stage axis (the critical-minerals trade,
   * schema 2) is read on the mineral's item at the column's stage ("lithium-refining")
   */
  mineralVars: readonly string[];
  /** the anchor on a mineral page */
  mineralAnchor?: string;
}

/**
 * The board columns in chain order (4.7), at the six stages (2026-10-28): Extraction (reserves, mining,
 * exports), Refining (refined output, exports, the critical-minerals capex), Components and Assembly
 * (exports; the manufacturing capacity shares and capex, which the sources give once for the two stages
 * and the bundle carries on both: shown once, on Assembly), Deployment (generation, investment) and
 * Recovery (exports). Columns without data are dropped.
 */
export const BOARD_COLS: readonly BoardColDef[] = [
  { key: 'reserves', label: 'Reserves', stage: 'extraction', vars: ['resources.fuel_reserves', 'resources.uranium_resources'], mineralVars: ['resources.mineral_reserves'], mineralAnchor: 'reserves' },
  { key: 'mining', label: 'Mining', stage: 'extraction', vars: ['production.fuels', 'production.uranium'], mineralVars: ['production.mining', 'production.mine_usgs'], mineralAnchor: 'mining-refining' },
  { key: 'exports_extraction', label: 'Exports: Extraction', stage: 'extraction', vars: ['trade.exports'], mineralVars: ['trade.critmin_exports'], mineralAnchor: 'trade' },
  { key: 'refining', label: 'Refining', stage: 'refining', vars: [], mineralVars: ['production.refining'], mineralAnchor: 'mining-refining' },
  { key: 'exports_refining', label: 'Exports: Refining', stage: 'refining', vars: ['trade.exports'], mineralVars: ['trade.critmin_exports'], mineralAnchor: 'trade' },
  { key: 'investment_refining', label: 'Investment: Refining', stage: 'refining', vars: ['investment.annual'], mineralVars: [] },
  { key: 'exports_components', label: 'Exports: Components', stage: 'components', vars: ['trade.exports'], mineralVars: [] },
  { key: 'exports_assembly', label: 'Exports: Assembly', stage: 'assembly', vars: ['trade.exports'], mineralVars: [] },
  { key: 'capacity', label: 'Manufacturing capacity', stage: 'assembly', vars: ['manufacturing.share_now'], mineralVars: [] },
  { key: 'investment_manufacturing', label: 'Manufacturing investment', stage: 'assembly', vars: ['investment.annual'], mineralVars: [] },
  { key: 'generation', label: 'Generation', stage: 'deployment', vars: ['production.generation'], mineralVars: [] },
  { key: 'investment_deployment', label: 'Investment: Deployment', stage: 'deployment', vars: ['investment.annual'], mineralVars: [] },
  { key: 'exports_recovery', label: 'Exports: Recovery', stage: 'recovery', vars: ['trade.exports'], mineralVars: ['trade.critmin_exports'], mineralAnchor: 'trade' },
];

export interface BoardTile {
  v: VarInfo;
  item: ItemInfo;
  s: SummaryInfo;
  /** the profile section or mineral page it opens (path + hash, no query) */
  path: string;
  hash: string;
}

export interface BoardRowDef {
  key: string;
  label: string;
  tech: TechSlug | null;
  mineral: ItemInfo | null;
  children: BoardRowDef[];
}

export interface Board {
  rows: BoardRowDef[];
  cols: BoardColDef[];
  tiles: Map<string, BoardTile>;
}

const tileKey = (row: string, col: string) => `${row}|${col}`;

function techTile(model: SectorModel, tech: TechSlug, col: BoardColDef, eu: 'members' | 'bloc'): BoardTile | null {
  const cell = model.cell(tech, col.stage);
  if (!cell) return null;
  for (const id of col.vars) {
    const v = model.varById.get(id);
    if (!v) continue;
    const item = measureItem(model, cell, v);
    if (!item) continue;
    const s = model.summaryLatest(item.idx, v.idx, eu);
    if (!s || !Number.isFinite(s.W)) continue;
    const section = (v.section as string | null) ?? FAMILY_SECTION[v.family] ?? '';
    return { v, item, s, path: `/sector/${tech}/${col.stage}`, hash: section };
  }
  return null;
}

function mineralTile(model: SectorModel, mineral: ItemInfo, col: BoardColDef, eu: 'members' | 'bloc'): BoardTile | null {
  for (const id of col.mineralVars) {
    const v = model.varById.get(id);
    if (!v) continue;
    // a mineral_stage measure (the critical-minerals trade) is read at the column's stage
    const item = v.item_axis === 'mineral_stage' ? model.item('mineral_stage', `${mineral.slug}-${col.stage}`) : mineral;
    if (!item) continue;
    const s = model.summaryLatest(item.idx, v.idx, eu);
    if (!s || !Number.isFinite(s.W)) continue;
    return { v, item, s, path: `/sector/minerals/${mineral.slug}`, hash: col.mineralAnchor ?? '' };
  }
  return null;
}

/** Minerals linked to any cell of a technology, heaviest first, de-duplicated. */
export function mineralsOfTech(model: SectorModel, tech: TechSlug): ItemInfo[] {
  const best = new Map<number, { item: ItemInfo; w: number }>();
  for (const cell of model.cells.filter((c) => c.techSlug === tech)) {
    for (const { item, weight } of model.itemsOfCell(cell.idx, 'mineral')) {
      const w = Number.isFinite(weight) ? weight : 0;
      const prev = best.get(item.idx);
      if (!prev || w > prev.w) best.set(item.idx, { item, w });
    }
  }
  return [...best.values()].sort((a, z) => z.w - a.w || a.item.sort - z.item.sort).map((x) => x.item);
}

/** The concentration board: technology rows (mineral sub-rows) x chain columns with data. */
export function concentrationBoard(model: SectorModel, eu: 'members' | 'bloc'): Board {
  const tiles = new Map<string, BoardTile>();
  const rows: BoardRowDef[] = [];
  const used = new Set<BoardColKey>();
  for (const tech of TECH_SLUGS) {
    if (!model.cells.some((c) => c.techSlug === tech)) continue;
    const key = tech;
    for (const col of BOARD_COLS) {
      const t = techTile(model, tech, col, eu);
      if (t) {
        tiles.set(tileKey(key, col.key), t);
        used.add(col.key);
      }
    }
    const children: BoardRowDef[] = [];
    for (const m of mineralsOfTech(model, tech)) {
      const mk = `${tech}:${m.slug}`;
      let any = false;
      for (const col of BOARD_COLS) {
        const t = mineralTile(model, m, col, eu);
        if (t) {
          tiles.set(tileKey(mk, col.key), t);
          used.add(col.key);
          any = true;
        }
      }
      if (any) children.push({ key: mk, label: m.label, tech, mineral: m, children: [] });
    }
    rows.push({ key, label: TECH_LABELS[tech], tech, mineral: null, children });
  }
  return { rows, cols: BOARD_COLS.filter((c) => used.has(c.key)), tiles };
}

/** A tile of the board, or null (no data: hatched). */
export function boardTile(board: Board, row: string, col: string): BoardTile | null {
  return board.tiles.get(tileKey(row, col)) ?? null;
}

// ---- share split by sector -----------------------------------------------------------------------

export type FamChoice = 'primary' | FamilyId;

export interface SplitEntry {
  /** the cell (or mineral) the row is about */
  of: ItemInfo;
  v: VarInfo;
  /** the item the measure is on (the cell, its fuel, or the mineral) */
  item: ItemInfo;
  s: SummaryInfo;
  /** a stage slug ('extraction' ... 'recovery'), 'minerals' or 'other' */
  section: string;
  lens: number;
  words?: string;
}

/**
 * Rows of the share split for a family: one per cell with a summary, grouped by stage and sorted by
 * the lens (largest first; HHI and shares alike); plus a minerals panel (IEA mining and refining).
 */
export function familySplit(model: SectorModel, fam: FamChoice, lens: string, eu: 'members' | 'bloc'): SplitEntry[] {
  const out: SplitEntry[] = [];
  for (const cell of model.cells) {
    const cm = fam === 'primary' ? primaryMeasure(model, cell) : familyMeasure(model, cell, fam);
    if (!cm) continue;
    const s = model.summaryLatest(cm.item.idx, cm.v.idx, eu);
    if (!s || !Number.isFinite(s.W)) continue;
    out.push({ of: cell, v: cm.v, item: cm.item, s, section: cell.stageSlug ?? 'other', lens: lensValue(lens, s) });
  }
  if (fam === 'primary' || fam === 'production') {
    for (const m of model.items.filter((x) => x.axis === 'mineral')) {
      for (const [id, words] of [['production.mining', 'mining'], ['production.refining', 'refining']] as const) {
        const v = model.varById.get(id);
        if (!v) continue;
        const s = model.summaryLatest(m.idx, v.idx, eu);
        if (!s || !Number.isFinite(s.W)) continue;
        out.push({ of: m, v, item: m, s, section: 'minerals', lens: lensValue(lens, s), words });
      }
    }
  }
  const order: string[] = [...STAGE_SLUGS, 'minerals', 'other'];
  return out.sort((a, z) => order.indexOf(a.section) - order.indexOf(z.section) || (Number.isFinite(z.lens) ? z.lens : -1) - (Number.isFinite(a.lens) ? a.lens : -1) || a.of.sort - z.of.sort);
}

/** The family choices that have at least one row (primary first). */
export function familyChoices(model: SectorModel, eu: 'members' | 'bloc'): FamChoice[] {
  const fams: FamChoice[] = ['primary', 'exports', 'investment', 'production', 'capacity', 'reserves', 'consumption'];
  return fams.filter((f) => familySplit(model, f, 'china', eu).length > 0);
}

// ---- the table of every item x measure ----------------------------------------------------------

/** The latest observed summary row of every (item, var) in an EU mode, in item then dictionary order. */
export function latestSummaries(model: SectorModel, eu: 'members' | 'bloc'): SummaryInfo[] {
  const best = new Map<string, SummaryInfo>();
  for (const s of model.summary) {
    if (s.eu_mode !== eu) continue;
    const v = model.vars[s.var];
    if (!v || (v.projection_from !== null && s.year >= v.projection_from)) continue;
    const k = `${s.item}|${s.var}`;
    const prev = best.get(k);
    if (!prev || s.year > prev.year) best.set(k, s);
  }
  return [...best.values()].sort((a, z) => (model.items[a.item]?.sort ?? 0) - (model.items[z.item]?.sort ?? 0) || a.var - z.var);
}

// ---- trends ------------------------------------------------------------------------------------

export interface TrendRow {
  item: ItemInfo;
  v: VarInfo;
  /** the earlier year (or the projection) */
  a: SummaryInfo;
  /** the later year (or the observation) */
  b: SummaryInfo;
  words: string;
}

/** Matrix HHI 2020 -> 2024 per trade cell (trade.exports). */
export function tradeHhiTrend(model: SectorModel, eu: 'members' | 'bloc'): TrendRow[] {
  const v = model.varById.get('trade.exports');
  if (!v) return [];
  const out: TrendRow[] = [];
  for (const cell of model.cells) {
    const a = model.summaryOf(cell.idx, v.idx, 2020, eu);
    const b = model.summaryOf(cell.idx, v.idx, 2024, eu);
    if (a && b && Number.isFinite(a.hhi) && Number.isFinite(b.hhi)) out.push({ item: cell, v, a, b, words: model.cellLabel(cell) });
  }
  return out.sort((x, z) => z.b.hhi - x.b.hhi);
}

/** IEA top-3 share, base year -> last projected year, per mineral and step (a = the projection). */
export function mineralTop3Trend(model: SectorModel, eu: 'members' | 'bloc'): TrendRow[] {
  const out: TrendRow[] = [];
  for (const [id, step] of [['production.mining', 'mining'], ['production.refining', 'refining']] as const) {
    const v = model.varById.get(id);
    if (!v) continue;
    for (const m of model.items.filter((x) => x.axis === 'mineral')) {
      const rows = model.summary.filter((s) => s.item === m.idx && s.var === v.idx && s.eu_mode === eu && Number.isFinite(s.top3_share));
      if (rows.length < 2) continue;
      const obs = rows.filter((s) => v.projection_from === null || s.year < v.projection_from).sort((x, z) => z.year - x.year)[0];
      const last = rows.slice().sort((x, z) => z.year - x.year)[0];
      if (!obs || !last || last.year === obs.year) continue;
      out.push({ item: m, v, a: last, b: obs, words: `${m.label} ${step}` });
    }
  }
  return out.sort((x, z) => z.b.top3_share - x.b.top3_share);
}
