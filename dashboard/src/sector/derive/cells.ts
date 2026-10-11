// Cells, families and coverage (spec_sector.md 3.3, 4.3 S0): which measures a cell has, its primary
// measure, the primary var of each family, coverage chips, the thin rule and the stage glyph. Pure.

import { FAMILY_IDS, FAMILY_SECTION, type FamilyId, type SectionId } from '../contract.ts';
import type { FactsIndex } from '../data/facts.ts';
import type { CoverageInfo, ItemInfo, SectorModel, VarInfo } from '../data/model.ts';

/**
 * The "lens" families (3.3): the measure families that describe a sector's market (the rows of S2 and
 * the Concentration board). Policy records, stage context and development exist for nearly every
 * economy in every cell, so they never make a cell rich (scripts/87 SECTOR_LENS_FAMILIES; the
 * manifest's coverage_glyph uses the same set).
 */
export const LENS_FAMILIES: readonly FamilyId[] = ['exports', 'production', 'capacity', 'reserves', 'investment', 'consumption', 'prices'];
const LENS_SET: ReadonlySet<string> = new Set(LENS_FAMILIES);

/** Lens families with country data (coverage status 'country'). */
export function countryFamilies(model: SectorModel, item: ItemInfo): FamilyId[] {
  return model.coverageOf(item.idx).filter((c) => c.status === 'country' && LENS_SET.has(c.family)).map((c) => c.family);
}

/** "Thin" = at most one lens family with country data (3.3). */
export function isThin(model: SectorModel, item: ItemInfo): boolean {
  return countryFamilies(model, item).length <= 1;
}

/** The stage glyph of a cell: rich (4+ lens families with data), some, thin. */
export function richness(model: SectorModel, item: ItemInfo): 'rich' | 'some' | 'thin' {
  const n = model.coverageOf(item.idx).filter((c) => LENS_SET.has(c.family) && (c.status === 'country' || c.status === 'region' || c.status === 'global')).length;
  if (isThin(model, item)) return 'thin';
  return n >= 4 ? 'rich' : 'some';
}

export const RICHNESS_GLYPH: Record<'rich' | 'some' | 'thin', { glyph: string; words: string }> = {
  rich: { glyph: '■', words: 'rich: 4 or more measure families' },
  some: { glyph: '◧', words: 'some data' },
  thin: { glyph: '□', words: 'thin: little country-level data' },
};

/** Vars with data for an item (avail rows), in dictionary order. */
export function varsOfItem(model: SectorModel, item: ItemInfo, chunk?: string): VarInfo[] {
  const out: VarInfo[] = [];
  for (const v of model.vars) {
    const rows = model.availOf(v, item.idx);
    if (rows.length && (!chunk || rows.some((r) => r.chunk === chunk))) out.push(v);
  }
  return out;
}

/** Item axes whose items stand for a cell's own measure (EI fuels: Oil Upstream production is fuel:oil). */
const CELL_LINKED_AXES: ReadonlySet<string> = new Set(['fuel']);

/** The var ids dict.cells.section_cards lists for a cell (scripts/87), or null when it lists none. */
function cardVars(model: SectorModel, cell: ItemInfo): Set<string> | null {
  const ids = (model.cellText.get(cell.slug)?.section_cards ?? []).filter((c) => model.varById.has(c));
  return ids.length ? new Set(ids) : null;
}

/**
 * The item a var is measured on for a cell (spec 3.1, 3.3): the cell itself when the var has data for
 * it; else the cell's linked fuel (heaviest link first) when the dictionary ties the var to the cell
 * (dict.cells.section_cards lists it, or its primary_for names the cell). Oil Upstream production is
 * `production.fuels` on fuel:oil; Oil Downstream does not show it. Non-cell items resolve to
 * themselves. Null when the var has no data for the cell.
 */
export function measureItem(model: SectorModel, cell: ItemInfo, v: VarInfo, chunk?: string): ItemInfo | null {
  const has = (it: ItemInfo) => model.availOf(v, it.idx).some((a) => !chunk || a.chunk === chunk);
  if (has(cell)) return cell;
  if (cell.axis !== 'cell' || !CELL_LINKED_AXES.has(v.item_axis)) return null;
  const cards = cardVars(model, cell);
  if (!(cards?.has(v.id) || v.primary_for.includes(cell.slug))) return null;
  for (const { item } of model.itemsOfCell(cell.idx, v.item_axis)) if (has(item)) return item;
  return null;
}

/** Vars measured for a cell (on the cell or its linked fuel), in dictionary order. */
export function varsOfCell(model: SectorModel, cell: ItemInfo, chunk?: string): VarInfo[] {
  return model.vars.filter((v) => measureItem(model, cell, v, chunk) !== null);
}

/** A var and the item it is measured on for a cell. */
export interface CellMeasure {
  v: VarInfo;
  item: ItemInfo;
}

/** The measure of a var for a cell (`measureItem`), or null. */
export function cellMeasure(model: SectorModel, cell: ItemInfo, v: VarInfo | null | undefined): CellMeasure | null {
  if (!v) return null;
  const item = measureItem(model, cell, v);
  return item ? { v, item } : null;
}

/** The cell's primary measure (dictionary primary_for), or the first family primary with data. */
export function primaryVar(model: SectorModel, item: ItemInfo): VarInfo | null {
  const p = model.primaryVarsOf(item.slug).find((v) => measureItem(model, item, v) !== null);
  if (p) return p;
  for (const f of FAMILY_IDS) {
    const v = familyVar(model, item, f);
    if (v && v.native_level === 'country') return v;
  }
  return null;
}

/** The primary measure of a cell with the item it is measured on. */
export function primaryMeasure(model: SectorModel, cell: ItemInfo): CellMeasure | null {
  return cellMeasure(model, cell, primaryVar(model, cell));
}

/**
 * The primary var of a family for an item: a var of that family with data for the item and a summary
 * row, preferring one whose primary_for lists the cell; country-level vars before others.
 */
export function familyVar(model: SectorModel, item: ItemInfo, family: FamilyId): VarInfo | null {
  const cands = varsOfCell(model, item).filter((v) => v.family === family);
  if (!cands.length) return null;
  const score = (v: VarInfo) => {
    const it = measureItem(model, item, v) ?? item;
    return (v.primary_for.includes(item.slug) ? 0 : 10) + (model.summary.some((s) => s.item === it.idx && s.var === v.idx) ? 0 : 5) + (v.native_level === 'country' ? 0 : 2);
  };
  return cands.slice().sort((a, z) => score(a) - score(z) || a.idx - z.idx)[0];
}

/** The family's measure of a cell with the item it is measured on. */
export function familyMeasure(model: SectorModel, cell: ItemInfo, family: FamilyId): CellMeasure | null {
  return cellMeasure(model, cell, familyVar(model, cell, family));
}

/** Country-level vars of a cell that have a world basis (rows of the S2 share split). */
export function shareVars(model: SectorModel, item: ItemInfo): VarInfo[] {
  return varsOfCell(model, item, 'facts').filter((v) => v.native_level === 'country' && v.world_basis !== 'none' && (v.agg === 'sum' || v.world_basis === 'one'));
}

/** Entity- and region-level vars of a cell (the S2 sub-table). */
export function entityVars(model: SectorModel, item: ItemInfo): VarInfo[] {
  return varsOfCell(model, item, 'facts').filter((v) => v.native_level === 'entity' || v.native_level === 'region');
}

/** Country-level vars of a cell (leaders, countries table). */
export function countryVars(model: SectorModel, item: ItemInfo): VarInfo[] {
  return varsOfCell(model, item, 'facts').filter((v) => v.native_level === 'country');
}

/** The profile sections that have data for a cell, in rendering order. */
export function sectionsWithData(model: SectorModel, item: ItemInfo): Set<SectionId> {
  const out = new Set<SectionId>();
  for (const c of model.coverageOf(item.idx)) {
    if (c.status === 'none' || c.status === 'licensed') continue;
    const s = FAMILY_SECTION[c.family];
    if (s) out.add(s);
  }
  for (const v of varsOfItem(model, item)) {
    const s = (v.section as SectionId | null) ?? FAMILY_SECTION[v.family];
    if (s) out.add(s);
  }
  return out;
}

export const COVERAGE_REASON_TEXT: Record<string, string> = {
  no_basket: 'No HS6 trade basket for this sector.',
  index_only: 'The pipeline holds only an index for this, not a measured value.',
  not_applicable: 'Not applicable to this stage.',
  no_source: 'No source with country data was found.',
  licensed: 'Licensed data (BNEF, BCG): not part of the public explorer.',
  mineral_level: 'Measured per mineral: see the mineral pages.',
  thin: 'Little country-level data.',
};

/** Words of a coverage reason code. */
export function reasonText(code: string | null | undefined): string {
  return (code && COVERAGE_REASON_TEXT[code]) || (code ? code.replace(/_/g, ' ') : 'No data for this sector.');
}

export interface Chip {
  family: FamilyId;
  label: string;
  icon: '✓' | '◐' | '○' | '—' | '';
  iconWords: string;
  text: string;
  section: SectionId | null;
  hasData: boolean;
  reason: string | null;
}

/** Coverage chips of a cell, one per family that has a coverage row (4.3 S0). */
export function coverageChips(model: SectorModel, item: ItemInfo): Chip[] {
  return model.coverageOf(item.idx).map((c: CoverageInfo) => {
    const fam = model.families.get(c.family);
    const label = fam?.label ?? c.family;
    const src = c.sources.length ? c.sources.map((s) => model.sources.get(s)?.short ?? s).join(', ') : '';
    const hasData = c.status === 'country' || c.status === 'region' || c.status === 'global';
    const icon = c.status === 'country' ? '✓' : c.status === 'region' ? '◐' : c.status === 'global' ? '○' : c.status === 'licensed' ? '' : '—';
    const iconWords = c.status === 'country' ? 'country data' : c.status === 'region' ? 'region or entity data' : c.status === 'global' ? 'global value only' : c.status === 'licensed' ? 'licensed' : 'not available';
    const text =
      c.status === 'country'
        ? `${src}${c.n_geo ? `, ${c.n_geo} economies` : ''}`
        : c.status === 'region'
          ? `${src}${c.n_geo ? `, ${c.n_geo} entities` : ''}`
          : c.status === 'global'
            ? `${src ? `${src}, ` : ''}global value`
            : c.status === 'licensed'
              ? 'Licensed: not in this build'
              : reasonText(c.reason).replace(/\.$/, '').toLowerCase();
    return { family: c.family, label, icon, iconWords, text, section: (fam?.section as SectionId | null) ?? FAMILY_SECTION[c.family], hasData, reason: hasData ? null : reasonText(c.reason) };
  });
}

/** The latest year of a var for an item from facts (or the avail table before facts load). */
export function latestYearOf(model: SectorModel, facts: FactsIndex | null, v: VarInfo, item: number, max?: number): number | null {
  if (facts) return facts.latestYear(v.idx, item, max);
  const a = model.availOf(v, item).filter((r) => r.chunk === 'facts');
  const ys = a.map((r) => r.year_max).filter((y): y is number => y !== null && (max === undefined || y <= max));
  return ys.length ? Math.max(...ys) : null;
}

/** Thin-cell notice text (3.3). */
export function thinNotice(model: SectorModel, item: ItemInfo): string {
  const label = model.cellLabel(item);
  if (item.techSlug === 'geothermal' && item.stageSlug !== 'downstream') return `Little country-level data exists for ${label}. The resource and cost data sit in Geothermal — Downstream.`;
  if (item.techSlug === 'electric-grid' && item.stageSlug === 'downstream') return `Little country-level data exists for ${label}. The equipment trade sits in Electric Grid — Midstream.`;
  if (item.stageSlug === 'upstream') return `Little country-level data exists for ${label}. The materials behind ${item.tech} are measured per mineral: see the minerals below.`;
  return `Little country-level data exists for ${label}.`;
}
