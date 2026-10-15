// CSV of every Sector card (spec_sector.md 4.0): the same 18 columns everywhere (the 17 of 4.0 plus
// `note`, the fact's facts.notes in words, 2026-10-12), UTF-8 with BOM (added at download), NA empty. Values are written after the dictionary's display_scale, in the unit named in
// the `unit` column. Pure.

import { rowsToCsv } from '../../lib/csv.ts';
import type { GeoInfo, ItemInfo, SectorModel, UnitInfo, VarInfo } from '../data/model.ts';

export const SECTOR_CSV_COLUMNS = [
  'iso3c',
  'economy',
  'group',
  'tier',
  'eu_member',
  'var_id',
  'measure',
  'item',
  'year',
  'value',
  'unit',
  'status',
  'share_of_world',
  'world_basis',
  'rank',
  'source',
  'licence',
  'note',
] as const;

export type SectorCsvRow = Record<(typeof SECTOR_CSV_COLUMNS)[number], string | number | boolean | null>;

export interface CsvInput {
  geo: GeoInfo | null;
  v: VarInfo;
  item: ItemInfo | null;
  year: number | string | null;
  value: number | null;
  status: string | null;
  share?: number | null;
  rank?: number | null;
  /** a per-item unit override */
  unit?: UnitInfo;
  /** an economy label for non-geo rows ("Other exporters", "World") */
  economy?: string;
  /** the fact's notes in words (derive/notes.ts noteLine), e.g. "Production not reported separately; assumed 0" */
  note?: string | null;
}

const num = (x: number | null | undefined): number | null => (typeof x === 'number' && Number.isFinite(x) ? x : null);

export function csvRow(model: SectorModel, r: CsvInput): SectorCsvRow {
  const unit = r.unit ?? (r.item ? model.unitOf(r.v, r.item.idx) : r.v.unit);
  const value = num(r.value);
  return {
    iso3c: r.geo ? r.geo.code : null,
    economy: r.economy ?? (r.geo ? r.geo.name : null),
    group: r.geo ? r.geo.group : null,
    tier: r.geo ? r.geo.tier : null,
    eu_member: r.geo ? r.geo.eu_member : null,
    var_id: r.v.id,
    measure: r.v.label,
    item: r.item ? r.item.slug : null,
    year: r.year ?? null,
    value: value === null ? null : value * r.v.display_scale,
    unit: unit.short || unit.id,
    status: r.status,
    share_of_world: num(r.share),
    world_basis: r.v.world_basis,
    rank: num(r.rank),
    source: r.v.source.short + (r.v.source.edition ? ` ${r.v.source.edition}` : ''),
    licence: r.v.licence,
    note: r.note ? r.note : null,
  };
}

/** CSV text (no BOM; ChartCard adds it on download). */
export function sectorCsv(model: SectorModel, rows: readonly CsvInput[]): string {
  return rowsToCsv(rows.map((r) => csvRow(model, r)) as Record<string, unknown>[], [...SECTOR_CSV_COLUMNS]);
}

/** The header line, for tests. */
export const SECTOR_CSV_HEADER = SECTOR_CSV_COLUMNS.join(',');
