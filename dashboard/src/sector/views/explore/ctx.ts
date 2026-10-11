// What every Explore layout receives (spec_sector.md 4.6): the measure, its item and year (resolved
// from the URL), the facts and series indexes (the year's slice reads facts when the year is there,
// else the series chunk: a (var, item, geo, year) in both carries the identical value), the highlight,
// the EU mode, the economies scope, Show-as and the scale.

import type { FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';
import type { GeoScope, ScaleParam, ShowAs } from '../../url.ts';

export interface ExploreCtx {
  model: SectorModel;
  theme: string;
  v: VarInfo;
  item: ItemInfo | null;
  /** the selected year (null for a measure without years in the loaded chunks) */
  year: number | null;
  /** every year of (v, item) in facts and series, ascending */
  years: number[];
  facts: FactsIndex | null;
  series: FactsIndex | null;
  highlight: GeoInfo | null;
  eu: 'members' | 'bloc';
  scope: GeoScope;
  as: ShowAs;
  scale: ScaleParam | null;
  setYear: (y: number) => void;
  /** select an item (cell items by tech + stage) */
  setItem: (it: ItemInfo) => void;
  setHighlight: (g: GeoInfo | null) => void;
}

/** The index holding (v, item, year): facts first, else series. */
export function indexFor(c: Pick<ExploreCtx, 'facts' | 'series' | 'v' | 'item'>, year: number | null): FactsIndex | null {
  if (!c.item || year === null) return null;
  if (c.facts?.has(c.v.idx, c.item.idx, year)) return c.facts;
  if (c.series?.has(c.v.idx, c.item.idx, year)) return c.series;
  return null;
}

/** The index with the most years of (v, item) (the series chunk for EI 1965-2025), for "Over time". */
export function timeIndex(c: Pick<ExploreCtx, 'facts' | 'series' | 'v' | 'item'>): FactsIndex | null {
  if (!c.item) return null;
  const nf = c.facts?.yearsOf(c.v.idx, c.item.idx).length ?? 0;
  const ns = c.series?.yearsOf(c.v.idx, c.item.idx).length ?? 0;
  if (!nf && !ns) return null;
  return ns > nf ? c.series : c.facts;
}
