// Generated sentences (spec_sector.md 4.0 summaries, 4.3 S1 sub lines, 4.4 lede). Descriptive words
// only: "holds", "share", "imports from"; never "risky", "secure", "good" or "bad". Pure.

import type { GeoInfo, SectorModel, VarInfo } from '../data/model.ts';
import { lc, fmtShare, ordinal } from '../format.ts';
import type { Concentration } from './concentration.ts';
import { basisWords } from './concentration.ts';

const pctWords = (x: number | null | undefined): string => (typeof x === 'number' && Number.isFinite(x) ? `${(x * 100).toFixed(1)} percent` : 'an unknown share');

/** "China leads with 43.7 percent of world exports; coalition members hold 51.9 percent; the United States ranks 4th of 198." */
export function concentrationSummary(c: Concentration, model: SectorModel, v: VarInfo, highlight?: GeoInfo | null): string {
  if (!c.top1 || c.W === null) {
    const n = c.rows.length;
    return n ? `${n} economies have a value for ${lc(v.short)}.` : `No values for ${lc(v.short)}.`;
  }
  const top = model.geos[c.top1.geo];
  const words = basisWords(c.basis, v.source.short);
  const parts = [`${top?.name ?? 'The largest holder'} leads with ${pctWords(c.top1.share)} ${words ? `${words} ` : ''}${lc(v.short)}`];
  const coal = c.split.core + c.split.partner;
  if (Number.isFinite(coal)) parts.push(`coalition members hold ${pctWords(coal)}`);
  if (highlight && highlight.idx !== c.top1.geo) {
    const p = c.rows.find((r) => r.geo === highlight.idx);
    parts.push(p ? `${theName(highlight)} ranks ${ordinal(p.rank)} of ${c.n} with ${pctWords(p.share)}` : `${theName(highlight)} has no value`);
  }
  return `${parts.join('; ')}.`;
}

/** "the United States", "China" (articles for the few names that take one). */
export function theName(g: GeoInfo): string {
  const n = g.name;
  return /^(United|Netherlands|Philippines|Czech|Dominican|Central African|Gambia|Bahamas|European Union)/.test(n) ? `the ${n}` : n;
}

/** "China 43.7%" style lens text for a tile. */
export function lensText(lens: string, s: { top1_share: number; top3_share: number; hhi: number; hhi_lower_bound: boolean; china: number; core: number; partner: number; top1_geo: number | null }, model: SectorModel): string {
  switch (lens) {
    case 'top1':
      return `${s.top1_geo !== null ? (model.geos[s.top1_geo]?.code ?? 'Top-1') : 'Top-1'} ${fmtShare(s.top1_share)}`;
    case 'top3':
      return `Top-3 ${fmtShare(s.top3_share)}`;
    case 'hhi':
      return `HHI ${s.hhi_lower_bound ? `≥ ${s.hhi.toFixed(2)}` : Number.isFinite(s.hhi) ? s.hhi.toFixed(3) : '–'}`;
    case 'coalition':
      return `Coalition ${fmtShare(s.core + s.partner)}`;
    default:
      return `China ${fmtShare(s.china)}`;
  }
}

/** The value of a lens on [0, 1] (for the tile shade). HHI is already on [0, 1]. */
export function lensValue(lens: string, s: { top1_share: number; top3_share: number; hhi: number; china: number; core: number; partner: number }): number {
  switch (lens) {
    case 'top1':
      return s.top1_share;
    case 'top3':
      return s.top3_share;
    case 'hhi':
      return s.hhi;
    case 'coalition':
      return s.core + s.partner;
    default:
      return s.china;
  }
}

export const LENS_LABEL: Record<string, string> = {
  china: 'China share',
  top1: 'Top-1 share',
  top3: 'Top-3 share',
  hhi: 'HHI',
  coalition: 'Coalition share',
};
