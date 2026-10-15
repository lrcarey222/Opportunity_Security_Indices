// Per-fact notes (facts.notes; contract FACTS_SPEC) and the assumed-zero rule of the net balance (user
// edit 2b of 2026-10-11): where no source reports an economy's production of a fuel separately (it sits
// inside EI's "Other" / "Rest of World" total and no EIA value is used), production is assumed to be 0
// and said so. The balance is then minus consumption and self-sufficiency 0%: status nsr WITH a value.
// Such facts are drawn as values (map fill, bar, dumbbell) with a dagger and the note, never as a
// hatch. Pure.

import type { FactRow } from '../data/facts.ts';
import type { SectorModel } from '../data/model.ts';

/** The measures whose nsr rows carry the value an assumed zero production gives. */
export const ASSUMED_ZERO_VARS: ReadonlySet<string> = new Set(['consumption.balance', 'consumption.self_sufficiency']);

/** The mark printed after a value that rests on an assumed zero. */
export const ASSUMED_MARK = '†';

/** The short badge after the economy's name in rankings (the label column is narrow; the note says the rest). */
export const ASSUMED_BADGE = 'assumed 0';

/** The note of an assumed-zero fact, in the words the tooltips, tables and card notes use. */
export const ASSUMED_ZERO_WORDS = 'Production not reported separately; assumed 0';

/** Short words of the note ids of this build (dict.caveats ids); other ids fall back to the caveat text. */
export const NOTE_SHORT: Readonly<Record<string, string>> = {
  production_assumed_zero: ASSUMED_ZERO_WORDS,
  fossil_assumed_zero: 'A fuel not reported separately counts as 0 in the fossil sum',
  eia_production: 'Production from the EIA International Energy Statistics',
  eia_consumption: 'Consumption from the EIA International Energy Statistics',
  ei_oil_shale: 'Estonia: EI counts oil shale as coal; coal production not reported',
  ei_residual: 'Not reported separately: inside the source’s regional total',
};

/** Does this fact rest on an assumed zero production (drawn as a value, with the note)? */
export function isAssumedZero(varId: string, row: Pick<FactRow, 'status' | 'value' | 'notes'> | null | undefined): boolean {
  if (!row || row.status !== 'nsr' || !Number.isFinite(row.value)) return false;
  return ASSUMED_ZERO_VARS.has(varId) || !!row.notes?.includes('production_assumed_zero');
}

/** The first sentence of a text (caveat fallbacks stay short in tooltips). */
function firstSentence(s: string): string {
  const m = /^(.+?[.;])(\s|$)/.exec(s);
  return (m ? m[1] : s).replace(/[.;]$/, '');
}

/** The words of one note id; `obsYear` is added to the EIA production note ("…, 2024"). */
export function noteText(model: Pick<SectorModel, 'caveats'> | null, id: string, opts: { obsYear?: number | null; varId?: string } = {}): string {
  if (id === 'eia_production' && opts.varId === 'production.fuels') return `EIA International Energy Statistics${opts.obsYear ? `, ${opts.obsYear}` : ''}`;
  const short = NOTE_SHORT[id];
  if (short) return short;
  const t = model?.caveats.get(id);
  return t ? firstSentence(t) : id.replace(/_/g, ' ');
}

/**
 * The notes of a fact as words, the assumed-zero note first. An assumed-zero fact without a notes row
 * (a build before facts.notes) still gets its note.
 */
export function noteWords(model: Pick<SectorModel, 'caveats'> | null, varId: string, row: Pick<FactRow, 'status' | 'value' | 'notes' | 'obs_year'> | null | undefined): string[] {
  if (!row) return [];
  const ids = [...(row.notes ?? [])];
  if (isAssumedZero(varId, row) && !ids.includes('production_assumed_zero')) ids.unshift('production_assumed_zero');
  ids.sort((a, z) => (a === 'production_assumed_zero' ? -1 : z === 'production_assumed_zero' ? 1 : 0));
  return [...new Set(ids.map((id) => noteText(model, id, { obsYear: row.obs_year, varId })))];
}

/** The notes of a fact joined for a table cell or a CSV ("" when none). */
export function noteLine(model: Pick<SectorModel, 'caveats'> | null, varId: string, row: Pick<FactRow, 'status' | 'value' | 'notes' | 'obs_year'> | null | undefined): string {
  return noteWords(model, varId, row).join('; ');
}

/** How many of a slice's rows rest on an assumed zero. */
export function assumedCount(varId: string, rows: readonly Pick<FactRow, 'status' | 'value' | 'notes'>[]): number {
  let n = 0;
  for (const r of rows) if (isAssumedZero(varId, r)) n++;
  return n;
}

/** The card note of a slice with assumed-zero facts: what was assumed and what the value then is. */
export function assumedNote(varId: string, n: number): string {
  const what = varId === 'consumption.self_sufficiency' ? 'self-sufficiency is then 0%' : 'the net balance is then minus consumption, a net import';
  return `${ASSUMED_MARK} ${ASSUMED_ZERO_WORDS} (${n} ${n === 1 ? 'economy' : 'economies'}): no source reports the production separately (it sits inside a regional "rest of world" total), so it counts as 0 and ${what}.`;
}
