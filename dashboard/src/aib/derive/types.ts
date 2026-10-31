// The compact types in words: a short tag, a one-line explanation and a tone (plain words, the NEIS copy
// refresh 2026-10-30: "Potential Sectoral Compact", "No compact needed"; the data's type strings are the
// keys). "Compact unnecessary" groups are shown, never hidden, with a muted tag: the analysis found the
// supply chains coherent, but a coalition would barely improve on what the demand anchors' existing
// relationships already give them. Pure.

import { fmtPp } from './format.ts';

export type TypeTone = 'compact' | 'bilateral' | 'unnecessary';

export interface TypeInfo {
  /** the tag text */
  short: string;
  /** one line on what the type means */
  explanation: string;
  tone: TypeTone;
}

const UNNECESSARY = 'compact unnecessary: existing relationships do nearly as well';

export function compactTypeInfo(type: string, gain?: number | null): TypeInfo {
  if (type === 'candidate compact') {
    return { short: 'Potential Sectoral Compact', tone: 'compact', explanation: 'Coordination among several countries, not a bilateral deal, is what recovers the supply.' };
  }
  if (type === 'bilateral arrangement sufficient') {
    return { short: 'Bilateral arrangement enough', tone: 'bilateral', explanation: 'Fewer countries than a compact needs take part: a bilateral arrangement would do.' };
  }
  if (type === UNNECESSARY || type.startsWith('compact unnecessary')) {
    const by = gain !== null && gain !== undefined && Number.isFinite(gain) ? ` (${fmtPp(gain)})` : '';
    return {
      short: 'No compact needed',
      tone: 'unnecessary',
      explanation: `Existing relationships do nearly as well: a coalition barely raises the share of the demand anchors' demand met over today's uncoordinated response${by}.`,
    };
  }
  return { short: type, tone: 'compact', explanation: type };
}
