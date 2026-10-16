// The compact types in words: a short tag, a one-line explanation and a tone. "Compact unnecessary"
// groups are shown, never hidden, with a muted tag: the model found the supply chains coherent, but a
// coalition would barely improve on what the demand anchors' existing relationships already give them.
// Pure.

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
    return { short: 'Candidate compact', tone: 'compact', explanation: 'Coordination among enough economies, not a bilateral deal, is what restores the supply.' };
  }
  if (type === 'bilateral arrangement sufficient') {
    return { short: 'Bilateral arrangement sufficient', tone: 'bilateral', explanation: 'Fewer economies than a compact needs take part: a bilateral arrangement would do.' };
  }
  if (type === UNNECESSARY || type.startsWith('compact unnecessary')) {
    const by = gain !== null && gain !== undefined && Number.isFinite(gain) ? ` (${fmtPp(gain)})` : '';
    return {
      short: 'Compact unnecessary',
      tone: 'unnecessary',
      explanation: `Existing relationships do nearly as well: the coalition barely improves the demand anchors' coverage over fragmented action${by}.`,
    };
  }
  return { short: type, tone: 'compact', explanation: type };
}
