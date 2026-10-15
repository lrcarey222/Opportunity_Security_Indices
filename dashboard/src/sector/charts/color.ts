// Colour helpers of the Sector chart kit (spec_sector.md 3.4, 7): the fixed split order, group fills
// for SVG (Plot) and CSS (HTML marks), and text colour chosen by contrast (WCAG relative luminance).

import type { CSSProperties } from 'react';
import { GROUP_COLOR, GROUP_LABEL, HATCH_CSS, HATCH_STYLE, NEUTRAL_SERIES, SEQ, UNATTRIBUTED_HATCH_ID } from '../../theme/palette.ts';
import type { GroupKey } from './types.ts';

/** The fixed stacking order of the share split (never reordered by value). */
export const SPLIT_ORDER: readonly GroupKey[] = ['china', 'core', 'partner', 'rest', 'unattributed'];

export const HATCH_URL = `url(#${UNATTRIBUTED_HATCH_ID})`;

/** The SVG fill of a group (the residual is the hatch pattern of SvgDefs). */
export function groupFill(g: GroupKey | 'neutral'): string {
  if (g === 'unattributed') return HATCH_URL;
  if (g === 'neutral') return NEUTRAL_SERIES;
  return GROUP_COLOR[g];
}

/** A solid colour for a group (the residual's hatch ground for strokes and dots). */
export function groupColor(g: GroupKey | 'neutral'): string {
  if (g === 'unattributed') return '#9ca3af';
  if (g === 'neutral') return NEUTRAL_SERIES;
  return GROUP_COLOR[g];
}

/** The CSS background of a group (HTML marks). */
export function groupCss(g: GroupKey | 'neutral'): string {
  return g === 'unattributed' ? HATCH_CSS : groupColor(g);
}

/** The CSS background of a group as longhand properties (the hatch as image over ground). */
export function groupBg(g: GroupKey | 'neutral'): CSSProperties {
  return g === 'unattributed' ? { ...HATCH_STYLE } : { backgroundColor: groupColor(g) };
}

export function groupLabel(g: GroupKey): string {
  return GROUP_LABEL[g];
}

// Contrast helpers moved to charts/scale.ts (spec_country.md C1.2); re-exported under the old names.
export { contrast, parseColor, relLuminance } from '../../charts/scale.ts';

/**
 * ink-900 or white, whichever reads better on `bg` (labels inside bars and tiles); black when neither
 * reaches 4.5:1. The same function as triText (charts/scale.ts).
 */
export { triText as textOn } from '../../charts/scale.ts';

/** The sequential step (SEQ) of a value in [lo, hi] (clamped; reverse for "smaller = more").
 *  @deprecated Superseded by the three-colour scale (charts/scale.ts triScale, spec_country.md C1); views may not import it (charts/noLegacyRamps.test.ts). */
export function seqStep(v: number, lo = 0, hi = 1, reverse = false): string {
  const n = SEQ.length;
  const t = hi > lo ? Math.min(1, Math.max(0, (v - lo) / (hi - lo))) : 0;
  const i = Math.min(n - 1, Math.floor(t * n));
  return SEQ[reverse ? n - 1 - i : i];
}
