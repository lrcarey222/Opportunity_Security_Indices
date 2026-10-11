// Colour helpers of the Sector chart kit (spec_sector.md 3.4, 7): the fixed split order, group fills
// for SVG (Plot) and CSS (HTML marks), and text colour chosen by contrast (WCAG relative luminance).

import type { CSSProperties } from 'react';
import { GROUP_COLOR, GROUP_LABEL, HATCH_CSS, HATCH_STYLE, INK, NEUTRAL_SERIES, PAPER, SEQ, UNATTRIBUTED_HATCH_ID } from '../../theme/palette.ts';
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

function channel(c: number): number {
  const s = c / 255;
  return s <= 0.03928 ? s / 12.92 : ((s + 0.055) / 1.055) ** 2.4;
}

/** Parses #rgb, #rrggbb and rgb(r g b) / rgb(r, g, b). */
export function parseColor(c: string): [number, number, number] {
  const m = /^rgba?\(\s*(\d+)[\s,]+(\d+)[\s,]+(\d+)/i.exec(c);
  if (m) return [Number(m[1]), Number(m[2]), Number(m[3])];
  let h = c.replace('#', '');
  if (h.length === 3) h = h.split('').map((x) => x + x).join('');
  return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
}

export function relLuminance(c: string): number {
  const [r, g, b] = parseColor(c);
  return 0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b);
}

/** The WCAG contrast ratio of two colours. */
export function contrast(a: string, b: string): number {
  const la = relLuminance(a);
  const lb = relLuminance(b);
  return (Math.max(la, lb) + 0.05) / (Math.min(la, lb) + 0.05);
}

/**
 * ink-900 or white, whichever reads better on `bg` (labels inside bars and tiles); black when neither
 * reaches 4.5:1 (a mid-tone such as the coalition-core blue #1889b0: ink-900 4.42, white 4.0, black 5.2).
 */
export function textOn(bg: string): string {
  const ink = contrast(INK[900], bg);
  const white = contrast(PAPER, bg);
  if (Math.max(ink, white) < 4.5 && contrast('#000000', bg) > Math.max(ink, white)) return '#000000';
  return ink >= white ? INK[900] : PAPER;
}

/** The sequential step (SEQ) of a value in [lo, hi] (clamped; reverse for "smaller = more"). */
export function seqStep(v: number, lo = 0, hi = 1, reverse = false): string {
  const n = SEQ.length;
  const t = hi > lo ? Math.min(1, Math.max(0, (v - lo) / (hi - lo))) : 0;
  const i = Math.min(n - 1, Math.floor(t * n));
  return SEQ[reverse ? n - 1 - i : i];
}
