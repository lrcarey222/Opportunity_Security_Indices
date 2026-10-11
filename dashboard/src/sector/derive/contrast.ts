// Atlas tile text colour per sequential step (spec_sector.md 4.1, 7): ink-900 on the light steps,
// white on the dark ones, chosen by WCAG contrast so every step reaches 4.5:1 (unit-tested). Pure.

import { INK, PAPER, SEQ } from '../../theme/palette.ts';

function channel(c: number): number {
  const s = c / 255;
  return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4);
}

function rgb(color: string): [number, number, number] {
  const m = /^rgb\((\d+)\s+(\d+)\s+(\d+)\)$/.exec(color);
  if (m) return [Number(m[1]), Number(m[2]), Number(m[3])];
  const h = color.replace('#', '');
  return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
}

export function luminance(color: string): number {
  const [r, g, b] = rgb(color);
  return 0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b);
}

export function contrast(a: string, b: string): number {
  const la = luminance(a);
  const lb = luminance(b);
  return (Math.max(la, lb) + 0.05) / (Math.min(la, lb) + 0.05);
}

/** The text colour (ink-900 or white) with the higher contrast on a background. */
export function textOn(bg: string): string {
  return contrast(bg, INK[900]) >= contrast(bg, PAPER) ? INK[900] : PAPER;
}

/** The SEQ step (0..6) of a value in [lo, hi] (tiles use discrete steps so the text colour is tested). */
export function seqStep(v: number, lo = 0, hi = 1): number {
  if (!Number.isFinite(v)) return -1;
  const t = hi > lo ? (v - lo) / (hi - lo) : 0;
  return Math.max(0, Math.min(SEQ.length - 1, Math.floor(t * SEQ.length - 1e-12)));
}

/** Background and text colour of a lens value on SEQ (reverse for effective n). */
export function seqTile(v: number, lo = 0, hi = 1, reverse = false): { bg: string; fg: string; step: number } | null {
  const s = seqStep(v, lo, hi);
  if (s < 0) return null;
  const step = reverse ? SEQ.length - 1 - s : s;
  const bg = SEQ[step];
  return { bg, fg: textOn(bg), step };
}
