// Tile colours on the three-colour scale (user edit 1a; spec_country.md C1.5): a tile's background is
// its value's colour on a TriScale built from the tiles shown (orange lower, white the median, blue
// higher), its text ink-900, white or black, whichever reads at >= 4.5:1 (palette triText; unit-
// tested). Tiles also carry the side of the median in words, because the two steps next to white are
// close in colour. Pure.

import { contrast as wcag, triScale, triText, type TriOptions, type TriScale } from '../../charts/scale.ts';

export { wcag as contrast };

/** The text colour (ink-900, white or black) with the better contrast on a background. */
export const textOn = triText;

export interface TriTile {
  bg: string;
  fg: string;
  /** 0..8, the nearest TRI stop */
  step: number;
  side: 'below' | 'median' | 'above';
}

/** Background, text colour, step and side of a tile value on a three-colour scale, or null (no colour: hatch). */
export function triTile(s: TriScale, v: number | null | undefined): TriTile | null {
  const bg = s.color(v);
  const step = s.step(v);
  const side = s.side(v);
  if (!bg || step === null || side === null) return null;
  return { bg, fg: triText(bg), step, side };
}

/** A three-colour scale over the finite values of a set of tiles (the values that define it are the tiles shown). */
export function tileScale(values: Iterable<number | null | undefined>, opts?: TriOptions): TriScale {
  return triScale(values, opts);
}

/** "above the median of the 66 tiles shown" (tooltips and aria-labels say the side in words). */
export function sideWords(side: TriTile['side'] | null, n: number, noun = 'tiles'): string {
  if (!side) return '';
  return side === 'median' ? `at the median of the ${n} ${noun} shown` : `${side} the median of the ${n} ${noun} shown`;
}
