// Selection and hover rings on three-colour visuals (spec_country.md C1.4). Orange is the low end of the
// three-colour scale, so an orange outline would vanish there: on tri visuals the selected mark gets a
// 4 px ink (#111827) stroke under a 2 px white stroke, visible on orange, white, blue and the dark
// ground; hover is the same at 3 px / 1 px. Focus rings of controls stay orange (chrome).

import * as Plot from '@observablehq/plot';
import { RING_INNER, RING_OUTER } from '../theme/palette.ts';

export type RingKind = 'selected' | 'hover';
export type RingShape = 'dot' | 'cell' | 'rect';

/** The stroke widths of the double ring: [outer ink, inner white]. */
export const RING_WIDTHS: Record<RingKind, readonly [number, number]> = { selected: [4, 2], hover: [3, 1] };

/**
 * The two Plot marks of a double ring over `data` with the caller's position channels (x, y, r, and
 * for cells / rects their extents). 'hover' wraps both in Plot.pointer, so they follow the pointer and
 * feed the figure's value like any pointer mark. Draw them after the filled marks.
 */
export function ringMarks(data: Plot.Data, channels: Record<string, unknown>, kind: RingKind = 'selected', shape: RingShape = 'dot'): Plot.Markish[] {
  const [outer, inner] = RING_WIDTHS[kind];
  const one = (stroke: string, strokeWidth: number) => {
    const opts = { ...channels, fill: 'none', stroke, strokeWidth } as Record<string, unknown>;
    const o = kind === 'hover' ? (Plot.pointer(opts as Plot.PointerOptions & Record<string, unknown>) as Record<string, unknown>) : opts;
    if (shape === 'cell') return Plot.cell(data, o as Plot.CellOptions);
    if (shape === 'rect') return Plot.rect(data, o as Plot.RectOptions);
    return Plot.dot(data, o as Plot.DotOptions);
  };
  return [one(RING_OUTER, outer), one(RING_INNER, inner)];
}

/** The SVG stroke layers of a ring around a shape drawn in React (maps): outer first. */
export function ringLayers(style: 'orange' | 'double', kind: RingKind, orange: string): { stroke: string; width: number }[] {
  if (style === 'orange') return [{ stroke: orange, width: 1.5 }];
  const [outer, inner] = RING_WIDTHS[kind];
  return [
    { stroke: RING_OUTER, width: outer },
    { stroke: RING_INNER, width: inner },
  ];
}
