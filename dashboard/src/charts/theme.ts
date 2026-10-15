// Shared Observable Plot defaults (spec_react.md 8.7): transparent background, Roboto 12 px, text
// #d1d5db, axis labels and ticks #9ca3af, grid rgb(255 255 255 / 0.08), zero rule 0.22; no titles
// inside the SVG (titles live in the card); tooltips are HTML (PlotFigure).

import type { PlotOptions } from '@observablehq/plot';
import { DIV_RANGE, FONT, PAPER_MUTE, SEQ } from '../theme/palette.ts';

export const AXIS_COLOR = PAPER_MUTE;

export function plotBase(width: number, height: number): PlotOptions {
  return {
    width,
    height,
    style: {
      background: 'transparent',
      color: AXIS_COLOR,
      fontFamily: FONT,
      fontSize: '12px',
      overflow: 'visible',
    },
    marginTop: 16,
    marginRight: 16,
    marginBottom: 36,
    marginLeft: 48,
  };
}

/** Sequential scale (low dim -> high bright); reverse for ranks (rank 1 brightest).
 *  @deprecated Superseded by the three-colour scale (charts/scale.ts triScale, spec_country.md C1); views may not import it (charts/noLegacyRamps.test.ts). */
export function seqScale(domain: [number, number], reverse = false) {
  return { type: 'linear' as const, domain, range: reverse ? [...SEQ].reverse() : SEQ, interpolate: 'rgb' as const, clamp: true };
}

/** Diverging scale symmetric around zero (decrease red, increase blue, neutral gray midpoint).
 *  @deprecated Superseded by the three-colour scale (charts/scale.ts triScale, spec_country.md C1); views may not import it (charts/noLegacyRamps.test.ts). */
export function divScale(limit: number) {
  const L = limit > 0 && Number.isFinite(limit) ? limit : 1;
  const n = DIV_RANGE.length;
  const domain = DIV_RANGE.map((_, i) => -L + (2 * L * i) / (n - 1));
  return { type: 'linear' as const, domain, range: DIV_RANGE, interpolate: 'rgb' as const, clamp: true };
}

/** A d3-style colour function for SVG charts drawn in React (maps). */
export function colorFn(domain: number[], range: string[]): (v: number) => string {
  return (v: number) => {
    if (!Number.isFinite(v)) return '#374151';
    if (v <= domain[0]) return range[0];
    const last = domain.length - 1;
    if (v >= domain[last]) return range[last];
    let i = 1;
    while (i < last && v > domain[i]) i++;
    const t = (v - domain[i - 1]) / (domain[i] - domain[i - 1] || 1);
    return mix(range[i - 1], range[i], t);
  };
}

/** @deprecated Superseded by the three-colour scale (charts/scale.ts triScale, spec_country.md C1); views may not import it (charts/noLegacyRamps.test.ts). */
export function seqColor(lo: number, hi: number, reverse = false): (v: number) => string {
  const range = reverse ? [...SEQ].reverse() : SEQ;
  const n = range.length;
  const span = hi - lo || 1;
  return colorFn(range.map((_, i) => lo + (span * i) / (n - 1)), range);
}

/** @deprecated Superseded by the three-colour scale (charts/scale.ts triScale, spec_country.md C1); views may not import it (charts/noLegacyRamps.test.ts). */
export function divColor(limit: number): (v: number) => string {
  const s = divScale(limit);
  return colorFn(s.domain, s.range);
}

function hex(c: string): [number, number, number] {
  const h = c.replace('#', '');
  return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
}

function mix(a: string, b: string, t: number): string {
  const x = hex(a);
  const y = hex(b);
  const c = x.map((v, i) => Math.round(v + (y[i] - v) * t));
  return `rgb(${c[0]} ${c[1]} ${c[2]})`;
}

/** Largest |x| of finite values (for symmetric diverging scales). */
export function absMax(xs: Iterable<number | null | undefined>): number {
  let m = 0;
  for (const x of xs) if (typeof x === 'number' && Number.isFinite(x) && Math.abs(x) > m) m = Math.abs(x);
  return m;
}

/** Approximate rendered width of a label in Roboto (average glyph ~0.56 em). */
export function textWidth(s: string, fontSize = 12): number {
  return Math.ceil(s.length * fontSize * 0.56);
}

/** A left (or bottom) margin wide enough for the longest label, within [min, max]. */
export function labelMargin(labels: Iterable<string>, { fontSize = 12, min = 40, max = 260, pad = 14 } = {}): number {
  let w = 0;
  for (const l of labels) w = Math.max(w, textWidth(l, fontSize));
  return Math.max(min, Math.min(max, w + pad));
}

/** Truncate a label to fit a pixel width (an ellipsis marks the cut; the full text stays in tooltips). */
export function fitLabel(s: string, px: number, fontSize = 12): string {
  const n = Math.floor(px / (fontSize * 0.56));
  return s.length <= n ? s : `${s.slice(0, Math.max(1, n - 1))}…`;
}

/** Symmetric domain around zero with a floor on the half-width (a flat change still gets an axis). */
export function symDomain(values: Iterable<number | null | undefined>, floor = 0.01): [number, number] {
  const L = Math.max(floor, absMax(values));
  return [-L, L];
}

/**
 * Greedy label placement: returns the indices of the points whose labels can be drawn without
 * overlapping an already placed label (in priority order). Positions and sizes are in pixels.
 */
export function declutter(boxes: { x: number; y: number; w: number; h: number }[], order?: number[]): Set<number> {
  const placed: { x0: number; x1: number; y0: number; y1: number }[] = [];
  const keep = new Set<number>();
  for (const i of order ?? boxes.map((_, k) => k)) {
    const b = boxes[i];
    const r = { x0: b.x - b.w / 2, x1: b.x + b.w / 2, y0: b.y - b.h / 2, y1: b.y + b.h / 2 };
    if (placed.some((p) => r.x0 < p.x1 && r.x1 > p.x0 && r.y0 < p.y1 && r.y1 > p.y0)) continue;
    placed.push(r);
    keep.add(i);
  }
  return keep;
}

/**
 * Tick values of a log axis: the powers of ten inside [lo, hi], thinned to at most `maxTicks` (every
 * k-th decade); when the range spans less than two decades, the 1-2-5 steps. Passed as `ticks`, so the
 * grid is drawn at these values only (no label on every minor tick).
 */
export function logTicks(lo: number, hi: number, maxTicks = 6): number[] {
  if (!(lo > 0) || !(hi > 0) || !(hi >= lo)) return [];
  const a = Math.floor(Math.log10(lo));
  const b = Math.ceil(Math.log10(hi));
  const inside = (t: number) => t >= lo * (1 - 1e-9) && t <= hi * (1 + 1e-9);
  const p10 = (e: number) => Number(Math.pow(10, e).toPrecision(12));
  const dec: number[] = [];
  for (let e = a; e <= b; e++) if (inside(p10(e))) dec.push(p10(e));
  const thin = (xs: number[]) => {
    const step = Math.max(1, Math.ceil(xs.length / Math.max(2, maxTicks)));
    return xs.filter((_, i) => i % step === 0);
  };
  if (dec.length >= 2) return thin(dec);
  const steps: number[] = [];
  for (let e = a; e <= b; e++) for (const m of [1, 2, 5]) if (inside(Number((m * Math.pow(10, e)).toPrecision(12)))) steps.push(Number((m * Math.pow(10, e)).toPrecision(12)));
  return thin(steps);
}

/** A compact tick label with its own SI step (log axes): 3 -> "3", 2000 -> "2k", 1e7 -> "10m", 1e10 -> "10bn". */
export function compactSi(x: number): string {
  if (!Number.isFinite(x)) return '';
  const a = Math.abs(x);
  const steps: [number, string][] = [
    [1e12, 'tn'],
    [1e9, 'bn'],
    [1e6, 'm'],
    [1e3, 'k'],
  ];
  for (const [v, w] of steps) if (a >= v) return `${Number((x / v).toPrecision(3))}${w}`;
  return String(Number(x.toPrecision(3)));
}
