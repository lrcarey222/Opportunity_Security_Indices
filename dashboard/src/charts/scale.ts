// The three-colour magnitude scale (user edit 1a; spec_country.md C1.2): NEIS orange at the bottom,
// white at the median of the values shown, NEIS blue at the top. Pure and unit-tested.
//
// Position, not colour, is computed first: pos(v) in [0, 1] (0 = the orange end, 0.5 = white, 1 = the
// blue end, after `reverse`), then color(v) interpolates in RGB between the two TRI stops around pos x 8
// (the stops are OKLab-spaced, so RGB between neighbours is fine).
//
//   * basis 'rank' (default), center 'median' (default): each arm by midrank fraction, so values that
//     cluster are spread apart (the user's complaint about the single-hue blues); white = the median.
//   * center 'zero': signed values (changes, net balances): negatives orange, positives blue, 0 white;
//     each arm by the rank of |v| among its own sign (the largest |v| reaches the end).
//   * basis 'value': piecewise linear lo -> mid -> hi (or a fixed `domain` for ordinal codes).
//
// Ties share one colour. n = 0: every call returns null. n = 1 or all equal (center median): white.
// The side of the median is never carried by colour alone near the middle (the two steps next to white
// are close): side() gives the words for tooltips and tables, and TriLegend names both ends.

import { INK, PAPER, TRI } from '../theme/palette.ts';

export type TriCenter = 'median' | 'zero';
export type TriBasis = 'rank' | 'value';

export interface TriOptions {
  /** 'median' (default): white = median of the values; 'zero': white = 0 (signed values, changes) */
  center?: TriCenter;
  /** 'rank' (default): position by rank among the values (each arm by midrank fraction); 'value': piecewise linear lo -> mid -> hi */
  basis?: TriBasis;
  /** true: low values blue, high orange (ranks where 1 is best) */
  reverse?: boolean;
  /** fixed [lo, mid, hi] for ordinal codes (CAT ratings 0..4, mid 2); implies basis 'value' */
  domain?: [number, number, number];
}

export interface TriTick {
  value: number;
  pos: number;
}

export interface TriScale {
  /** the count of finite values that define the scale */
  n: number;
  lo: number;
  /** the median (center 'median'), 0 (center 'zero') or domain[1] */
  mid: number;
  hi: number;
  center: TriCenter;
  basis: TriBasis;
  reverse: boolean;
  /** 0 = orange end, 0.5 = white, 1 = blue end (after reverse); null if v is not finite or n = 0 */
  pos(v: number | null | undefined): number | null;
  /** '#rrggbb'; null -> the caller hatches */
  color(v: number | null | undefined): string | null;
  /** 0..8, the nearest TRI stop (heat tables that print text) */
  step(v: number | null | undefined): number | null;
  /** the words for tooltips and tables: below / at / above the median (or zero) */
  side(v: number | null | undefined): 'below' | 'median' | 'above' | null;
  /** lo, q25, mid, q75, hi (rank, median); lo, mid, hi (value, zero); distinct values only */
  ticks: TriTick[];
}

const finite = (v: unknown): v is number => typeof v === 'number' && Number.isFinite(v);

/** The median of sorted values (mean of the two middle values for even n). */
export function medianSorted(xs: readonly number[]): number {
  const n = xs.length;
  if (!n) return NaN;
  const h = n >> 1;
  return n % 2 ? xs[h] : (xs[h - 1] + xs[h]) / 2;
}

/** The p-quantile of sorted values (linear between order statistics, as d3.quantileSorted). */
export function quantileSorted(xs: readonly number[], p: number): number {
  const n = xs.length;
  if (!n) return NaN;
  if (n === 1) return xs[0];
  const i = (n - 1) * p;
  const i0 = Math.floor(i);
  const i1 = Math.min(n - 1, i0 + 1);
  return xs[i0] + (xs[i1] - xs[i0]) * (i - i0);
}

/** Index of the first element >= v (sorted ascending). */
function lowerBound(xs: readonly number[], v: number): number {
  let a = 0;
  let b = xs.length;
  while (a < b) {
    const m = (a + b) >> 1;
    if (xs[m] < v) a = m + 1;
    else b = m;
  }
  return a;
}

/** Index of the first element > v. */
function upperBound(xs: readonly number[], v: number): number {
  let a = 0;
  let b = xs.length;
  while (a < b) {
    const m = (a + b) >> 1;
    if (xs[m] <= v) a = m + 1;
    else b = m;
  }
  return a;
}

/**
 * A rank fraction of sorted values, for v inside or outside the set: `kind` 'mid' gives the midrank
 * fraction (r - 1) / (n - 1) with ties at their average rank; 'max' gives #{x <= v} / n (the zero-centred
 * arms: the smallest |v| is already off white, the largest reaches the end). Values not in the set
 * interpolate linearly between their neighbours' fractions (clamped at the ends).
 */
function rankFraction(xs: readonly number[], kind: 'mid' | 'max'): (v: number) => number {
  const n = xs.length;
  const at = (v: number): number => {
    const a = lowerBound(xs, v);
    const b = upperBound(xs, v);
    if (kind === 'max') return b / n;
    if (n === 1) return 0.5;
    // midrank (1-based) = a + (b - a + 1) / 2
    return (a + (b - a - 1) / 2) / (n - 1);
  };
  return (v: number) => {
    if (!n) return NaN;
    if (v <= xs[0]) return kind === 'max' && v < xs[0] ? (v >= 0 ? (v / xs[0]) * at(xs[0]) : 0) : at(xs[0]);
    if (v >= xs[n - 1]) return at(xs[n - 1]);
    const b = upperBound(xs, v);
    const a = lowerBound(xs, v);
    if (a < b) return at(v); // in the set
    // between xs[a - 1] and xs[a]
    const x0 = xs[a - 1];
    const x1 = xs[a];
    const f0 = at(x0);
    const f1 = at(x1);
    return f0 + ((f1 - f0) * (v - x0)) / (x1 - x0 || 1);
  };
}

function hexRgb(c: string): [number, number, number] {
  const h = c.replace('#', '');
  return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
}

const toHex = (x: number) => Math.round(Math.max(0, Math.min(255, x))).toString(16).padStart(2, '0');

/** The colour at a scale position (0 orange, 0.5 white, 1 blue): RGB between the TRI stops around pos x 8. */
export function triColorAt(pos: number): string {
  const p = Math.max(0, Math.min(1, pos)) * (TRI.length - 1);
  const i = Math.floor(p);
  const t = p - i;
  if (t < 1e-9 || i >= TRI.length - 1) return TRI[Math.min(TRI.length - 1, i)];
  const a = hexRgb(TRI[i]);
  const b = hexRgb(TRI[i + 1]);
  return `#${toHex(a[0] + (b[0] - a[0]) * t)}${toHex(a[1] + (b[1] - a[1]) * t)}${toHex(a[2] + (b[2] - a[2]) * t)}`;
}

const clamp01 = (x: number) => Math.max(0, Math.min(1, x));

/** Build a three-colour scale over the values a chart shows (non-finite values are ignored). */
export function triScale(values: Iterable<number | null | undefined>, opts: TriOptions = {}): TriScale {
  const domain = opts.domain;
  const center: TriCenter = domain ? 'median' : (opts.center ?? 'median');
  const basis: TriBasis = domain ? 'value' : (opts.basis ?? 'rank');
  const reverse = !!opts.reverse;
  const xs: number[] = [];
  for (const v of values) if (finite(v)) xs.push(v);
  xs.sort((a, b) => a - b);
  const n = xs.length;

  let lo: number;
  let mid: number;
  let hi: number;
  if (domain) [lo, mid, hi] = domain;
  else if (center === 'zero') {
    lo = n ? Math.min(0, xs[0]) : 0;
    hi = n ? Math.max(0, xs[n - 1]) : 0;
    mid = 0;
  } else {
    lo = n ? xs[0] : NaN;
    hi = n ? xs[n - 1] : NaN;
    mid = medianSorted(xs);
  }

  // the raw (unreversed) position
  let raw: (v: number) => number;
  if (!n && !domain) raw = () => NaN;
  else if (basis === 'value') {
    raw = (v) => {
      if (v === mid) return 0.5;
      if (v < mid) return mid > lo ? 0.5 * clamp01((v - lo) / (mid - lo)) : 0;
      return hi > mid ? 0.5 + 0.5 * clamp01((v - mid) / (hi - mid)) : 1;
    };
  } else if (center === 'zero') {
    const neg = xs.filter((x) => x < 0).map((x) => -x).sort((a, b) => a - b);
    const pos = xs.filter((x) => x > 0);
    const fn = neg.length ? rankFraction(neg, 'max') : null;
    const fp = pos.length ? rankFraction(pos, 'max') : null;
    raw = (v) => {
      if (v === 0) return 0.5;
      if (v < 0) return fn ? 0.5 - 0.5 * clamp01(fn(-v)) : 0;
      return fp ? 0.5 + 0.5 * clamp01(fp(v)) : 1;
    };
  } else {
    const F = rankFraction(xs, 'mid');
    const fLo = F(lo);
    const fMid = F(mid);
    const fHi = F(hi);
    raw = (v) => {
      if (n === 1 || lo === hi) return 0.5;
      const f = F(v);
      if (v <= mid) return fMid > fLo ? 0.5 * clamp01((f - fLo) / (fMid - fLo)) : 0.5;
      return fHi > fMid ? 0.5 + 0.5 * clamp01((f - fMid) / (fHi - fMid)) : 0.5;
    };
  }

  const pos = (v: number | null | undefined): number | null => {
    if (!finite(v) || (!n && !domain)) return null;
    const p = raw(v);
    if (!Number.isFinite(p)) return null;
    return reverse ? 1 - p : p;
  };
  const color = (v: number | null | undefined): string | null => {
    const p = pos(v);
    return p === null ? null : triColorAt(p);
  };
  const step = (v: number | null | undefined): number | null => {
    const p = pos(v);
    return p === null ? null : Math.round(p * (TRI.length - 1));
  };
  const side = (v: number | null | undefined): 'below' | 'median' | 'above' | null => {
    if (!finite(v) || (!n && !domain) || !Number.isFinite(mid)) return null;
    const eps = 1e-12 * Math.max(1, Math.abs(mid));
    return v < mid - eps ? 'below' : v > mid + eps ? 'above' : 'median';
  };

  const tickValues: number[] = [];
  if (n || domain) {
    if (basis === 'rank' && center === 'median') tickValues.push(lo, quantileSorted(xs, 0.25), mid, quantileSorted(xs, 0.75), hi);
    else tickValues.push(lo, mid, hi);
  }
  const ticks: TriTick[] = [];
  for (const t of tickValues) {
    if (!Number.isFinite(t) || ticks.some((k) => Math.abs(k.value - t) <= 1e-12 * Math.max(1, Math.abs(t)))) continue;
    const p = pos(t);
    if (p !== null) ticks.push({ value: t, pos: p });
  }
  ticks.sort((a, b) => a.pos - b.pos);

  return { n, lo, mid, hi, center, basis, reverse, pos, color, step, side, ticks };
}

// ---- text on a fill (moved here from sector/charts/color.ts textOn; that one re-exports it) --------

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
 * ink-900 or white, whichever reads better on `bg` (labels inside bars, tiles and heat cells); black
 * when neither reaches 4.5:1 (a mid-tone such as the blue end #1889b0: ink-900 4.42, white 4.0, black
 * 5.2). On every TRI stop this gives ink or black: white text never sits on orange.
 */
export function triText(bg: string): string {
  const ink = contrast(INK[900], bg);
  const white = contrast(PAPER, bg);
  if (Math.max(ink, white) < 4.5 && contrast('#000000', bg) > Math.max(ink, white)) return '#000000';
  return ink >= white ? INK[900] : PAPER;
}
