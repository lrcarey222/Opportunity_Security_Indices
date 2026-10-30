// The quadrant scatter's shared core (2026-10-30): the look of the Country page's "Security and opportunity,
// sector by sector" chart (country/charts/QuadrantScatter.tsx, the Legacy OSI block), taken out so the Prize x
// Position scatter (posture/PrizePositionScatter.tsx) is the same chart, not a look-alike. Observable Plot
// draws it through PlotFigure:
// - a 0-100 frame on both axes, square-ish (scatterHeight), ticks at 0 / 25 / 50 / 75 / 100 with the grid,
//   the axis labels at the ends ("... →" across, "↑ ..." up);
// - two dashed rules (PAPER_MUTE, 4,3), where the caller splits its corners;
// - one dot per point, radius 6 (5 above 33 points), a 1.5 px outline, its shape the point's stage
//   (STAGE_SYMBOL: one of six, never a colour); fill and stroke from the caller;
// - point labels ("Batteries · Ref"): the selected point always (bold white, ringed: signal orange by
//   default, `ring` for a chart whose dots wear the choice colours, where orange is Secure), then the
//   caller's priority points, then the rest, each above its dot, else below it, only where it covers neither
//   an earlier label nor another point's dot and stays inside the figure (placeQuadLabels; below 640 px the
//   rest go unlabelled);
// - the four corner headings in the margins above and below the plot (QuadCornerRow), so no dot hides them.
// Plot's own pointer ring is optional (`pointer`): the Legacy chart uses it with PlotFigure's tooltip; the
// Prize x Position chart draws focusable points over the figure instead (quadLeft / quadTop place them).

import type { ReactNode } from 'react';
import * as Plot from '@observablehq/plot';
import { symbol as d3Symbol, symbolCircle, symbolCross, symbolDiamond, symbolSquare, symbolStar, symbolTriangle, type SymbolType } from 'd3';
import { SUPPLY_STAGES, type SupplyStage } from '../lib/stages.ts';
import { PAPER_MUTE, SIGNAL } from '../theme/palette.ts';
import { textWidth } from './theme.ts';

/** The light neutral of the Legacy chart's dots and labels. */
export const QUAD_MARK = '#d1d5db';
/** The plot margins (the corner headings are padded to the same left and right). */
// top 24: the y axis's label sits at the very top, clear of the "100" tick
export const QUAD_MARGIN = { top: 24, right: 20, bottom: 46, left: 52 } as const;
export const QUAD_TICKS = [0, 25, 50, 75, 100];
/** The dash of the threshold (or median) rules. */
export const QUAD_DASH = '4,3';

/** The dot shape of each of the six stages (Plot's fillable symbols, so a dot can be filled or hollow). */
export const STAGE_SYMBOL: Readonly<Record<SupplyStage, 'circle' | 'diamond' | 'square' | 'cross' | 'triangle' | 'star'>> = {
  Extraction: 'circle',
  Refining: 'diamond',
  Components: 'square',
  Assembly: 'cross',
  Deployment: 'triangle',
  Recovery: 'star',
};
const D3_SYMBOL: Readonly<Record<(typeof STAGE_SYMBOL)[SupplyStage], SymbolType>> = {
  circle: symbolCircle,
  diamond: symbolDiamond,
  square: symbolSquare,
  cross: symbolCross,
  triangle: symbolTriangle,
  star: symbolStar,
};

/** The dot radius: 6 px, 5 px when more than 33 points are placed (the six-stage sheets place up to 58). */
export const scatterRadius = (n: number): number => (n > 33 ? 5 : 6);
/** The scatter's height for a width: square-ish, clamped to 320-560 px. */
export const scatterHeight = (w: number): number => Math.max(320, Math.min(560, Math.round(w)));

/** A point's left edge in CSS, over a figure of any width (x on 0-100): the plot area between the margins. */
export const quadLeft = (x: number): string => `calc(${QUAD_MARGIN.left}px + (100% - ${QUAD_MARGIN.left + QUAD_MARGIN.right}px) * ${x / 100})`;
/** The same distance from the right edge (a tooltip that flips to the point's left). */
export const quadRight = (x: number): string => `calc(${QUAD_MARGIN.right}px + (100% - ${QUAD_MARGIN.left + QUAD_MARGIN.right}px) * ${1 - x / 100})`;
/** A point's top in px for a figure height (y on 0-100). */
export const quadTop = (y: number, height: number): number => QUAD_MARGIN.top + (1 - y / 100) * (height - QUAD_MARGIN.top - QUAD_MARGIN.bottom);

/** One point: a key, 0-100 coordinates, its short label and its stage (the shape). */
export interface QuadDatum {
  key: string;
  x: number;
  y: number;
  short: string;
  stage: string;
}

export interface QuadScatterSpec<T extends QuadDatum> {
  width: number;
  height: number;
  /** in drawing order (the last on top) */
  points: T[];
  selected: string | null;
  /** labelled before the rest (and below 640 px, the only ones besides the selected point) */
  priority: (d: T) => boolean;
  xLabel: string;
  yLabel: string;
  /** the dashed rules, on the 0-100 axes (null: none) */
  ruleX: number | null;
  ruleY: number | null;
  fill: (d: T) => string;
  stroke: (d: T) => string;
  /** the stages the shape scale knows (default all six) */
  stages?: readonly SupplyStage[];
  /** Plot's pointer ring, for PlotFigure's tooltip and click (default on) */
  pointer?: boolean;
  /** the selection ring's colour (default signal orange) */
  ring?: string;
}

/** A label's or a dot's box in pixels. */
interface Box {
  x0: number;
  x1: number;
  y0: number;
  y1: number;
}
const boxAt = (cx: number, cy: number, w: number, h: number): Box => ({ x0: cx - w / 2, x1: cx + w / 2, y0: cy - h / 2, y1: cy + h / 2 });
const hits = (a: Box, b: Box) => a.x0 < b.x1 && a.x1 > b.x0 && a.y0 < b.y1 && a.y1 > b.y0;

/** The label offsets (Plot text dy): above the dot, else below it; the selected label sits a little further out (its ring). */
export const QUAD_LABEL_DY = { above: -13, below: 13, selectedAbove: -16, selectedBelow: 16 } as const;

/**
 * Where each label goes (greedy, in the candidates' order): above its dot, else below it, where the label's
 * box covers no label already placed and no other point's dot (a box of the dot's radius and its outline) and
 * stays inside the figure; a candidate with no free place goes unlabelled. The selected point is always
 * labelled (above when nothing frees a place). Positions are in pixels.
 */
export function placeQuadLabels<T extends QuadDatum>(
  cands: readonly T[],
  points: readonly T[],
  at: { px: (x: number) => number; py: (y: number) => number; r: number; width: number; height: number; left: number },
  selected: string | null,
): { above: T[]; below: T[] } {
  const dots = points.map((p) => ({ key: p.key, b: boxAt(at.px(p.x), at.py(p.y), 2 * (at.r + 2), 2 * (at.r + 2)) }));
  const placed: Box[] = [];
  const out = { above: [] as T[], below: [] as T[] };
  for (const p of cands) {
    const sel = p.key === selected;
    const w = textWidth(p.short, sel ? 11 : 10) + 4;
    const tries = [
      { to: 'above' as const, dy: sel ? QUAD_LABEL_DY.selectedAbove : QUAD_LABEL_DY.above },
      { to: 'below' as const, dy: sel ? QUAD_LABEL_DY.selectedBelow : QUAD_LABEL_DY.below },
    ];
    let done = false;
    for (const t of tries) {
      const b = boxAt(at.px(p.x), at.py(p.y) + t.dy, w, 12);
      if (b.x0 < at.left - 6 || b.x1 > at.width - 2 || b.y0 < 0 || b.y1 > at.height) continue;
      if (placed.some((q) => hits(b, q)) || dots.some((d) => d.key !== p.key && hits(b, d.b))) continue;
      placed.push(b);
      out[t.to].push(p);
      done = true;
      break;
    }
    if (!done && sel) {
      placed.push(boxAt(at.px(p.x), at.py(p.y) + QUAD_LABEL_DY.selectedAbove, w, 12));
      out.above.push(p);
    }
  }
  return out;
}

/** The Plot options of a quadrant scatter (null: no point). */
export function quadScatterOptions<T extends QuadDatum>(s: QuadScatterSpec<T>): Plot.PlotOptions | null {
  const { width, height: H, points, selected } = s;
  if (!points.length) return null;
  const M = QUAD_MARGIN;
  const narrow = width < 640;
  const iw = width - M.left - M.right;
  const ih = H - M.top - M.bottom;
  const px = (x: number) => M.left + (x / 100) * iw;
  const py = (y: number) => M.top + (1 - y / 100) * ih;
  const r = scatterRadius(points.length);
  // point labels: the selected point first (always), then the priority points, then the rest, each where it
  // fits clear of the other labels and dots (with up to 58 dots, labels that always print would pile up)
  const sel = points.filter((p) => p.key === selected);
  const must = points.filter((p) => p.key !== selected && s.priority(p));
  const rest = points.filter((p) => p.key !== selected && !s.priority(p));
  const cands = [...sel, ...must, ...(narrow ? [] : rest)];
  const lab = placeQuadLabels(cands, points, { px, py, r, width, height: H, left: M.left }, selected);
  const notSel = (xs: T[]) => xs.filter((p) => p.key !== selected);
  const selBelow = lab.below.some((p) => p.key === selected);
  const ring = s.ring ?? SIGNAL;
  const stages = s.stages ?? SUPPLY_STAGES;
  return {
    width,
    height: H,
    marginTop: M.top,
    marginRight: M.right,
    marginBottom: M.bottom,
    marginLeft: M.left,
    x: { domain: [0, 100], ticks: QUAD_TICKS, label: s.xLabel, labelAnchor: 'right', grid: true },
    y: { domain: [0, 100], ticks: QUAD_TICKS, label: s.yLabel, labelAnchor: 'top', grid: true },
    symbol: { domain: [...stages], range: stages.map((x) => STAGE_SYMBOL[x]) },
    marks: [
      Plot.frame({ stroke: 'rgb(255 255 255 / 0.12)' }),
      ...(s.ruleX !== null ? [Plot.ruleX([s.ruleX], { stroke: PAPER_MUTE, strokeWidth: 1, strokeDasharray: QUAD_DASH, className: 'quad-rule-x' })] : []),
      ...(s.ruleY !== null ? [Plot.ruleY([s.ruleY], { stroke: PAPER_MUTE, strokeWidth: 1, strokeDasharray: QUAD_DASH, className: 'quad-rule-y' })] : []),
      Plot.dot(points, { x: 'x', y: 'y', symbol: 'stage', r, fill: s.fill, stroke: s.stroke, strokeWidth: 1.5, className: 'quad-dots' }),
      Plot.text(notSel(lab.above), { x: 'x', y: 'y', text: 'short', dy: QUAD_LABEL_DY.above, fill: QUAD_MARK, fontSize: 10, className: 'quad-labels' }),
      Plot.text(notSel(lab.below), { x: 'x', y: 'y', text: 'short', dy: QUAD_LABEL_DY.below, fill: QUAD_MARK, fontSize: 10, className: 'quad-labels quad-labels-below' }),
      Plot.dot(sel, { x: 'x', y: 'y', symbol: 'stage', r: r + 3, fill: 'none', stroke: ring, strokeWidth: 2.5, className: 'quad-selected' }),
      Plot.text(sel, { x: 'x', y: 'y', text: 'short', dy: selBelow ? QUAD_LABEL_DY.selectedBelow : QUAD_LABEL_DY.selectedAbove, fill: '#ffffff', fontSize: 11, fontWeight: 600 }),
      ...(s.pointer === false ? [] : [Plot.dot(points, Plot.pointer({ x: 'x', y: 'y', r: 10, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 16 }))]),
    ],
  };
}

/** One corner heading: its name in capitals, its meaning beneath. */
export function QuadCorner({ title, children, right = false }: { title: ReactNode; children?: ReactNode; right?: boolean }) {
  return (
    <div className={right ? 'text-right' : ''}>
      <span className="block text-[11px] font-semibold uppercase tracking-wider text-paper">{title}</span>
      {children !== undefined && <span className="block text-[11px] leading-snug text-paper-mute">{children}</span>}
    </div>
  );
}

/** Two corner headings in the margin above (or below) the plot, padded to the plot area. */
export function QuadCornerRow({ left, right, below = false, testId }: { left: ReactNode; right: ReactNode; below?: boolean; testId?: string }) {
  return (
    <div
      className={`grid grid-cols-2 gap-4 ${below ? 'pt-1' : 'pb-1'}`}
      style={{ paddingLeft: QUAD_MARGIN.left, paddingRight: QUAD_MARGIN.right }}
      aria-hidden="true"
      data-testid={testId}
    >
      {left}
      {right}
    </div>
  );
}

/** A stage shape at legend size (r = 5), as Plot draws it (d3 symbols). */
export function StageSymbol({ shape, fill = 'none', stroke = QUAD_MARK }: { shape: (typeof STAGE_SYMBOL)[SupplyStage]; fill?: string; stroke?: string }) {
  const d = d3Symbol(D3_SYMBOL[shape], Math.PI * 25)() ?? '';
  return (
    <svg width="14" height="14" viewBox="-7 -7 14 14" aria-hidden="true" className="inline-block">
      <path d={d} fill={fill} stroke={stroke} strokeWidth="1.5" />
    </svg>
  );
}

/** The dashed rule at legend size. */
export function DashedRuleSwatch() {
  return (
    <svg width="20" height="4" aria-hidden="true" className="inline-block">
      <line x1="0" y1="2" x2="20" y2="2" stroke={PAPER_MUTE} strokeWidth="1" strokeDasharray={QUAD_DASH} />
    </svg>
  );
}
