// The generic beeswarm (spec_country.md C2; user edit 1b "dots go out beyond the boundaries"): every
// point a dot along one value axis, dodged vertically. The layout is computed here (charts/dodge.ts), not
// by Plot's dodge, so the figure's height always holds its stack:
//
//   height = margins + the swarm's span (2 x (extent + r)) + label rows, between 120 and 480 px;
//   above 480 the dots shrink (r 4 -> 3 -> 2.5 -> 2) and are laid out again; still above, stack mode:
//   each pixel column shows at most k dots and a "+m" above it, with the note "Some economies share
//   values too close to separate; the table lists every one." Marks are drawn at the computed x and y
//   (Plot.dot with x and y channels) and clipped to the frame as a last guard (the x range is inset by
//   the largest dot, so the clip never cuts the dots at the ends of the axis).
//
// Styling is the caller's: each point carries its fill and stroke (the Sector wrapper uses the group
// colours, Strategic the three-colour scale). Point labels (the selected point first, then `labelled`)
// sit above or below their dot where they do not collide. The selected point gets the orange ring, or
// the ink-under-white double ring on three-colour fills (`ringStyle="double"`, C1.4).

import { useCallback, useMemo, type ReactNode } from 'react';
import * as Plot from '@observablehq/plot';
import { scaleLinear, scaleLog } from 'd3';
import { PlotFigure } from './PlotFigure.tsx';
import { useWidth } from './WorldMap.tsx';
import { compactSi, logTicks, textWidth } from './theme.ts';
import { swarmFit, type SwarmFit } from './dodge.ts';
import { ringMarks } from './rings.ts';
import { PAPER, PAPER_DIM, PAPER_MUTE, SIGNAL } from '../theme/palette.ts';

export interface SwarmPoint {
  id: string;
  value: number;
  label: string;
  /** dot fill ('none' for a hollow ring) */
  fill?: string;
  stroke?: string;
  strokeWidth?: number;
  /** 0..1 (older observations 0.5) */
  opacity?: number;
  /** draw the label (the selected point is always labelled) */
  labelled?: boolean;
}

export interface BeeswarmChartProps<P extends SwarmPoint> {
  points: readonly P[];
  ariaLabel: string;
  xLabel?: string;
  log?: boolean;
  /** a fixed x domain (default: the points' extent, nice on a linear axis) */
  domain?: [number, number];
  tickFormat?: (v: number) => string;
  /** dashed reference ticks with their words above the swarm ("World median") */
  medians?: readonly { label: string; value: number }[];
  selected?: string | null;
  onSelect?: (p: P) => void;
  tooltip?: (p: P) => ReactNode;
  /** 'orange' (default) or the double ring for three-colour fills */
  ringStyle?: 'orange' | 'double';
  /** what the dots are, for the stack-mode note (default "economies") */
  noun?: string;
  minHeight?: number;
  maxHeight?: number;
}

export const SWARM_MIN_HEIGHT = 120;
export const SWARM_MAX_HEIGHT = 480;
const SIDE = 16;
/** the x range is inset by the largest dot (r 4 + its stroke), so the frame clip never cuts an end dot */
const INSET = 5;
const AXIS = 36;
const LABEL_ROW = 14;
const MEDIAN_ROW = 18;
/** a further row of median words when two would overlap ("World median" beside "Coalition median") */
const MEDIAN_STEP = 12;
const OVERFLOW_ROW = 12;

export interface SwarmGeometry {
  width: number;
  height: number;
  marginTop: number;
  marginBottom: number;
  /** pixel x of the centre of each value (the x scale) */
  px: (v: number) => number;
  domain: [number, number];
  ticks?: number[];
  fit: SwarmFit;
  /** label placement per point index: above, below or none */
  labels: ('above' | 'below' | null)[];
  /** the median words: the centre of each label (px, kept inside the figure) and its row (0 = top) */
  medianLabels: { label: string; value: number; cx: number; row: number }[];
}

/** The x domain of the points (linear: nice, including 0 when the values straddle it). */
function xDomain(values: number[], log: boolean, fixed?: [number, number]): [number, number] {
  if (fixed) return fixed;
  const lo = Math.min(...values);
  const hi = Math.max(...values);
  if (log) return lo === hi ? [lo / 2, hi * 2] : [lo, hi];
  const s = scaleLinear().domain(lo === hi ? [lo - 1, hi + 1] : [lo, hi]).nice();
  return s.domain() as [number, number];
}

/** The layout of a swarm at a width (pure apart from text measurement estimates). */
export function swarmGeometry(
  points: readonly SwarmPoint[],
  width: number,
  opts: { log?: boolean; domain?: [number, number]; selected?: string | null; medians?: readonly { label: string; value: number }[]; minHeight?: number; maxHeight?: number },
): SwarmGeometry {
  const log = !!opts.log;
  const minH = opts.minHeight ?? SWARM_MIN_HEIGHT;
  const maxH = opts.maxHeight ?? SWARM_MAX_HEIGHT;
  const values = points.map((p) => p.value);
  const domain = xDomain(values.length ? values : [0, 1], log, opts.domain);
  const range: [number, number] = [SIDE + INSET, Math.max(SIDE + INSET + 1, width - SIDE - INSET)];
  const scale = log ? scaleLog().domain(domain).range(range).clamp(true) : scaleLinear().domain(domain).range(range).clamp(true);
  const px = (v: number) => scale(v) as number;
  const xs = points.map((p) => px(p.value));
  const wantLabels = points.some((p) => p.labelled || p.id === opts.selected);
  // median words: centred on their tick, kept inside the figure, and stacked in rows where they collide
  const medianRows: [number, number][][] = [];
  const medianLabels = [...(opts.medians ?? [])]
    .map((m) => ({ ...m, x: px(m.value), w: textWidth(m.label, 10) + 8 }))
    .sort((a, b) => a.x - b.x)
    .map((m) => {
      const cx = Math.max(m.w / 2, Math.min(width - m.w / 2, m.x));
      let row = 0;
      while (medianRows[row]?.some(([x0, x1]) => cx - m.w / 2 < x1 && cx + m.w / 2 > x0)) row++;
      (medianRows[row] ??= []).push([cx - m.w / 2, cx + m.w / 2]);
      return { label: m.label, value: m.value, cx, row };
    });
  const medianH = medianRows.length ? MEDIAN_ROW + (medianRows.length - 1) * MEDIAN_STEP : 6;
  const marginTop = medianH + (wantLabels ? LABEL_ROW : 0) + OVERFLOW_ROW;
  const marginBottom = AXIS + (wantLabels ? LABEL_ROW : 0);
  const maxSwarm = Math.max(16, maxH - marginTop - marginBottom);
  // stack priority: the selected dot first, then labelled ones
  const priority = points.map((p) => (p.id === opts.selected ? 0 : p.labelled ? 1 : 2));
  const fit = swarmFit(xs, { maxSwarm, priority });
  const height = Math.max(minH, Math.min(maxH, Math.ceil(marginTop + marginBottom + fit.span + 2)));
  // label placement (the selected point first): above the dot, else below, else left to the tooltip
  const frameMid = marginTop + (height - marginTop - marginBottom) / 2;
  const order = points.map((_, i) => i).filter((i) => (points[i].labelled || points[i].id === opts.selected) && Number.isFinite(fit.y[i]));
  order.sort((a, b) => Number(points[b].id === opts.selected) - Number(points[a].id === opts.selected));
  const boxes: { x: number; y: number; w: number; h: number }[] = [];
  const owner: { i: number; side: 'above' | 'below' }[] = [];
  for (const i of order) {
    const w = textWidth(points[i].label, 11) + 6;
    const cy = frameMid - fit.y[i];
    const x = Math.max(w / 2, Math.min(width - w / 2, fit.x[i]));
    boxes.push({ x, y: cy - fit.r - 8, w, h: 13 });
    owner.push({ i, side: 'above' });
    boxes.push({ x, y: cy + fit.r + 9, w, h: 13 });
    owner.push({ i, side: 'below' });
  }
  // dots are obstacles too: a label never covers a dot
  const dotBoxes = points.map((_, i) => (Number.isFinite(fit.y[i]) ? { x: fit.x[i], y: frameMid - fit.y[i], w: 2 * fit.r, h: 2 * fit.r } : null));
  const labels: ('above' | 'below' | null)[] = points.map(() => null);
  const placed: { x0: number; x1: number; y0: number; y1: number }[] = [];
  const hit = (b: { x: number; y: number; w: number; h: number }) => {
    const r = { x0: b.x - b.w / 2, x1: b.x + b.w / 2, y0: b.y - b.h / 2, y1: b.y + b.h / 2 };
    if (r.y0 < 0 || r.y1 > height - AXIS + 4) return true;
    if (placed.some((p) => r.x0 < p.x1 && r.x1 > p.x0 && r.y0 < p.y1 && r.y1 > p.y0)) return true;
    return dotBoxes.some((d) => d && r.x0 < d.x + d.w / 2 && r.x1 > d.x - d.w / 2 && r.y0 < d.y + d.h / 2 && r.y1 > d.y - d.h / 2);
  };
  for (let k = 0; k < boxes.length; k += 2) {
    const { i } = owner[k];
    for (const j of [k, k + 1]) {
      const b = boxes[j];
      if (hit(b)) continue;
      placed.push({ x0: b.x - b.w / 2, x1: b.x + b.w / 2, y0: b.y - b.h / 2, y1: b.y + b.h / 2 });
      labels[i] = owner[j].side;
      break;
    }
  }
  const ticks = log ? logTicks(domain[0], domain[1], Math.max(3, Math.floor((width - 2 * SIDE) / 90))) : undefined;
  return { width, height, marginTop, marginBottom, px, domain, ticks, fit, labels, medianLabels };
}

export function BeeswarmChart<P extends SwarmPoint>({
  points, ariaLabel, xLabel, log, domain, tickFormat, medians = [], selected, onSelect, tooltip, ringStyle = 'orange', noun = 'economies', minHeight, maxHeight,
}: BeeswarmChartProps<P>) {
  const [ref, width] = useWidth<HTMLDivElement>();
  const pts = useMemo(() => points.filter((p) => Number.isFinite(p.value) && (!log || p.value > 0)), [points, log]);
  const ms = useMemo(() => medians.filter((m) => Number.isFinite(m.value) && (!log || m.value > 0)), [medians, log]);
  const geo = useMemo(
    () => (width > 0 && pts.length ? swarmGeometry(pts, width, { log, domain, selected, medians: ms, minHeight, maxHeight }) : null),
    [pts, width, log, domain, selected, ms, minHeight, maxHeight],
  );
  const build = useCallback(
    (w: number): Plot.PlotOptions | null => {
      if (!geo || !pts.length) return null;
      const g = w === geo.width ? geo : swarmGeometry(pts, w, { log, domain, selected, medians: ms, minHeight, maxHeight });
      const { fit } = g;
      const frameH = g.height - g.marginTop - g.marginBottom;
      const half = frameH / 2;
      const shown = pts.map((_, i) => i).filter((i) => Number.isFinite(fit.y[i]));
      const data = shown.map((i) => pts[i]);
      // x in data units (stack mode snaps to columns: invert the snapped pixel)
      const xv = shown.map((i) => (fit.mode === 'stack' ? invert(g, fit.x[i]) : pts[i].value));
      const yv = shown.map((i) => fit.y[i]);
      const r = fit.r;
      const sel = shown.filter((i) => pts[i].id === selected);
      const selX = sel.map((i) => (fit.mode === 'stack' ? invert(g, fit.x[i]) : pts[i].value));
      const selY = sel.map((i) => fit.y[i]);
      // point labels: one mark per side (Plot's dy is a constant)
      const labelMark = (side: 'above' | 'below') => {
        const lab = shown.filter((i) => g.labels[i] === side);
        return Plot.text(lab.map((i) => pts[i]), {
          x: lab.map((i) => (fit.mode === 'stack' ? invert(g, fit.x[i]) : pts[i].value)),
          y: lab.map((i) => fit.y[i]),
          text: lab.map((i) => pts[i].label),
          dy: side === 'above' ? -(r + 8) : r + 10,
          fill: lab.map((i) => (pts[i].id === selected ? PAPER : PAPER_DIM)),
          fontSize: 11,
          fontWeight: 500,
        } as Plot.TextOptions);
      };
      const over = fit.overflow;
      const overX = over.map((o) => invert(g, o.x));
      const overY = over.map((o) => o.top);
      const frame = { x: xv, y: yv };
      const ring = selected && sel.length
        ? ringStyle === 'double'
          ? ringMarks(sel.map((i) => pts[i]), { x: selX, y: selY, r: r + 1.5 }, 'selected', 'dot')
          : [Plot.dot(sel.map((i) => pts[i]), { x: selX, y: selY, r: r + 1.5, fill: 'none', stroke: SIGNAL, strokeWidth: 2.5 })]
        : [];
      const hover = ringStyle === 'double'
        ? ringMarks(data, { ...frame, r: r + 1.5, maxRadius: 16 }, 'hover', 'dot')
        : [Plot.dot(data, Plot.pointer({ ...frame, r: r + 1.5, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 16 }))];
      return {
        // the layout's own height (PlotFigure measures its host; a width other than the wrapper's re-lays out)
        height: g.height,
        marginTop: g.marginTop,
        marginBottom: g.marginBottom,
        marginLeft: SIDE,
        marginRight: SIDE,
        x: log
          ? { type: 'log', domain: g.domain, label: xLabel, grid: true, ticks: g.ticks, tickFormat: tickFormat ?? compactSi, insetLeft: INSET, insetRight: INSET }
          : { type: 'linear', domain: g.domain, label: xLabel, grid: true, tickFormat, insetLeft: INSET, insetRight: INSET },
        y: { domain: [-half, half], axis: null },
        marks: [
          Plot.tickX(ms, { x: 'value', stroke: PAPER_MUTE, strokeDasharray: '3 3' }),
          // the median words, one mark per row (Plot's dy is a constant), each at its kept-inside centre
          ...[...new Set(g.medianLabels.map((m) => m.row))].map((row) => {
            const ml = g.medianLabels.filter((m) => m.row === row);
            return Plot.text(ml, { x: ml.map((m) => invert(g, m.cx)), frameAnchor: 'top', text: 'label', dy: -(g.marginTop - 8 - row * MEDIAN_STEP), fill: PAPER_MUTE, fontSize: 10, textAnchor: 'middle' } as Plot.TextOptions);
          }),
          Plot.dot(data, {
            ...frame,
            r,
            fill: data.map((p) => p.fill ?? PAPER_DIM),
            stroke: data.map((p) => p.stroke ?? 'none'),
            strokeWidth: data.map((p) => p.strokeWidth ?? 1),
            fillOpacity: data.map((p) => p.opacity ?? 1),
            strokeOpacity: data.map((p) => p.opacity ?? 1),
            clip: 'frame',
          } as Plot.DotOptions),
          ...ring,
          labelMark('above'),
          labelMark('below'),
          Plot.text(over, { x: overX, y: overY, text: over.map((o) => `+${o.hidden}`), dy: -(r + 7), fill: PAPER_DIM, fontSize: 10, fontWeight: 500 } as Plot.TextOptions),
          ...hover,
        ],
      };
    },
    [geo, pts, log, domain, selected, ms, minHeight, maxHeight, xLabel, tickFormat, ringStyle],
  );
  return (
    <div ref={ref} className="w-full" data-testid="beeswarm" data-mode={geo?.fit.mode} data-height={geo?.height}>
      {pts.length > 0 ? (
        geo && <PlotFigure<P> build={build} height={geo.height} ariaLabel={ariaLabel} onSelect={onSelect} tooltip={tooltip} />
      ) : (
        <p className="py-6 text-center text-sm text-paper-mute">No {noun} with a value to place.</p>
      )}
      {geo && geo.fit.mode === 'stack' && (
        <p className="mt-1 text-xs text-paper-mute" data-testid="swarm-stack-note">
          Some {noun} share values too close to separate: a number above a column counts the dots not drawn there. The table lists every one.
        </p>
      )}
    </div>
  );
}

/** The data value at a pixel x of the geometry (stack mode draws dots at their column centres). */
function invert(g: SwarmGeometry, x: number): number {
  const [lo, hi] = g.domain;
  const a = SIDE + INSET;
  const b = Math.max(SIDE + INSET + 1, g.width - SIDE - INSET);
  const t = (x - a) / (b - a);
  // the log axis interpolates in log space
  if (lo > 0 && hi > 0 && g.ticks) return Math.pow(10, Math.log10(lo) + t * (Math.log10(hi) - Math.log10(lo)));
  return lo + t * (hi - lo);
}
