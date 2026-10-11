// LabelledLines (spec_sector.md 4.3 S5, 4.6, 5.3): up to 7 lines, 2 px, every line with its end
// label (colliding labels are nudged apart, with a short leader to the line end when moved); group
// colours (economies are told apart by their labels, not by colour), and lines of the same group get
// distinct dash patterns (solid, long dash, dot, dash-dot) so they can be told apart without the label;
// neutral context lines; the highlighted line drawn last with a 2 px orange halo; estimated points
// hollow; projected segments thinner and named "projection" in the tooltip, with hollow markers.
// Optional index (first point = 100). A legend lists every line (two or more series) with its pattern.

import { useCallback, useMemo } from 'react';
import * as Plot from '@observablehq/plot';
import { scaleLinear, scaleLog } from 'd3';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin } from '../../charts/theme.ts';
import { Legend } from '../../charts/legends.tsx';
import { INK, NEUTRAL_SERIES, PAPER_DIM, SIGNAL } from '../../theme/palette.ts';
import { groupColor } from './color.ts';
import type { LabelledLinesProps, LineSeries } from './types.ts';

export const MAX_LINES = 7;
/** Dash patterns of the 1st, 2nd, 3rd and 4th line of one group (solid first). */
export const LINE_DASHES = [null, '7 3', '2 3', '9 3 2 3'] as const;
const LABEL_GAP = 13;

/** The dash pattern of each line: lines of the same group take the patterns in order. */
export function lineDashes(lines: readonly LineSeries[]): Map<string, string | null> {
  const k = new Map<string, number>();
  const out = new Map<string, string | null>();
  for (const l of lines) {
    const i = k.get(l.group) ?? 0;
    k.set(l.group, i + 1);
    out.set(l.id, LINE_DASHES[i % LINE_DASHES.length]);
  }
  return out;
}

/**
 * End-label positions (pixels, 0 = top) for anchors `y0`: in order, each label at least `gap` below
 * the one above; labels pushed past `bottom` are moved back up. Every label is kept.
 */
export function spreadLabels(y0: readonly number[], gap: number, bottom: number): number[] {
  const order = y0.map((y, i) => ({ y, i })).sort((a, z) => a.y - z.y || a.i - z.i);
  const ys = order.map((o) => o.y);
  for (let j = 1; j < ys.length; j++) if (ys[j] < ys[j - 1] + gap) ys[j] = ys[j - 1] + gap;
  if (ys.length && ys[ys.length - 1] > bottom) {
    ys[ys.length - 1] = bottom;
    for (let j = ys.length - 2; j >= 0; j--) if (ys[j] > ys[j + 1] - gap) ys[j] = ys[j + 1] - gap;
  }
  const out = new Array<number>(y0.length);
  order.forEach((o, j) => (out[o.i] = ys[j]));
  return out;
}

interface Pt {
  id: string;
  label: string;
  x: number;
  y: number;
  raw: number;
  est: boolean;
  projection: boolean;
}

/** Index a line to its first finite point = 100 (a zero or missing base leaves the line out). */
export function indexLine(l: LineSeries): LineSeries | null {
  const base = l.points.find((p) => Number.isFinite(p.y));
  if (!base || base.y === 0) return null;
  return { ...l, points: l.points.map((p) => ({ ...p, y: (100 * p.y) / base.y })) };
}

const defaultX = (v: number) => String(Math.round(v));

export function LabelledLines({ lines, highlight, index, yLabel, ariaLabel, yTickFormat, valueFormat, xTickFormat = defaultX, onPickX, log, height = 280 }: LabelledLinesProps) {
  const used = useMemo(() => {
    const ls = lines.slice(0, MAX_LINES);
    return index ? (ls.map(indexLine).filter(Boolean) as LineSeries[]) : ls;
  }, [lines, index]);
  const rawById = useMemo(() => new Map(lines.map((l) => [l.id, l])), [lines]);
  const pts = useMemo(
    () =>
      used.flatMap((l) =>
        l.points
          .map((p, i) => ({ id: l.id, label: l.label, x: p.x, y: p.y, raw: rawById.get(l.id)?.points[i]?.y ?? p.y, est: !!p.est, projection: !!p.projection }))
          .filter((p) => Number.isFinite(p.x) && Number.isFinite(p.y) && (!log || p.y > 0)),
      ),
    [used, rawById, log],
  );
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!pts.length) return null;
      const color = (id: string) => {
        const l = used.find((x) => x.id === id);
        return l ? groupColor(l.group) : NEUTRAL_SERIES;
      };
      // order: neutral context first, then groups, the highlight last
      const order = [...used].sort((a, b) => Number(a.id === highlight) - Number(b.id === highlight) || Number(b.group === 'neutral') - Number(a.group === 'neutral'));
      const lastPts = used
        .map((l) => [...pts].reverse().find((p) => p.id === l.id))
        .filter((p): p is Pt => !!p);
      const ys = pts.map((p) => p.y);
      const lo = log ? Math.min(...ys) : Math.min(0, ...ys);
      const hi = Math.max(...ys);
      const mt = 30;
      const mb = 36;
      const innerH = height - mt - mb;
      // the y scale as Plot will draw it (the same nice domain, passed explicitly), to place the labels
      const ysc = log ? scaleLog().domain([lo, hi === lo ? lo * 10 : hi]).nice().range([innerH, 0]) : scaleLinear().domain([lo, hi === lo ? lo + 1 : hi]).nice().range([innerH, 0]);
      const dom = ysc.domain() as [number, number];
      const placed = spreadLabels(lastPts.map((p) => ysc(p.y)), LABEL_GAP, innerH);
      const endLabels = lastPts.map((p, i) => ({ ...p, ly: ysc.invert(placed[i]), moved: Math.abs(placed[i] - ysc(p.y)) > 2 }));
      const mr = labelMargin(endLabels.map((p) => p.label), { min: 16, max: Math.min(160, Math.floor(width * 0.3)) }) + 6;
      const dash = lineDashes(used);
      const marks: Plot.Markish[] = [];
      for (const l of order) {
        const lp = pts.filter((p) => p.id === l.id);
        // observed points; projected ones thinner, joined to the last observation
        const obs = lp.filter((p) => !p.projection);
        const projected = lp.filter((p) => p.projection);
        const proj = projected.length && obs.length ? [obs[obs.length - 1], ...projected] : projected;
        const stroke = color(l.id);
        const da = dash.get(l.id) ?? null;
        // the highlight's halo: a wider orange line under it (2 px each side)
        if (l.id === highlight) marks.push(Plot.lineY(lp, { x: 'x', y: 'y', stroke: SIGNAL, strokeWidth: 6, strokeOpacity: 0.9 }));
        if (obs.length) marks.push(Plot.lineY(obs, { x: 'x', y: 'y', stroke, strokeWidth: l.group === 'neutral' ? 1.5 : 2, ...(da ? { strokeDasharray: da } : {}) }));
        if (proj.length > 1) marks.push(Plot.lineY(proj, { x: 'x', y: 'y', stroke, strokeWidth: 1.25, strokeOpacity: 0.8, strokeDasharray: da ?? '4 3' }));
        const hollow = lp.filter((p) => p.est || p.projection);
        if (hollow.length) marks.push(Plot.dot(hollow, { x: 'x', y: 'y', r: 4, fill: INK[800], stroke, strokeWidth: 2 }));
      }
      // a label moved off its line end gets a short leader from the end to the label
      const moved = endLabels.filter((p) => p.moved);
      if (moved.length) marks.push(Plot.link(moved, { x1: 'x', x2: 'x', y1: 'y', y2: 'ly', dx: 4, stroke: PAPER_DIM, strokeOpacity: 0.6, strokeWidth: 1 }));
      marks.push(Plot.text(endLabels, { x: 'x', y: 'ly', text: 'label', dx: moved.length ? 8 : 6, textAnchor: 'start', fill: PAPER_DIM, fontSize: 11 }));
      marks.push(Plot.dot(pts, Plot.pointer({ x: 'x', y: 'y', r: 5, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 24 })));
      return {
        marginTop: mt,
        marginBottom: mb,
        marginRight: mr,
        x: { label: null, tickFormat: xTickFormat, ticks: Math.max(2, Math.floor(width / 70)) },
        y: { type: log ? 'log' : 'linear', label: index ? `${yLabel} (first year = 100)` : yLabel, grid: true, tickFormat: index ? undefined : yTickFormat, domain: dom, nice: false },
        marks,
      };
    },
    [pts, used, highlight, index, yLabel, yTickFormat, xTickFormat, log, height],
  );
  const legend = useMemo(() => {
    if (used.length < 2) return null;
    const dash = lineDashes(used);
    return used.map((l) => ({ label: l.label, color: l.id === highlight ? SIGNAL : groupColor(l.group), kind: 'line' as const, dash: dash.get(l.id) ?? null }));
  }, [used, highlight]);
  return (
    <div>
      <PlotFigure<Pt>
        build={build}
        height={height}
        ariaLabel={ariaLabel}
        onSelect={onPickX ? (p) => onPickX(p.x) : undefined}
        tooltip={(p) => (
          <Tip
            value={index ? `${p.y.toFixed(1)} (index)` : valueFormat(p.y)}
            title={p.label}
            lines={[xTickFormat(p.x), index ? valueFormat(p.raw) : null, p.est ? 'includes an estimate' : null, p.projection ? 'projection' : null]}
          />
        )}
      />
      {legend && (
        <div className="mt-2">
          <Legend items={legend} label="Lines" />
        </div>
      )}
    </div>
  );
}
