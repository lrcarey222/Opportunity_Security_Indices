// Draws a Chart of the Day spec (data.ts) with Observable Plot. Five shapes: ranked bars (focus country in the
// signal colour, or suppliers coloured by UN-voting bloc), exports vs imports by supply-chain step, investment lines
// (focus bold), an exports-vs-imports scatter on log scales, and a technology x step grid of China's import share.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure } from '../charts/PlotFigure.tsx';
import { BLOC_COLOR, PAPER_DIM, PAPER_MUTE, SIGNAL } from '../theme/palette.ts';
import { fmtValue, type BarRow, type CodFormat, type CodSpec, type DumbbellRow, type GroupedRow, type LineRow, type ScatterRow, type SeriesRow, type TileRow } from './data.ts';

const OTHER = '#6b7280';
/** Axis ticks in dollars without a trailing ".0" ($60bn, not $60.0bn). */
const axisUsd = (v: number) => fmtValue(v, 'usd').replace('.0', '');
/** Axis tick formatter for a value format. */
const axisFmt = (f: CodFormat) => (f === 'usd' ? axisUsd : f === 'pct' || f === 'pct2' ? (v: number) => `${Math.round(v * 100)}%` : (v: number) => fmtValue(v, f === 'count' ? 'count' : 'num'));
const EXPORTS = '#8b9d17';
const IMPORTS = '#1889b0';
const BLOC: Record<string, string> = {
  'U.S.-leaning': BLOC_COLOR.us_leaning,
  'China-leaning': BLOC_COLOR.china_leaning,
  Nonaligned: BLOC_COLOR.nonaligned,
  Others: '#374151',
};

export function chartHeight(spec: CodSpec, compact = false, k = 1): number {
  return Math.round(baseHeight(spec, compact) * (k === 1 ? 1 : 0.5 + k / 2));
}

function baseHeight(spec: CodSpec, compact: boolean): number {
  const c = spec.chart;
  if (c.type === 'bar') return Math.max(compact ? 180 : 220, spec.rows.length * (compact ? 22 : 30) + 40);
  if (c.type === 'tiles') return (c.rows_order.length + 1) * (compact ? 24 : 32) + 40;
  if (c.type === 'dumbbell') return spec.rows.length * (compact ? 28 : 36) + 60;
  if (c.type === 'stacked_bar') return compact ? 240 : 380;
  return compact ? 240 : 360;
}

/** `textScale` enlarges every label (the PNG export draws at 1.35x so text survives being shared at small sizes). */
export function ChartOfDayFigure({ spec, compact = false, textScale = 1 }: { spec: CodSpec; compact?: boolean; textScale?: number }) {
  const k = textScale;
  const h = chartHeight(spec, compact, k);
  const f = spec.chart.format;
  const fs = useCallback((n: number) => Math.round(n * k * 10) / 10, [k]);
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      const o = buildOptions(width);
      if (!o || k === 1) return o;
      const m = (v: unknown) => (typeof v === 'number' ? Math.round(v * k) : v);
      return {
        ...o,
        marginLeft: m(o.marginLeft ?? 48) as number,
        marginRight: m(o.marginRight ?? 16) as number,
        // a labelled vertical y axis needs its own row above the top tick (PlotFigure adds it only at scale 1)
        marginTop: m(o.marginTop ?? (typeof (o.y as { label?: unknown } | undefined)?.label === 'string' ? 30 : 16)) as number,
        marginBottom: m(o.marginBottom ?? 36) as number,
        style: { ...((o.style as object) ?? {}), fontSize: `${fs(12)}px` },
      };
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [spec, f, k],
  );
  // the chart options at text scale 1 (a hoisted function, so build can call it)
  function buildOptions(width: number): Plot.PlotOptions | null {
      const c = spec.chart;
      const narrow = width < 560;
      if (c.type === 'bar') {
        const rows = spec.rows as BarRow[];
        const palette = c.colors ?? BLOC;
        const color = (d: BarRow) => (c.color_by === 'group' ? (palette[d.group ?? ''] ?? OTHER) : d.highlight ? SIGNAL : OTHER);
        return {
          marginLeft: narrow ? 120 : 170,
          marginRight: 64,
          x: { label: null, axis: null },
          y: { domain: rows.map((r) => r.label), label: null, tickSize: 0 },
          marks: [
            Plot.barX(rows, { x: 'value', y: 'label', fill: color }),
            Plot.text(rows, { x: 'value', y: 'label', text: (d: BarRow) => fmtValue(d.value, f), textAnchor: 'start', dx: 4, fill: PAPER_DIM, fontSize: fs(11) }),
            Plot.ruleX([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
            Plot.tip(rows, Plot.pointerY({ x: 'value', y: 'label', title: (d: BarRow) => `${d.label}\n${fmtValue(d.value, f)}` })),
          ],
          style: { fontSize: '12px' },
        };
      }
      if (c.type === 'grouped_bar') {
        const rows = spec.rows as GroupedRow[];
        const cats = [...new Set(rows.map((r) => r.category))];
        return {
          marginLeft: 56,
          marginBottom: 40,
          fx: { domain: cats, label: null, padding: 0.15 },
          x: { domain: c.series, axis: null, padding: 0.1 },
          y: { label: '↑ US dollars', tickFormat: axisUsd, grid: true },
          color: { domain: ['Exports', 'Imports'], range: [EXPORTS, IMPORTS] },
          marks: [
            Plot.barY(rows, { fx: 'category', x: 'series', y: 'value', fill: 'series', tip: { format: { y: (v: number) => fmtValue(v, 'usd'), fx: true, x: true, fill: false } } }),
            Plot.ruleY([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          ],
        };
      }
      if (c.type === 'lines') {
        const rows = spec.rows as LineRow[];
        const last = [...new Set(rows.map((r) => r.series))].map((s) => {
          const pts = rows.filter((r) => r.series === s).sort((a, b) => a.x - b.x);
          return pts[pts.length - 1];
        });
        return {
          marginLeft: 56,
          marginRight: narrow ? 90 : 120,
          x: { label: null, tickFormat: 'd', ticks: [...new Set(rows.map((r) => r.x))] },
          y: { label: `↑ ${c.y_label ?? 'Investment, 2024 dollars'}`, tickFormat: axisFmt(f), grid: true },
          marks: [
            Plot.ruleY([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
            Plot.line(rows.filter((r) => !r.highlight), { x: 'x', y: 'value', z: 'series', stroke: PAPER_MUTE, strokeWidth: 1.5, strokeOpacity: 0.7 }),
            Plot.line(rows.filter((r) => r.highlight), { x: 'x', y: 'value', stroke: SIGNAL, strokeWidth: 3 }),
            Plot.dot(rows.filter((r) => r.highlight), { x: 'x', y: 'value', fill: SIGNAL, r: 3 }),
            Plot.text(last, { x: 'x', y: 'value', text: 'series', textAnchor: 'start', dx: 6, fill: (d: LineRow) => (d.highlight ? SIGNAL : PAPER_DIM), fontSize: fs(11) }),
            Plot.tip(rows, Plot.pointer({
              x: 'x', y: 'value',
              title: (d: LineRow) => {
                const indexed = (c.y_label ?? '').startsWith('Index');
                const lvl = d.level !== undefined && c.level_format ? `${fmtValue(d.level, c.level_format)}${c.level_unit ? ` ${c.level_unit}` : ''}` : '';
                return indexed ? `${d.series}, ${d.x}\nIndex ${Math.round(d.value)}${lvl ? ` (${lvl})` : ''}` : `${d.series}, ${d.x}\n${fmtValue(d.value, f)}`;
              },
            })),
          ],
        };
      }
      if (c.type === 'scatter') {
        const rows = spec.rows as ScatterRow[];
        const vals = rows.flatMap((r) => [r.x, r.y]).filter((v) => v > 0);
        const lo = Math.min(...vals) * 0.8;
        const hi = Math.max(...vals) * 1.25;
        const others = rows.filter((r) => !r.highlight);
        const focus = rows.filter((r) => r.highlight);
        return {
          marginLeft: 64,
          marginBottom: 44,
          x: { type: 'log', domain: [lo, hi], label: `${c.x_label} →`, tickFormat: axisUsd, ticks: 5, grid: true },
          y: { type: 'log', domain: [lo, hi], label: `↑ ${c.y_label}`, tickFormat: axisUsd, ticks: 5, grid: true },
          marks: [
            Plot.line([[lo, lo], [hi, hi]], { stroke: 'rgb(255 255 255 / 0.3)', strokeDasharray: '4,4' }),
            Plot.dot(others, { x: 'x', y: 'y', fill: OTHER, fillOpacity: 0.7, r: 3.5 }),
            Plot.dot(focus, { x: 'x', y: 'y', fill: SIGNAL, r: 7, stroke: '#111827' }),
            Plot.text(rows.filter((r) => r.show_label), {
              x: 'x', y: 'y', text: 'label', dy: -10, fontSize: fs(11),
              fill: (d: ScatterRow) => (d.highlight ? SIGNAL : PAPER_DIM), stroke: '#111827', strokeWidth: 3, paintOrder: 'stroke',
            }),
            Plot.tip(rows, Plot.pointer({ x: 'x', y: 'y', title: (d: ScatterRow) => `${d.label}\nExports ${fmtValue(d.y, 'usd')}\nImports ${fmtValue(d.x, 'usd')}` })),
          ],
        };
      }
      if (c.type === 'stacked_bar') {
        const rows = spec.rows as SeriesRow[];
        const years = [...new Set(rows.map((r) => r.x))].sort((a, b) => a - b);
        const every = width < 560 && years.length > 10 ? 5 : years.length > 14 ? 2 : 1;
        return {
          marginLeft: 56,
          x: { domain: years, label: null, tickFormat: (y: number) => (years.indexOf(y) % every === 0 || y === years[years.length - 1] ? String(y) : ''), padding: 0.15 },
          y: { label: `↑ ${c.y_label}`, tickFormat: axisFmt(f), grid: true, ...(f === 'pct' ? { domain: [0, 1] } : {}) },
          color: { domain: c.series, range: c.series.map((s) => c.colors[s] ?? OTHER) },
          marks: [
            Plot.barY(rows, { x: 'x', y: 'value', fill: 'series', order: c.series }),
            Plot.ruleY([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
            Plot.tip(rows, Plot.pointer(Plot.stackY({ x: 'x', y: 'value', fill: 'series', order: c.series, title: (d: SeriesRow) => `${d.series}, ${d.x}\n${fmtValue(d.value, f)}` }))),
          ],
        };
      }
      if (c.type === 'dumbbell') {
        const rows = spec.rows as DumbbellRow[];
        const col = (d: DumbbellRow) => (d.to >= d.from ? SIGNAL : IMPORTS);
        return {
          marginLeft: narrow ? 170 : 230,
          marginRight: 24,
          marginTop: 30,
          x: { domain: [0, 1], label: null, tickFormat: (v: number) => `${Math.round(v * 100)}%`, grid: true, axis: 'top' },
          y: { domain: rows.map((r) => r.label), label: null, tickSize: 0 },
          marks: [
            Plot.link(rows, { x1: 'from', x2: 'to', y: 'label', stroke: col, strokeWidth: 2.5, markerEnd: 'arrow' }),
            Plot.dot(rows, { x: 'from', y: 'label', fill: PAPER_MUTE, r: 4 }),
            Plot.text(rows, { x: 'from', y: 'label', text: (d: DumbbellRow) => fmtValue(d.from, 'pct'), dy: -11, fill: PAPER_MUTE, fontSize: fs(10) }),
            Plot.text(rows, { x: 'to', y: 'label', text: (d: DumbbellRow) => fmtValue(d.to, 'pct'), dy: -11, fill: col, fontSize: fs(11), fontWeight: 600 }),
            Plot.tip(rows, Plot.pointerY({ x: 'to', y: 'label', title: (d: DumbbellRow) => `${d.label}\n${c.from_label}: ${fmtValue(d.from, 'pct')}\n${c.to_label}: ${fmtValue(d.to, 'pct')}\nImports, latest: ${fmtValue(d.imports, 'usd')}` })),
          ],
        };
      }
      if (c.type === 'tiles') {
        const rows = spec.rows as TileRow[];
        return {
          marginLeft: narrow ? 110 : 140,
          marginTop: 30,
          marginBottom: 8,
          padding: 0.06,
          x: { domain: c.cols_order, label: null, axis: 'top', tickSize: 0 },
          y: { domain: c.rows_order, label: null, tickSize: 0 },
          color: { type: 'linear', domain: [0, 1], range: ['#1f2937', SIGNAL], clamp: true },
          marks: [
            Plot.cell(rows, { x: 'col', y: 'row', fill: 'value', inset: 1 }),
            // the cell the title is about
            Plot.cell(rows.filter((r) => r.row === spec.tech && r.stage === spec.stage), { x: 'col', y: 'row', fill: null, stroke: '#ffffff', strokeWidth: 2.5, inset: 1 }),
            Plot.text(rows, { x: 'col', y: 'row', text: (d: TileRow) => fmtValue(d.value, 'pct'), fill: (d: TileRow) => (d.value > 0.55 ? '#111827' : PAPER_DIM), fontSize: fs(narrow ? 10 : 12) }),
            Plot.tip(rows, Plot.pointer({ x: 'col', y: 'row', title: (d: TileRow) => `${d.row}, ${d.col}\n${fmtValue(d.value, 'pct')} from China\nof ${fmtValue(d.imports, 'usd')} imports` })),
          ],
        };
      }
      return null;
  }
  return <PlotFigure build={build} height={h} ariaLabel={`${spec.title}. ${spec.subtitle}`} />;
}

/** Key for the China-shift chart: grey dot = 2024, arrowhead = latest 12 months. */
export function ShiftLegend({ spec, className = 'text-xs' }: { spec: CodSpec; className?: string }) {
  if (spec.chart.type !== 'dumbbell') return null;
  return (
    <div className={`mt-2 flex flex-wrap gap-4 text-paper-dim ${className}`} aria-label="Colour key">
      <span className="inline-flex items-center gap-1.5">
        <span aria-hidden="true" className="inline-block h-2.5 w-2.5 rounded-full" style={{ background: PAPER_MUTE }} />
        China's share in {spec.chart.from_label}
      </span>
      <span className="inline-flex items-center gap-1.5">
        <span aria-hidden="true" className="inline-block h-0.5 w-5" style={{ background: SIGNAL }} />
        Higher by {spec.chart.to_label}
      </span>
      <span className="inline-flex items-center gap-1.5">
        <span aria-hidden="true" className="inline-block h-0.5 w-5" style={{ background: IMPORTS }} />
        Lower by {spec.chart.to_label}
      </span>
    </div>
  );
}

/** Key for stacked series (power mix, investment by sector, capacity by technology). */
export function SeriesLegend({ spec, className = 'text-xs' }: { spec: CodSpec; className?: string }) {
  if (spec.chart.type !== 'stacked_bar') return null;
  const c = spec.chart;
  return (
    <div className={`mb-2 flex flex-wrap gap-x-4 gap-y-1 text-paper-dim ${className}`} aria-label="Colour key">
      {c.series.map((s) => (
        <span key={s} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-3 w-3" style={{ background: c.colors[s] ?? OTHER }} />
          {s}
        </span>
      ))}
    </div>
  );
}

/** Key for exports vs imports by step. */
export function TradeLegend({ spec, className = 'text-xs' }: { spec: CodSpec; className?: string }) {
  if (spec.chart.type !== 'grouped_bar') return null;
  return (
    <div className={`mb-2 flex flex-wrap gap-4 text-paper-dim ${className}`} aria-label="Colour key">
      {[['Exports', EXPORTS], ['Imports', IMPORTS]].map(([k, c]) => (
        <span key={k} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-3 w-3" style={{ background: c }} />
          {k}
        </span>
      ))}
    </div>
  );
}

/** Legend for supplier/customer bars coloured by bloc. */
export function BlocLegend({ spec, className = 'text-xs' }: { spec: CodSpec; className?: string }) {
  if (spec.chart.type !== 'bar' || spec.chart.color_by !== 'group') return null;
  const groups = [...new Set((spec.rows as BarRow[]).map((r) => r.group ?? 'Others'))];
  const own = spec.chart.colors;
  if (own) {
    return (
      <div className={`mt-2 flex flex-wrap gap-4 text-paper-dim ${className}`} aria-label="Colour key">
        {Object.keys(own).filter((g) => groups.includes(g)).map((g) => (
          <span key={g} className="inline-flex items-center gap-1.5">
            <span aria-hidden="true" className="inline-block h-3 w-3" style={{ background: own[g] }} />
            {g}
          </span>
        ))}
      </div>
    );
  }
  const order = ['U.S.-leaning', 'Nonaligned', 'China-leaning', 'Others'].filter((g) => groups.includes(g));
  return (
    <div className={`mt-2 flex flex-wrap gap-4 text-paper-dim ${className}`} aria-label="Colour key">
      {order.map((g) => (
        <span key={g} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-3 w-3" style={{ background: BLOC[g] }} />
          {g === 'Others' ? 'All other countries' : g}
        </span>
      ))}
    </div>
  );
}

