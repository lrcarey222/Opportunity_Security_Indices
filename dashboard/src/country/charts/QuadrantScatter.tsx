// QuadrantScatter (spec_country.md A4.2 S2): one country's placed sectors at their percentiles of Economic
// Opportunity (across) and Energy Security (up), 0-100 on both axes (the share of other economies the
// country beats in each sector), dashed crosshairs at the country's own median sector on each axis
// (summary.country_median; the user's rule of 2026-10-15), so the corners sort the country's relatively
// strong and weak sectors; the four named corners in the chart's margins (above and below the plot area,
// so no dot ever hides them; sectors crowd the corners), shape = stage (one of six, design S4: never a
// colour), filled = among the country's top five Strategic Index sectors. No colour encoding (position
// carries the corner). The selected sector gets the orange ring and a bold white label. Labels (six stages,
// 2026-10-29: up to 58 dots): the selected sector always; then the high-priority and top-five sectors, then
// the rest, each only where it fits without covering an earlier label (below 640 px the rest go unlabelled);
// with more than 33 dots the dots are a little smaller. Every sector is in the table twin.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { symbol as d3Symbol, symbolCircle, symbolCross, symbolDiamond, symbolSquare, symbolStar, symbolTriangle, type SymbolType } from 'd3';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { useWidth } from './useWidth.ts';
import { declutter, textWidth } from '../../charts/theme.ts';
import { PAPER_MUTE, SIGNAL } from '../../theme/palette.ts';
import { SUPPLY_STAGES, STAGE_LABEL, type SupplyStage } from '../../lib/stages.ts';
import type { FactSheet } from '../contract.ts';
import { QUADRANTS } from '../contract.ts';
import type { ScatterPoint } from '../derive/model.ts';
import { quadrantWord, proseName, poss, cap } from '../derive/text.ts';

const MARK = '#d1d5db';
/** The dot shape of each of the six stages (Plot's fillable symbols, so a top-five sector can be filled). */
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
/** The dot radius: 6 px, 5 px when more than 33 sectors are placed (the six-stage sheets place up to 58). */
export const scatterRadius = (n: number): number => (n > 33 ? 5 : 6);

// x = Economic Opportunity, y = Energy Security: Grow top right, Stable (key watch) top left, Exposed
// (key secure) bottom left, Upgrade bottom right. The headings print the QUADRANTS labels.
const CORNERS = [
  { id: 'grow', x: 100, y: 100, anchor: 'end', top: true },
  { id: 'watch', x: 0, y: 100, anchor: 'start', top: true },
  { id: 'secure', x: 0, y: 0, anchor: 'start', top: false },
  { id: 'upgrade', x: 100, y: 0, anchor: 'end', top: false },
] as const;

export interface QuadrantScatterProps {
  sheet: FactSheet;
  points: ScatterPoint[];
  selected: string | null;
  onSelect: (key: string) => void;
  ariaLabel: string;
}

/** The scatter's height for a width: square-ish, clamped to 320-560 px. */
export const scatterHeight = (w: number): number => Math.max(320, Math.min(560, Math.round(w)));

export function QuadrantScatter({ sheet, points, selected, onSelect, ariaLabel }: QuadrantScatterProps) {
  const name = proseName(sheet.meta.country);
  const M = { top: 20, right: 20, bottom: 46, left: 52 };
  const cm = sheet.summary.country_median;
  const medEo = cm?.eo.v ?? null;
  const medEs = cm?.es.v ?? null;
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!points.length) return null;
      const H = scatterHeight(width);
      const narrow = width < 640;
      const iw = width - M.left - M.right;
      const ih = H - M.top - M.bottom;
      const px = (x: number) => M.left + (x / 100) * iw;
      const py = (y: number) => M.top + (1 - y / 100) * ih;
      const r = scatterRadius(points.length);
      // point labels: the selected sector first (always), then priority and top five, then the rest, each
      // where it fits (with up to 58 dots, labels that always print would pile up on neighbouring stages)
      const sel = points.filter((p) => p.key === selected);
      const must = points.filter((p) => p.key !== selected && (p.priority || p.top5));
      const rest = points.filter((p) => p.key !== selected && !(p.priority || p.top5));
      const cands = [...sel, ...must, ...(narrow ? [] : rest)];
      const boxes = cands.map((p) => ({ x: px(p.x), y: py(p.y) - (p.key === selected ? 16 : 13), w: textWidth(p.short, p.key === selected ? 11 : 10) + 4, h: 12 }));
      const keep = declutter(boxes, cands.map((_, j) => j));
      const lab = cands.filter((p, j) => p.key !== selected && keep.has(j));
      const fill = (d: ScatterPoint) => (d.top5 ? MARK : 'none');
      const ticks = [0, 25, 50, 75, 100];
      return {
        width,
        height: H,
        marginTop: M.top,
        marginRight: M.right,
        marginBottom: M.bottom,
        marginLeft: M.left,
        x: { domain: [0, 100], ticks, label: `Economic Opportunity: share of economies ${name} beats (%) →`, labelAnchor: 'right', grid: true },
        y: { domain: [0, 100], ticks, label: `↑ Energy Security: share of economies ${name} beats (%)`, labelAnchor: 'top', grid: true },
        symbol: { domain: [...SUPPLY_STAGES], range: SUPPLY_STAGES.map((s) => STAGE_SYMBOL[s]) },
        marks: [
          Plot.frame({ stroke: 'rgb(255 255 255 / 0.12)' }),
          // the country's median sector on each axis: the corners split here
          ...(medEo !== null ? [Plot.ruleX([medEo], { stroke: PAPER_MUTE, strokeWidth: 1, strokeDasharray: '4,3' })] : []),
          ...(medEs !== null ? [Plot.ruleY([medEs], { stroke: PAPER_MUTE, strokeWidth: 1, strokeDasharray: '4,3' })] : []),
          Plot.dot(points, { x: 'x', y: 'y', symbol: 'stage', r, fill, stroke: MARK, strokeWidth: 1.5 }),
          Plot.text(lab, { x: 'x', y: 'y', text: 'short', dy: -13, fill: '#d1d5db', fontSize: 10 }),
          Plot.dot(sel, { x: 'x', y: 'y', symbol: 'stage', r: r + 3, fill: 'none', stroke: SIGNAL, strokeWidth: 2.5 }),
          Plot.text(sel, { x: 'x', y: 'y', text: 'short', dy: -16, fill: '#ffffff', fontSize: 11, fontWeight: 600 }),
          Plot.dot(points, Plot.pointer({ x: 'x', y: 'y', r: 10, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 16 })),
        ],
      };
    },
    // M is a constant
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [points, selected, sheet, name, medEo, medEs],
  );
  const [ref, w] = useWidth<HTMLDivElement>();
  const height = scatterHeight(w || 440);
  const tip = (p: ScatterPoint) => {
    const c = p.cell;
    const es = c.es!;
    const eo = c.eo!;
    const few = (eo.n.v ?? 99) < 10 ? ` (only ${eo.n.d} economies)` : '';
    const vs = (high: boolean) => (high ? 'at or above' : 'below');
    return (
      <Tip
        value={quadrantWord(p.quadrant)}
        title={c.label}
        lines={[
          `${cap(name)}: Economic Opportunity ${vs(p.x >= (medEo ?? 0))} its median sector (${cm?.eo.d ?? '–'}), Energy Security ${vs(p.y >= (medEs ?? 0))} it (${cm?.es.d ?? '–'})`,
          `Economic Opportunity ${eo.d}: higher than ${eo.pct.d} of the other ${eo.n_others.d} economies${few} (median economy ${eo.med.d}; ${eo.side === 'above' ? 'above' : 'below'} it)`,
          `Energy Security ${es.d}: higher than ${es.pct.d} of them (median economy ${es.med.d}; ${es.side === 'above' ? 'above' : 'below'} it)`,
          c.si.in_sample ? `Strategic Index rank ${c.si.rank.d} of ${c.si.n.d}` : `Not in the Strategic Index${c.si.exclusion ? `: ${c.si.exclusion}` : ''}`,
        ]}
      />
    );
  };
  const corner = (id: (typeof CORNERS)[number]['id'], right: boolean) => {
    const q = QUADRANTS.find((x) => x.id === id)!;
    return (
      <div className={right ? 'text-right' : ''}>
        <span className="block text-[11px] font-semibold uppercase tracking-wider text-paper">{quadrantWord(id)}</span>
        <span className="block text-[11px] leading-snug text-paper-mute">
          {q.meaning} <span className="whitespace-nowrap">({q.posture})</span>
        </span>
      </div>
    );
  };
  return (
    <div ref={ref}>
      <div className="grid grid-cols-2 gap-4 pb-1" style={{ paddingLeft: M.left, paddingRight: M.right }} aria-hidden="true">
        {corner('watch', false)}
        {corner('grow', true)}
      </div>
      <PlotFigure<ScatterPoint> build={build} height={height} ariaLabel={ariaLabel} tooltip={tip} onSelect={(p) => onSelect(p.key)} />
      <div className="grid grid-cols-2 gap-4 pt-1" style={{ paddingLeft: M.left, paddingRight: M.right }} aria-hidden="true">
        {corner('secure', false)}
        {corner('upgrade', true)}
      </div>
    </div>
  );
}

function Sym({ shape, filled, stroke = MARK }: { shape: (typeof STAGE_SYMBOL)[SupplyStage]; filled?: boolean; stroke?: string }) {
  const f = filled ? MARK : 'none';
  // the same shapes Plot draws (d3 symbols), at a legend size (r = 5)
  const d = d3Symbol(D3_SYMBOL[shape], Math.PI * 25)() ?? '';
  return (
    <svg width="14" height="14" viewBox="-7 -7 14 14" aria-hidden="true" className="inline-block">
      <path d={d} fill={f} stroke={stroke} strokeWidth="1.5" />
    </svg>
  );
}

/** The scatter's legend: the six stage shapes, filled vs hollow, the selection ring and the country's
 *  median-sector lines, in words. */
export function ScatterLegend({ name }: { name: string }) {
  const items: [React.ReactNode, string][] = [
    ...SUPPLY_STAGES.map((s): [React.ReactNode, string] => [<Sym key={s} shape={STAGE_SYMBOL[s]} />, STAGE_LABEL[s]]),
    [<Sym key="f" shape="circle" filled />, `Filled: among ${poss(name)} top five Strategic Index sectors`],
    [<Sym key="s" shape="circle" stroke={SIGNAL} />, 'Selected'],
    [
      <svg key="l" width="20" height="4" aria-hidden="true" className="inline-block">
        <line x1="0" y1="2" x2="20" y2="2" stroke={PAPER_MUTE} strokeWidth="1" strokeDasharray="4,3" />
      </svg>,
      `Dashed lines: ${poss(name)} median sector`,
    ],
  ];
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="How to read the dots">
      {items.map(([k, t]) => (
        <li key={t} className="inline-flex items-center gap-1.5">
          {k}
          {t}
        </li>
      ))}
    </ul>
  );
}
