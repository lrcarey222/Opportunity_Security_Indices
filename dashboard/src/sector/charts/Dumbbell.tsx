// Dumbbell (spec_sector.md 4.2c, 4.3 S5, S6, S8, 4.7d, 5.3): two values per row joined by a link in
// the row's group colour; the first value hollow (the earlier year, or the projection), the second
// filled with a 2 px surface ring. A missing value is left out and its words ("no 5 GW tranche") stay
// in the text column. The legend says which dot is which.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { labelMargin, textWidth } from '../../charts/theme.ts';
import { INK, PAPER, PAPER_DIM, PAPER_MUTE, SIGNAL } from '../../theme/palette.ts';
import { groupColor } from './color.ts';
import type { DumbbellProps, DumbbellRow } from './types.ts';

const ROW_PX = 24;
const AXIS_PX = 40;

const ok = (v: number, log?: boolean) => Number.isFinite(v) && (!log || v > 0);
const both = (r: DumbbellRow) => `${r.aLabel} → ${r.bLabel}`;

export function Dumbbell({ rows, xLabel, aName, bName, ariaLabel, selected, onSelect, tickFormat, log, rule }: DumbbellProps) {
  const height = rows.length * ROW_PX + AXIS_PX;
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!rows.length) return null;
      const ids = rows.map((r) => r.id);
      const ml = labelMargin(rows.map((r) => r.label), { max: Math.min(200, Math.floor(width * 0.34)) });
      const textW = Math.max(...rows.map((r) => textWidth(both(r), 11))) + 10;
      const showText = textW < width * 0.34;
      const color = (r: DumbbellRow) => groupColor(r.group);
      const linked = rows.filter((r) => ok(r.a, log) && ok(r.b, log));
      const aPts = rows.filter((r) => ok(r.a, log));
      const bPts = rows.filter((r) => ok(r.b, log));
      const xs = rows.flatMap((r) => [r.a, r.b]).filter((v) => ok(v, log));
      if (rule && ok(rule.x, log)) xs.push(rule.x);
      const sel = rows.filter((r) => r.id === selected);
      return {
        marginLeft: ml,
        marginRight: showText ? textW + 6 : 16,
        marginTop: 8,
        x: log ? { type: 'log', label: xLabel, grid: true, tickFormat } : { label: xLabel, grid: true, tickFormat, nice: true, domain: [Math.min(0, ...xs), Math.max(0, ...xs) || 1] },
        y: { domain: ids, axis: null },
        marks: [
          Plot.text(rows.filter((r) => r.id !== selected), { y: 'id', text: 'label', frameAnchor: 'left', dx: -8, textAnchor: 'end', fill: PAPER_DIM, fontSize: 12 }),
          Plot.text(sel, { y: 'id', text: 'label', frameAnchor: 'left', dx: -8, textAnchor: 'end', fill: PAPER, fontSize: 12, fontWeight: 700 }),
          rule && ok(rule.x, log) ? Plot.ruleX([rule.x], { stroke: PAPER_MUTE, strokeDasharray: '3 3' }) : null,
          rule && ok(rule.x, log) ? Plot.text([rule], { x: 'x', frameAnchor: 'top', text: 'label', dy: -2, fill: PAPER_MUTE, fontSize: 10, textAnchor: 'middle' }) : null,
          Plot.link(linked, { x1: 'a', x2: 'b', y1: 'id', y2: 'id', stroke: color, strokeWidth: 2 }),
          Plot.dot(aPts, { x: 'a', y: 'id', r: 4.5, fill: INK[800], stroke: color, strokeWidth: 2 }),
          Plot.dot(bPts, { x: 'b', y: 'id', r: 4.5, fill: color, stroke: INK[800], strokeWidth: 2 }),
          sel.length ? Plot.dot(sel.filter((r) => ok(r.b, log)), { x: 'b', y: 'id', r: 7, fill: 'none', stroke: SIGNAL, strokeWidth: 2 }) : null,
          showText ? Plot.text(rows, { y: 'id', text: both, frameAnchor: 'right', dx: 10, textAnchor: 'start', fill: PAPER_MUTE, fontSize: 11 }) : null,
          Plot.dot(
            rows.filter((r) => ok(r.b, log) || ok(r.a, log)),
            Plot.pointerY({ x: (r: DumbbellRow) => (ok(r.b, log) ? r.b : r.a), y: 'id', r: 8, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 400 }),
          ),
        ],
      };
    },
    [rows, xLabel, selected, tickFormat, log, rule],
  );
  if (!rows.length) return <p className="py-6 text-center text-sm text-paper-mute">No rows for this selection.</p>;
  return (
    <div>
      <PlotFigure<DumbbellRow>
        build={build}
        height={height}
        ariaLabel={ariaLabel}
        onSelect={onSelect}
        tooltip={(r) => <Tip title={r.label} lines={[`${aName}: ${r.aLabel}`, `${bName}: ${r.bLabel}`]} />}
      />
      <ul className="mt-2 flex flex-wrap gap-x-4 gap-y-1 text-xs text-paper-dim" aria-label="Legend">
        <li className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-2.5 w-2.5 rounded-full border-2 border-paper-dim" />
          {aName}
        </li>
        <li className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-block h-2.5 w-2.5 rounded-full bg-paper-dim" />
          {bName}
        </li>
      </ul>
    </div>
  );
}
