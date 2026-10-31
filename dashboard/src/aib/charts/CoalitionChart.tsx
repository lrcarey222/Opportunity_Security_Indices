// How resilience changes as countries join: the greedy coalition path of a compact (Observable Plot).
// x = members in the coalition (step 0: the demand anchors), each tick labelled with the economy added
// at that step; y = coverage of the anchors' demand: the minimum over the compact's nodes (solid, with
// dots) and the mean (dashed). The coverage targets are dotted rules; the first step reaching each
// target is ringed and named ("90% reached"; targets reached at the same step share one label). The
// path is a heuristic; the exact minimum coalitions are listed beside the chart.
// Hover gives the step with names.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { PAPER_MUTE, SIGNAL } from '../../theme/palette.ts';
import { curveFloor, targetLabel, type CurvePoint, type TargetMark } from '../derive/coalition.ts';
import { fmtShare } from '../derive/format.ts';

export const CURVE_COLOR = { min: '#1889b0', mean: '#9ca3af', target: 'rgb(255 255 255 / 0.45)' } as const;

export function CoalitionChart({ points, marks, ariaLabel }: { points: CurvePoint[]; marks: TargetMark[]; ariaLabel: string }) {
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      const pts = points.filter((p) => p.min !== null);
      if (!pts.length) return null;
      const floor = curveFloor(points);
      const nMin = Math.min(...points.map((p) => p.n));
      const nMax = Math.max(...points.map((p) => p.n));
      const span = Math.max(1, nMax - nMin);
      // one tick per member count while they fit (about 44 px each); each names the economy added there
      // (codes; names are in the tooltip and the table)
      const step = Math.max(1, Math.ceil((span + 1) / Math.max(2, Math.floor((width - 56 - 96) / 44))));
      const ticks = Array.from({ length: span + 1 }, (_, i) => nMin + i).filter((v) => (v - nMin) % step === 0 || v === nMax);
      const byN = new Map(points.map((p) => [p.n, p]));
      const tick = (v: number) => {
        const p = byN.get(v);
        return p ? [String(v), p.added ?? 'anchors'].join('\n') : String(v);
      };
      // targets reached at the same step share one ring and one label
      const reached: { point: CurvePoint; text: string }[] = [];
      for (const m of marks) {
        if (!m.point) continue;
        const r = reached.find((x) => x.point.step === m.point!.step);
        if (r) r.text = r.text.replace(' reached', `, ${targetLabel(m.target)} reached`);
        else reached.push({ point: m.point, text: `${targetLabel(m.target)} reached` });
      }
      return {
        marginLeft: 56,
        marginRight: 96,
        marginBottom: 58,
        x: { domain: [nMin - 0.3, nMax + 0.3], ticks, tickFormat: tick, label: 'Members in the coalition (economy added) →', labelAnchor: 'right', labelOffset: 52 },
        y: { domain: [floor, 1], label: "↑ Coverage of the anchors' demand (%)", tickFormat: (v: number) => `${Math.round(v * 100)}`, grid: true },
        marks: [
          Plot.ruleY(marks.map((m) => m.target), { stroke: CURVE_COLOR.target, strokeDasharray: '2,3' }),
          Plot.text(marks, { x: nMax + 0.3, y: 'target', text: (m: TargetMark) => `${targetLabel(m.target)} target`, textAnchor: 'start', dx: 6, fill: PAPER_MUTE, fontSize: 10 }),
          Plot.line(points.filter((p) => p.mean !== null), { x: 'n', y: 'mean', stroke: CURVE_COLOR.mean, strokeWidth: 2, strokeDasharray: '5,4' }),
          Plot.line(pts, { x: 'n', y: 'min', stroke: CURVE_COLOR.min, strokeWidth: 2 }),
          Plot.dot(pts, { x: 'n', y: 'min', r: 4, fill: CURVE_COLOR.min, stroke: '#1f2937', strokeWidth: 1.5 }),
          // the target coalitions: a white ring and the target under the dot
          Plot.dot(reached, { x: (m: { point: CurvePoint }) => m.point.n, y: (m: { point: CurvePoint }) => m.point.min, r: 8, fill: 'none', stroke: '#ffffff', strokeWidth: 1.5 }),
          Plot.text(reached, {
            x: (m: { point: CurvePoint }) => m.point.n,
            y: (m: { point: CurvePoint }) => m.point.min,
            text: 'text',
            dy: 17,
            fill: '#ffffff',
            fontSize: 10,
            fontWeight: 600,
            stroke: '#1f2937',
            strokeWidth: 3,
            paintOrder: 'stroke',
          }),
          Plot.dot(pts, Plot.pointerX({ x: 'n', y: 'min', r: 7, fill: 'none', stroke: SIGNAL, strokeWidth: 2 })),
        ],
      };
    },
    [points, marks],
  );
  return (
    <PlotFigure<CurvePoint>
      build={build}
      height={340}
      ariaLabel={ariaLabel}
      tooltip={(p) => (
        <Tip
          value={`${fmtShare(p.min)} minimum`}
          title={p.step === 0 ? `Step 0: the demand anchors (${p.n})` : `Step ${p.step}: ${p.addedName} joins (${p.n} members)`}
          lines={[`${fmtShare(p.mean)} mean coverage over the supply-chain steps`, ...marks.filter((m) => m.point?.step === p.step).map((m) => `First step of the path to reach the ${targetLabel(m.target)} target`)]}
        />
      )}
    />
  );
}
