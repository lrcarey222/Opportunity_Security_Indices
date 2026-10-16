// The specialization-resilience frontier of one node (Observable Plot): x = the supplier-share limit (25 /
// 40 / 60 / 100%), y = 0-100%; three lines with dots: coverage, coverage if the largest supplier is also
// lost (a second shock) and the specialization score, each in its own colour and dash, labelled at the
// right end. A dotted rule marks the 80% acceptable coverage the second-shock reading uses; vertical
// rules mark the least diversification that keeps it (minRequiredDiversification) and the tightest limit
// that costs at most 2 points of coverage (tightestCostless). Hover gives the values at a limit.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { isNum } from '../../lib/format.ts';
import { PAPER_MUTE, SIGNAL } from '../../theme/palette.ts';
import { capLabel, FRONTIER_SERIES, frontierPoints, type FrontierRow } from '../derive/frontier.ts';
import { fmtShare } from '../derive/format.ts';

/** The coverage the second-shock reading asks for (the exporter's acceptable_coverage default). */
export const ACCEPTABLE_COVERAGE = 0.8;
export const REQUIRED_COLOR = '#ffffff';
export const COSTLESS_COLOR = '#9ca3af';

type Pt = ReturnType<typeof frontierPoints>[number];

export function FrontierChart({
  rows, required, costless, ariaLabel,
}: {
  rows: FrontierRow[];
  required: number | null;
  costless: number | null;
  ariaLabel: string;
}) {
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      const pts = frontierPoints(rows);
      if (!pts.length) return null;
      const caps = rows.map((r) => r.cap);
      const lo = Math.min(...caps);
      const ends = FRONTIER_SERIES.map((s) => {
        const last = [...pts].filter((p) => p.key === s.key).sort((a, b) => b.cap - a.cap)[0];
        return last ? { ...last, label: s.label, labelY: last.value } : null;
      })
        .filter((x): x is Pt & { label: string; labelY: number } => x !== null)
        .sort((a, b) => b.value - a.value);
      // keep the end labels apart: each at least two lines (about 0.12 of the axis) below the one above
      for (let i = 1; i < ends.length; i++) ends[i].labelY = Math.min(ends[i].labelY, ends[i - 1].labelY - 0.12);
      const narrow = width < 560;
      const vrules = [
        ...(isNum(required) ? [{ cap: required, color: REQUIRED_COLOR, dash: null as string | null, text: 'least diversification for resilience' }] : []),
        ...(isNum(costless) ? [{ cap: costless, color: COSTLESS_COLOR, dash: '2,3' as string | null, text: 'tightest costless limit' }] : []),
      ];
      return {
        marginLeft: 48,
        marginRight: narrow ? 16 : 150,
        x: { domain: [Math.max(0, lo - 0.05), 1.03], ticks: caps, tickFormat: (v: number) => `${Math.round(v * 100)}%`, label: 'Supplier-share limit (no exporter above this share of supply) →', labelAnchor: 'right' },
        y: { domain: [0, 1], label: '↑ Coverage (% of demand); specialization score × 100', tickFormat: (v: number) => `${Math.round(v * 100)}`, grid: true },
        marks: [
          Plot.ruleY([ACCEPTABLE_COVERAGE], { stroke: 'rgb(255 255 255 / 0.4)', strokeDasharray: '2,3' }),
          Plot.text([ACCEPTABLE_COVERAGE], { y: (d: number) => d, frameAnchor: 'left', textAnchor: 'start', dx: 4, dy: -7, text: () => `${Math.round(ACCEPTABLE_COVERAGE * 100)}% after a second shock`, fill: PAPER_MUTE, fontSize: 10 }),
          ...vrules.map((r) => Plot.ruleX([r.cap], { stroke: r.color, strokeWidth: 1.5, strokeDasharray: r.dash ?? undefined })),
          ...vrules.map((r, i) =>
            Plot.text([r.cap], {
              x: (d: number) => d,
              frameAnchor: 'bottom',
              dy: -8 - i * 13,
              dx: r.cap > 0.8 ? -4 : 4,
              textAnchor: r.cap > 0.8 ? 'end' : 'start',
              text: () => r.text,
              fill: r.color === REQUIRED_COLOR ? '#ffffff' : PAPER_MUTE,
              fontSize: 10,
              stroke: '#1f2937',
              strokeWidth: 3,
              paintOrder: 'stroke',
            }),
          ),
          ...FRONTIER_SERIES.map((s) =>
            Plot.line(
              pts.filter((p) => p.key === s.key),
              { x: 'cap', y: 'value', stroke: s.color, strokeWidth: 2, strokeDasharray: s.dash ?? undefined },
            ),
          ),
          ...FRONTIER_SERIES.map((s) => Plot.dot(pts.filter((p) => p.key === s.key), { x: 'cap', y: 'value', r: 3.5, fill: s.color, stroke: '#1f2937', strokeWidth: 1 })),
          ...(narrow ? [] : [Plot.text(ends, { x: 'cap', y: 'labelY', text: 'label', textAnchor: 'start', dx: 10, fill: '#d1d5db', fontSize: 10, lineWidth: 14 })]),
          Plot.dot(pts, Plot.pointer({ x: 'cap', y: 'value', r: 7, fill: 'none', stroke: SIGNAL, strokeWidth: 2 })),
        ],
      };
    },
    [rows, required, costless],
  );
  const tip = (p: Pt) => {
    const r = rows.find((x) => x.cap === p.cap);
    const s = FRONTIER_SERIES.find((x) => x.key === p.key);
    return (
      <Tip
        value={fmtShare(p.value)}
        title={`${s?.label ?? p.key}, supplier limit ${capLabel(p.cap)}`}
        lines={r ? [`Coverage ${fmtShare(r.coverage)}; after a second shock ${fmtShare(r.secondShock)}; specialization ${fmtShare(r.specialization, 0)}`, r.largestName ? `Largest supplier ${r.largestName} (${fmtShare(r.largestShare)})` : null] : []}
      />
    );
  };
  return <PlotFigure<Pt> build={build} height={300} ariaLabel={ariaLabel} tooltip={tip} />;
}
