// The coverage comparison of a compact: shock (static) -> existing relationships -> trade compact ->
// trade + investment response, as columns in % of the demand scope's demand (Observable Plot). One
// measure, so one hue (the shock step gray as the starting point), each column labelled with its value
// and the step-to-step change; hover gives the step's meaning. An optional reference (the global optimum:
// coverage if every economy coordinated) is a dashed white rule across the columns, labelled at the left.

import { useCallback } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { isNum } from '../../lib/format.ts';
import { PAPER_DIM } from '../../theme/palette.ts';
import type { CoverageStep } from '../derive/coverage.ts';
import { fmtPp, fmtShare } from '../derive/format.ts';
import { STEP_COLOR } from './CoverageStrip.tsx';

interface Bar extends CoverageStep {
  v: number;
  delta: number | null;
}

export const REFERENCE_COLOR = '#ffffff';

export function CoverageBars({ steps, ariaLabel, reference = null, referenceLabel = '' }: { steps: CoverageStep[]; ariaLabel: string; reference?: number | null; referenceLabel?: string }) {
  const bars: Bar[] = steps.map((s, i) => {
    const prev = i > 0 ? steps[i - 1].value : null;
    return { ...s, v: isNum(s.value) ? s.value : 0, delta: isNum(s.value) && isNum(prev) ? s.value - prev : null };
  });
  const domain = steps.map((s) => s.label);
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!bars.some((b) => isNum(b.value))) return null;
      const narrow = width < 520;
      const known = bars.filter((b) => isNum(b.value));
      return {
        marginLeft: 48,
        marginBottom: narrow ? 56 : 40,
        x: { domain, label: null, tickFormat: (d: string) => (narrow ? (steps.find((s) => s.label === d)?.short ?? d) : d), padding: 0.35 },
        y: { domain: [0, 1], label: '↑ Demand served after the shock (%)', tickFormat: (v: number) => `${Math.round(v * 100)}`, grid: true },
        marks: [
          Plot.ruleY([0], { stroke: 'rgb(255 255 255 / 0.22)' }),
          ...(isNum(reference)
            ? [
                Plot.ruleY([reference], { stroke: REFERENCE_COLOR, strokeOpacity: 0.8, strokeWidth: 1.5, strokeDasharray: '6,4' }),
                Plot.text([reference], {
                  y: (d: number) => d,
                  // at the left, above the shock column (the lowest), clear of the response columns' labels
                  frameAnchor: 'left',
                  textAnchor: 'start',
                  dx: 4,
                  dy: -7,
                  text: (d: number) => `${referenceLabel}: ${fmtShare(d, 1)}`,
                  fill: '#ffffff',
                  fontSize: 10,
                  stroke: '#1f2937',
                  strokeWidth: 3,
                  paintOrder: 'stroke',
                }),
              ]
            : []),
          // the columns over the reference rule
          Plot.barY(bars, { x: 'label', y: 'v', fill: (d: Bar) => (d.key === 'static' ? STEP_COLOR.static : STEP_COLOR.response), insetLeft: 1, insetRight: 1 }),
          Plot.text(known, { x: 'label', y: 'v', text: (d: Bar) => fmtShare(d.value, 1), dy: -8, fill: '#ffffff', fontSize: 12, fontWeight: 600, stroke: '#1f2937', strokeWidth: 4, paintOrder: 'stroke' }),
          Plot.text(
            known.filter((d) => d.delta !== null),
            { x: 'label', y: 'v', text: (d: Bar) => fmtPp(d.delta), dy: -22, fill: PAPER_DIM, fontSize: 10, stroke: '#1f2937', strokeWidth: 3, paintOrder: 'stroke' },
          ),
          Plot.barY(bars, Plot.pointerX({ x: 'label', y: 'v', fill: 'none', stroke: '#f58229', strokeWidth: 2 })),
        ],
      };
    },
    // bars and domain are derived from steps
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [steps, reference, referenceLabel],
  );
  return (
    <PlotFigure<Bar>
      build={build}
      height={300}
      ariaLabel={ariaLabel}
      tooltip={(d) => <Tip value={fmtShare(d.value)} title={d.label} lines={[`of demand served: ${d.meaning}`, d.delta !== null ? `${fmtPp(d.delta)} on the step before` : null]} />}
    />
  );
}
