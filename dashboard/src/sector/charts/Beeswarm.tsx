// Beeswarm (spec_sector.md 4.4 card 2, 4.6 "Distribution", 5.3): every economy as a dot (r 4) dodged
// along y; group fill, "rest of world" as hollow gray rings; old observations at 50% opacity; the
// highlighted economy ringed orange and labelled, plus the points the view asks to label (China, the
// largest coalition member); median ticks with their words. Reported zeros are listed in a separate
// "0" strip under the swarm (they have no place on a log axis and would hide the origin).

import { useCallback, useMemo } from 'react';
import * as Plot from '@observablehq/plot';
import { PlotFigure, Tip } from '../../charts/PlotFigure.tsx';
import { compactSi, logTicks, textWidth } from '../../charts/theme.ts';
import { GROUP_COLOR, INK, PAPER, PAPER_DIM, PAPER_MUTE, SIGNAL } from '../../theme/palette.ts';
import { groupLabel } from './color.ts';
import type { BeeswarmPoint, BeeswarmProps } from './types.ts';

export function Beeswarm({ points, medians, log, xLabel, ariaLabel, selected, onSelect, tickFormat, zeroStrip = true }: BeeswarmProps) {
  const { swarm, zeros } = useMemo(() => {
    // zeros go to the strip for additive measures (and always on a log axis); a bounded 0 is a dot
    const isZero = (p: BeeswarmPoint) => p.state === 'zero' || (Number.isFinite(p.value) && p.value === 0 && (zeroStrip || !!log));
    const zeros = points.filter(isZero);
    const swarm = points.filter((p) => Number.isFinite(p.value) && !isZero(p) && (!log || p.value > 0));
    return { swarm, zeros };
  }, [points, log, zeroStrip]);
  const height = Math.max(140, Math.min(320, 60 + Math.ceil(swarm.length / 6) * 8));
  const build = useCallback(
    (width: number): Plot.PlotOptions | null => {
      if (!swarm.length) return null;
      const dodge = { x: 'value', r: 4, anchor: 'middle' as const, padding: 1 };
      const xs = swarm.map((p) => p.value);
      const lo = Math.min(...xs);
      const hi = Math.max(...xs);
      // log: ticks and grid at the decades only, each with its own SI step (no label on every minor tick)
      const logX = log ? { ticks: logTicks(lo, hi, Math.max(3, Math.floor((width - 32) / 90))), tickFormat: tickFormat ?? compactSi } : null;
      // point labels (the highlight first, then the view's picks): a label that would overlap one
      // already placed moves below its dot; if that collides too, it is left to the tooltip
      const px = (x: number) => (log ? (Math.log10(x) - Math.log10(lo)) / (Math.log10(hi) - Math.log10(lo) || 1) : (x - Math.min(0, lo)) / (hi - Math.min(0, lo) || 1)) * (width - 32) + 16;
      const want = swarm.filter((p) => p.labelled || p.id === selected).sort((a, z) => Number(z.id === selected) - Number(a.id === selected));
      const placeAbove = new Set<string>();
      const placeBelow = new Set<string>();
      const rows: { above: [number, number][]; below: [number, number][] } = { above: [], below: [] };
      const free = (row: [number, number][], a: number, b: number) => !row.some(([x0, x1]) => a < x1 && b > x0);
      for (const p of want) {
        const w = textWidth(p.label, 11) + 6;
        const a = px(p.value) - w / 2;
        const b = a + w;
        if (free(rows.above, a, b)) {
          rows.above.push([a, b]);
          placeAbove.add(p.id);
        } else if (free(rows.below, a, b)) {
          rows.below.push([a, b]);
          placeBelow.add(p.id);
        }
      }
      const fill = (p: BeeswarmPoint) => (p.group === 'rest' ? 'none' : GROUP_COLOR[p.group]);
      const stroke = (p: BeeswarmPoint) => (p.group === 'rest' ? GROUP_COLOR.rest : INK[800]);
      const ms = medians.filter((m) => Number.isFinite(m.value) && (!log || m.value > 0));
      return {
        marginTop: 26,
        marginLeft: 16,
        marginRight: 16,
        x: log ? { type: 'log', label: xLabel, grid: true, ...logX } : { type: 'linear', label: xLabel, grid: true, tickFormat, nice: true },
        y: { axis: null },
        marks: [
          Plot.tickX(ms, { x: 'value', stroke: PAPER_MUTE, strokeDasharray: '3 3' }),
          Plot.text(ms, { x: 'value', frameAnchor: 'top', text: 'label', dy: -16, fill: PAPER_MUTE, fontSize: 10, textAnchor: 'middle' }),
          // every mark uses the same dodge over the same data, so the rings and labels sit on their dots
          Plot.dot(swarm, Plot.dodgeY({ ...dodge, fill, stroke, strokeWidth: (p: BeeswarmPoint) => (p.group === 'rest' ? 1.5 : 1), fillOpacity: (p: BeeswarmPoint) => (p.state === 'old' ? 0.5 : 1), strokeOpacity: (p: BeeswarmPoint) => (p.state === 'old' ? 0.5 : 1) } as Plot.DotOptions)),
          Plot.dot(swarm, Plot.dodgeY({ ...dodge, fill: 'none', stroke: (p: BeeswarmPoint) => (p.id === selected ? SIGNAL : 'none'), strokeWidth: 2.5 } as Plot.DotOptions)),
          Plot.text(swarm, Plot.dodgeY({ ...dodge, text: (p: BeeswarmPoint) => (placeAbove.has(p.id) ? p.label : ''), dy: -9, fill: (p: BeeswarmPoint) => (p.id === selected ? PAPER : PAPER_DIM), fontSize: 11, fontWeight: 500 } as Plot.TextOptions)),
          Plot.text(swarm, Plot.dodgeY({ ...dodge, text: (p: BeeswarmPoint) => (placeBelow.has(p.id) ? p.label : ''), dy: 11, fill: (p: BeeswarmPoint) => (p.id === selected ? PAPER : PAPER_DIM), fontSize: 11, fontWeight: 500 } as Plot.TextOptions)),
          Plot.dot(swarm, Plot.pointer(Plot.dodgeY({ ...dodge, fill: 'none', stroke: SIGNAL, strokeWidth: 2, maxRadius: 16 } as Plot.DotOptions))),
        ],
      };
    },
    [swarm, medians, log, xLabel, selected, tickFormat],
  );
  return (
    <div>
      {swarm.length > 0 ? (
        <PlotFigure<BeeswarmPoint>
          build={build}
          height={height}
          ariaLabel={ariaLabel}
          onSelect={onSelect}
          tooltip={(p) => <Tip value={p.display} title={p.label} lines={[groupLabel(p.group), p.state === 'old' ? 'older observation' : null]} />}
        />
      ) : (
        <p className="py-6 text-center text-sm text-paper-mute">No economies with a value to place.</p>
      )}
      {zeros.length > 0 && (
        <p className="mt-2 border-t hairline pt-2 text-xs text-paper-mute" data-testid="zero-strip">
          <span className="mr-2 inline-flex items-center gap-1 font-medium text-paper-dim">
            <span aria-hidden="true" className="inline-block h-2.5 w-2.5" style={{ background: '#4b5563' }} />0
          </span>
          Reported zero ({zeros.length}): {zeros.map((z) => z.label).join(', ')}
        </p>
      )}
    </div>
  );
}
