// SmallMultiples (spec_sector.md 4.3 S7 iii, S10 c, 4.6 4b, 5.3): a CSS grid of memoised PlotFigures,
// one per panel, with one shared y domain (the default) or independent scales. A panel without data
// is a quiet tile with its reason, not left out. One y axis per panel, never two.

import { memo, useCallback, useMemo } from 'react';
import { PlotFigure } from '../../charts/PlotFigure.tsx';
import type { SmallMultiplePanel, SmallMultiplesProps } from './types.ts';

/** The shared [min, max] over every panel (min clamped at 0 for non-negative data). */
export function sharedDomain<R>(panels: readonly SmallMultiplePanel<R>[], yValue: (r: R) => number): [number, number] | null {
  let lo = Infinity;
  let hi = -Infinity;
  for (const p of panels) {
    for (const r of p.rows) {
      const v = yValue(r);
      if (!Number.isFinite(v)) continue;
      if (v < lo) lo = v;
      if (v > hi) hi = v;
    }
  }
  if (!Number.isFinite(lo)) return null;
  return [Math.min(0, lo), hi > lo ? hi : lo + 1];
}

interface PanelProps<R> {
  panel: SmallMultiplePanel<R>;
  yDomain: [number, number] | null;
  height: number;
  render: SmallMultiplesProps<R>['render'];
  ariaLabel: string;
  tooltip?: SmallMultiplesProps<R>['tooltip'];
}

function PanelInner<R>({ panel, yDomain, height, render, ariaLabel, tooltip }: PanelProps<R>) {
  const build = useCallback((width: number) => render(panel, { width, height, yDomain }), [render, panel, height, yDomain]);
  return (
    <figure className="min-w-0 border hairline bg-ink-850 p-3" aria-label={ariaLabel}>
      <figcaption className="mb-1">
        <span className="block truncate text-sm font-medium text-paper" title={panel.title}>
          {panel.title}
        </span>
        {panel.sub && <span className="block text-xs text-paper-mute">{panel.sub}</span>}
      </figcaption>
      {panel.empty || !panel.rows.length ? (
        <div className="flex items-center justify-center text-center text-xs text-paper-mute" style={{ height }}>
          {panel.empty ?? 'No data'}
        </div>
      ) : (
        <PlotFigure<R> build={build} height={height} ariaLabel={ariaLabel} tooltip={tooltip} />
      )}
    </figure>
  );
}

const Panel = memo(PanelInner) as typeof PanelInner;

export function SmallMultiples<R>({ panels, shared, yValue, render, height = 140, minPanelWidth = 220, ariaLabel, tooltip }: SmallMultiplesProps<R>) {
  const domain = useMemo(() => (shared ? sharedDomain(panels, yValue) : null), [shared, panels, yValue]);
  return (
    <div className="grid gap-3" style={{ gridTemplateColumns: `repeat(auto-fill, minmax(min(100%, ${minPanelWidth}px), 1fr))` }} data-testid="small-multiples">
      {panels.map((p) => (
        <Panel key={p.id} panel={p} yDomain={domain} height={height} render={render} ariaLabel={ariaLabel(p)} tooltip={tooltip} />
      ))}
    </div>
  );
}
