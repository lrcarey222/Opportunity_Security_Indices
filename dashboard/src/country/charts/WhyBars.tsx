// WhyBars (spec_country.md A4.2 S4): what pulls one pillar of a sector down or up, compared with the
// middle economy of the sector. One horizontal bar per category, value = impact = the category's share of
// the pillar's weight x (its score - the median score in the sector), centred at zero: orange to the left
// ("below the middle economy"), blue to the right ("above"). Filled-in (imputed) categories are hollow
// and say "filled-in data". HTML, so it reads as a list to assistive technology; the side of zero and the
// words carry the meaning, never the colour alone.

import { TRI_HIGH, TRI_LOW } from '../../theme/palette.ts';
import type { Driver } from '../contract.ts';

export interface WhyBarsProps {
  drivers: Driver[];
  /** the largest |impact| on the panel (both pillars share one scale) */
  max: number;
  label: string;
}

export function WhyBars({ drivers, max, label }: WhyBarsProps) {
  if (!drivers.length) return <p className="text-sm text-paper-mute">No category breakdown for this score.</p>;
  const m = max > 0 ? max : 1;
  return (
    <ul className="space-y-2.5" aria-label={label}>
      {drivers.map((d) => {
        const v = d.impact.v ?? 0;
        const w = Math.min(50, (50 * Math.abs(v)) / m);
        const neg = v < 0;
        const color = neg ? TRI_LOW : TRI_HIGH;
        const words = v === 0 ? 'at the middle economy' : neg ? 'below the middle economy' : 'above the middle economy';
        return (
          <li key={d.key} className="text-sm">
            <div className="flex flex-wrap items-baseline justify-between gap-x-3">
              <span className="text-paper-dim">
                {d.category}
                {d.top && <span className="sr-only"> (a main driver)</span>}
                <span className="text-paper-mute">
                  {' '}
                  · {d.score.d} vs {d.median.d}
                  {d.imputed ? ' · filled-in data' : ''}
                  {d.constant ? ' · the same for every economy' : ''}
                </span>
              </span>
              <span className="tabular text-xs text-paper-mute">
                {d.impact.d} <span className="sr-only">({words})</span>
              </span>
            </div>
            <div className="relative mt-1 h-3 w-full bg-ink-700/60" aria-hidden="true">
              <span className="absolute inset-y-0 left-1/2 w-px bg-paper-mute/60" />
              {w > 0 && (
                <span
                  className="absolute inset-y-0"
                  style={{
                    left: neg ? `${50 - w}%` : '50%',
                    width: `${w}%`,
                    background: d.imputed ? 'transparent' : color,
                    border: d.imputed ? `1.5px solid ${color}` : undefined,
                    boxSizing: 'border-box',
                  }}
                />
              )}
            </div>
          </li>
        );
      })}
    </ul>
  );
}
