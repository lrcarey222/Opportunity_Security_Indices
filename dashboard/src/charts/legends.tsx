// Legends (dataviz: a legend for >= 2 series; marks mirrored: dot, hollow ring, rect, line, hatch)
// and colour-ramp legends for sequential and diverging scales. Text wears text tokens, never the
// series colour.

import { DIV_RANGE, SEQ, SIGNAL } from '../theme/palette.ts';

export type LegendKind = 'dot' | 'hollow' | 'rect' | 'line' | 'hatch' | 'outline' | 'diamond' | 'tick';

export interface LegendItem {
  label: string;
  color?: string;
  kind?: LegendKind;
  /** a line's dash pattern (SVG stroke-dasharray), mirrored in the key */
  dash?: string | null;
}

function Key({ kind = 'rect', color = '#9ca3af', dash }: { kind?: LegendKind; color?: string; dash?: string | null }) {
  if (kind === 'line' && dash) {
    return (
      <svg width="20" height="4" className="inline-block" aria-hidden="true">
        <line x1="0" y1="2" x2="20" y2="2" stroke={color} strokeWidth="2" strokeDasharray={dash} />
      </svg>
    );
  }
  switch (kind) {
    case 'dot':
      return <span className="inline-block h-2.5 w-2.5 rounded-full" style={{ background: color }} />;
    case 'hollow':
      return <span className="inline-block h-2.5 w-2.5 rounded-full border-2" style={{ borderColor: color }} />;
    case 'line':
      return <span className="inline-block h-0.5 w-4" style={{ background: color }} />;
    case 'tick':
      return <span className="inline-block h-3 w-0.5" style={{ background: color }} />;
    case 'hatch':
      return <span className="swatch-hatch inline-block h-2.5 w-3.5" />;
    case 'outline':
      return <span className="inline-block h-2.5 w-3.5 border-2" style={{ borderColor: color ?? SIGNAL }} />;
    case 'diamond':
      return <span className="inline-block h-2 w-2 rotate-45" style={{ background: color }} />;
    default:
      return <span className="inline-block h-2.5 w-3.5" style={{ background: color }} />;
  }
}

export function Legend({ items, label = 'Legend' }: { items: LegendItem[]; label?: string }) {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label={label}>
      {items.map((it) => (
        <li key={it.label} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className={`inline-flex ${it.dash ? 'w-5' : 'w-4'} items-center justify-center`}>
            <Key kind={it.kind} color={it.color} dash={it.dash} />
          </span>
          {it.label}
        </li>
      ))}
    </ul>
  );
}

/** A ramp legend: sequential (low -> high) or diverging (decrease | 0 | increase). */
export function RampLegend({
  kind, min, max, label, format, reverse,
}: {
  kind: 'seq' | 'div';
  min: number;
  max: number;
  label: string;
  format: (v: number) => string;
  reverse?: boolean;
}) {
  const colors = kind === 'seq' ? (reverse ? [...SEQ].reverse() : SEQ) : DIV_RANGE;
  const stops = colors.map((c, i) => `${c} ${(100 * i) / (colors.length - 1)}%`).join(', ');
  return (
    <div className="inline-flex flex-col gap-1 text-xs text-paper-mute" aria-label={`${label}: from ${format(min)} to ${format(max)}`}>
      <span>{label}</span>
      <span className="block h-2.5 w-56 max-w-full" style={{ background: `linear-gradient(to right, ${stops})` }} aria-hidden="true" />
      <span className="flex w-56 max-w-full justify-between tabular">
        <span>{format(min)}</span>
        {kind === 'div' && <span>0</span>}
        <span>{format(max)}</span>
      </span>
    </div>
  );
}
