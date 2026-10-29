// Legends (dataviz: a legend for >= 2 series; marks mirrored: dot, hollow ring, rect, line, hatch)
// and colour-ramp legends: TriLegend for the three-colour scale (spec_country C1.3; the one to use),
// RampLegend for the superseded sequential and diverging ramps. Text wears text tokens, never the
// series colour.

import type { ReactNode } from 'react';
import { DIV_RANGE, SEQ, SIGNAL, TRI } from '../theme/palette.ts';
import type { TriScale } from './scale.ts';

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

/** A ramp legend: sequential (low -> high) or diverging (decrease | 0 | increase).
 *  @deprecated Use TriLegend with a triScale (spec_country.md C1.3). */
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

export interface TriLegendProps {
  scale: TriScale;
  /** what the colour shows ("Energy Security, Batteries Refining") */
  label: string;
  format: (v: number) => string;
  /** the things counted in "median of the {n} {noun} shown" (default "values") */
  noun?: string;
  /** override the end words (default lower / higher; swapped when the scale is reversed) */
  lowWord?: string;
  highWord?: string;
  /** a hatched swatch with this label (missing, not ranked) */
  missing?: string;
  /** extra content after the words line */
  children?: ReactNode;
}

const W = 256;

/** The words of a three-colour legend: what orange, white and blue mean (also the aria-label body). */
export function triWords(scale: TriScale, format: (v: number) => string, noun = 'values', lowWord?: string, highWord?: string): { orange: string; white: string; blue: string } {
  if (scale.center === 'zero') return { orange: lowWord ?? 'negative', white: 'zero', blue: highWord ?? 'positive' };
  const low = lowWord ?? 'lower';
  const high = highWord ?? 'higher';
  const white = scale.basis === 'rank' ? `median of the ${scale.n} ${noun} shown (${format(scale.mid)})` : `middle of the scale (${format(scale.mid)})`;
  return scale.reverse ? { orange: high, white, blue: low } : { orange: low, white, blue: high };
}

/**
 * The legend of a three-colour scale: a 256 px bar of the nine stops as hard blocks (the reader reads
 * steps), the tick values under it at their positions (each tick sits where its colour is), and one line
 * of words: "Orange: lower · White: median of the 57 economies shown (0.52) · Blue: higher".
 */
export function TriLegend({ scale, label, format, noun = 'values', lowWord, highWord, missing, children }: TriLegendProps) {
  const words = triWords(scale, format, noun, lowWord, highWord);
  const ends = scale.n || scale.basis === 'value' ? ` from ${format(scale.reverse ? scale.hi : scale.lo)} (orange) to ${format(scale.reverse ? scale.lo : scale.hi)} (blue)` : '';
  const aria = `${label}:${ends}. Orange: ${words.orange}; white: ${words.white}; blue: ${words.blue}.`;
  const k = TRI.length;
  // a position p in [0, 1] is drawn at the centre of block p x 8 (block i spans [i / 9, (i + 1) / 9])
  const x = (p: number) => ((p * (k - 1) + 0.5) / k) * 100;
  // long labels (units, currencies): the ends and the median only, so the quartile labels never collide
  const labels = scale.ticks.map((t) => format(t.value));
  const ticks = labels.some((l) => l.length > 7) && scale.ticks.length > 3 ? scale.ticks.filter((t) => t.value === scale.lo || t.value === scale.hi || t.value === scale.mid) : scale.ticks;
  return (
    <div className="inline-flex max-w-full flex-col gap-1 text-xs text-paper-mute" role="group" aria-label={aria} data-testid="tri-legend">
      <span className="text-paper-dim">{label}</span>
      <span className="flex h-2.5 max-w-full" style={{ width: W }} aria-hidden="true">
        {TRI.map((c) => (
          <span key={c} className="block h-full flex-1" style={{ background: c }} />
        ))}
      </span>
      {ticks.length > 0 && (
        <span className="relative block h-4 max-w-full tabular" style={{ width: W }} aria-hidden="true">
          {ticks.map((t, i) => {
            const left = x(t.pos);
            const edge = i === 0 && left < 12 ? 'start' : i === ticks.length - 1 && left > 88 ? 'end' : 'mid';
            return (
              <span
                key={`${t.value}-${i}`}
                className="absolute top-0 whitespace-nowrap"
                style={{ left: `${left}%`, transform: edge === 'start' ? 'translateX(-4px)' : edge === 'end' ? 'translateX(calc(-100% + 4px))' : 'translateX(-50%)' }}
              >
                {format(t.value)}
              </span>
            );
          })}
        </span>
      )}
      <span aria-hidden="true">
        <span className="text-paper-dim">Orange:</span> {words.orange} · <span className="text-paper-dim">White:</span> {words.white} · <span className="text-paper-dim">Blue:</span> {words.blue}
      </span>
      {missing && (
        <span className="inline-flex items-center gap-1.5" aria-hidden="true">
          <span className="swatch-hatch inline-block h-2.5 w-3.5" />
          {missing}
        </span>
      )}
      {children}
    </div>
  );
}
