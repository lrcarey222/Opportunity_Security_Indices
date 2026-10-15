// PartnerSlope (spec_country.md A4.2 S5, "Priority partners and today's partners"): left, the top 10
// partners by Partnership Strength from the country's viewpoint; right, the top 10 by average trade share
// across its energy-goods baskets. A neutral line joins a partner present in both lists; a partner in one
// list only ends at a short stub and says its rank on the other side. Rows are buttons (select the
// partner); hover or focus highlights the row's line in orange and its text in white. Each row carries its
// group as a text tag (China / Core / Partner / Other), never colour alone. Below 640 px: two stacked
// lists, each row tagged "also in the other list (rank n)".

import { useState } from 'react';
import { SIGNAL } from '../../theme/palette.ts';
import type { SlopeSide } from '../derive/model.ts';
import { GROUP_WORD } from '../derive/model.ts';
import { useWidth } from './useWidth.ts';

const ROW = 40;

export interface PartnerSlopeProps {
  left: SlopeSide[];
  right: SlopeSide[];
  selected: string | null;
  onSelect: (iso3: string) => void;
}

function Row({ s, side, on, hot, setHot, onSelect, other }: {
  s: SlopeSide;
  side: 'left' | 'right';
  on: boolean;
  hot: boolean;
  setHot: (iso: string | null) => void;
  onSelect: (iso: string) => void;
  other: string;
}) {
  const label = side === 'left'
    ? `${s.name}, Partnership Strength rank ${s.rank}, value ${s.value}; ${other}`
    : `${s.name}, trade rank ${s.rank}, ${s.value}; ${other}`;
  return (
    <li style={{ height: ROW }}>
      <button
        type="button"
        className={`flex h-full w-full items-center gap-2 px-2 text-left text-sm ${side === 'right' ? 'flex-row-reverse text-right' : ''} ${on ? 'outline outline-2 outline-signal' : ''}`}
        aria-label={label}
        aria-pressed={on}
        onClick={() => onSelect(s.iso3)}
        onMouseEnter={() => setHot(s.iso3)}
        onMouseLeave={() => setHot(null)}
        onFocus={() => setHot(s.iso3)}
        onBlur={() => setHot(null)}
      >
        <span className="w-5 shrink-0 tabular text-xs text-paper-mute">{s.rank}</span>
        <span className="min-w-0 flex-1">
          <span className={`block truncate ${hot || on ? 'text-paper' : 'text-paper-dim'}`}>
            {s.name} <span className="tag ml-1 py-0 text-[10px]">{GROUP_WORD[s.group]}</span>
          </span>
          {side === 'right' && <span className="block truncate text-[11px] leading-tight text-paper-mute">{s.value}</span>}
        </span>
        {side === 'left' && <span className="shrink-0 tabular text-xs text-paper-mute">{s.value}</span>}
      </button>
    </li>
  );
}

export function PartnerSlope({ left, right, selected, onSelect }: PartnerSlopeProps) {
  const [hot, setHot] = useState<string | null>(null);
  const [ref, w] = useWidth<HTMLDivElement>();
  const narrow = w > 0 && w < 640;
  const rIdx = new Map(right.map((r, i) => [r.iso3, i]));
  const lIdx = new Map(left.map((r, i) => [r.iso3, i]));
  const otherL = (s: SlopeSide) => (rIdx.has(s.iso3) ? `also trade rank ${rIdx.get(s.iso3)! + 1}` : s.other !== null ? `trade rank ${s.other}` : 'no trade in these baskets');
  const otherR = (s: SlopeSide) => (lIdx.has(s.iso3) ? `also Partnership Strength rank ${lIdx.get(s.iso3)! + 1}` : s.other !== null ? `Partnership Strength rank ${s.other}` : 'not ranked');
  const n = Math.max(left.length, right.length);
  const H = n * ROW;

  if (narrow) {
    return (
      <div ref={ref} className="space-y-5">
        {[
          { title: 'Rated highest by the Partnership Strength Index', xs: left, other: otherL, side: 'left' as const },
          { title: 'Largest trade partners today', xs: right, other: otherR, side: 'right' as const },
        ].map((g) => (
          <div key={g.title}>
            <div className="eyebrow mb-1">{g.title}</div>
            <ol className="divide-y divide-ink-700">
              {g.xs.map((s) => (
                <li key={s.iso3}>
                  <button type="button" className={`w-full px-2 py-2 text-left text-sm ${selected === s.iso3 ? 'outline outline-2 outline-signal' : ''}`} aria-pressed={selected === s.iso3} onClick={() => onSelect(s.iso3)}>
                    <span className="tabular text-xs text-paper-mute">{s.rank}. </span>
                    <span className="text-paper-dim">{s.name}</span> <span className="tag ml-1 py-0 text-[10px]">{GROUP_WORD[s.group]}</span>
                    <span className="block text-xs text-paper-mute">{g.side === 'left' ? `Partnership Strength ${s.value} · ` : `${s.value} · `}{g.other(s)}</span>
                  </button>
                </li>
              ))}
            </ol>
          </div>
        ))}
      </div>
    );
  }

  const lines: { a: number; b: number; iso: string }[] = [];
  left.forEach((s, i) => {
    const j = rIdx.get(s.iso3);
    if (j !== undefined) lines.push({ a: i, b: j, iso: s.iso3 });
  });
  const y = (i: number) => i * ROW + ROW / 2;
  return (
    <div ref={ref}>
      <div className="mb-1 grid grid-cols-[minmax(0,1fr)_minmax(4rem,9rem)_minmax(0,1fr)] gap-0 text-xs">
        <div className="eyebrow px-2">Partnership Strength (rank, value)</div>
        <div />
        <div className="eyebrow px-2 text-right">Trade today (rank)</div>
      </div>
      <div className="grid grid-cols-[minmax(0,1fr)_minmax(4rem,9rem)_minmax(0,1fr)] gap-0">
        <ol className="m-0 p-0" aria-label="Partners the Partnership Strength Index rates highest">
          {left.map((s) => (
            <Row key={s.iso3} s={s} side="left" on={selected === s.iso3} hot={hot === s.iso3} setHot={setHot} onSelect={onSelect} other={otherL(s)} />
          ))}
        </ol>
        <svg width="100%" height={H} viewBox={`0 0 100 ${H}`} preserveAspectRatio="none" aria-hidden="true" className="overflow-visible">
          {lines.map((l) => {
            const on = hot === l.iso || selected === l.iso;
            return <line key={l.iso} x1={0} y1={y(l.a)} x2={100} y2={y(l.b)} stroke={on ? SIGNAL : '#9ca3af'} strokeWidth={on ? 2.5 : 1.5} vectorEffect="non-scaling-stroke" />;
          })}
          {left.filter((s) => !rIdx.has(s.iso3)).map((s) => {
            const i = lIdx.get(s.iso3)!;
            const on = hot === s.iso3 || selected === s.iso3;
            return <line key={`l-${s.iso3}`} x1={0} y1={y(i)} x2={14} y2={y(i)} stroke={on ? SIGNAL : '#6b7280'} strokeWidth={1.5} vectorEffect="non-scaling-stroke" />;
          })}
          {right.filter((s) => !lIdx.has(s.iso3)).map((s) => {
            const i = rIdx.get(s.iso3)!;
            const on = hot === s.iso3 || selected === s.iso3;
            return <line key={`r-${s.iso3}`} x1={86} y1={y(i)} x2={100} y2={y(i)} stroke={on ? SIGNAL : '#6b7280'} strokeWidth={1.5} vectorEffect="non-scaling-stroke" />;
          })}
        </svg>
        <ol className="m-0 p-0" aria-label="Largest trade partners today">
          {right.map((s) => (
            <Row key={s.iso3} s={s} side="right" on={selected === s.iso3} hot={hot === s.iso3} setHot={setHot} onSelect={onSelect} other={otherR(s)} />
          ))}
        </ol>
      </div>
      {/* the one-list stubs' other rank, in words under the chart */}
      <p className="mt-2 text-xs text-paper-mute">
        {[
          ...left.filter((s) => !rIdx.has(s.iso3)).map((s) => `${s.name}: ${otherL(s)}`),
          ...right.filter((s) => !lIdx.has(s.iso3)).map((s) => `${s.name}: ${otherR(s)}`),
        ].join(' · ') || 'Every partner appears in both lists.'}
      </p>
    </div>
  );
}
