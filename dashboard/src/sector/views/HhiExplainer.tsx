// The concentration index explained in plain words (user edit 2a-ii; spec_country.md C5.3): what the
// HHI is, how to read it (ten equal holders 0.10, four 0.25, one 1), its inverse as the "effective
// number" of holders, and, on a sector page, what this sector's value summarises: a bar of the largest
// holders' shares (group colours, "Not attributed" hatched) and a small reference scale with the
// sector's value on it. Competition-authority thresholds (dict.hhi_refs) are given as context only.
// Shown once in the Trade section of every profile (open the first time, collapsed afterwards; the
// flag lives in localStorage, wrapped in try/catch) and at the top of Concentration.

import { useEffect, useState } from 'react';
import { Term } from '../../components/Term.tsx';
import type { SectorModel } from '../data/model.ts';
import { groupColor, HATCH_URL } from '../charts/color.ts';
import type { GroupKey } from '../charts/types.ts';
import { GROUP_SHORT } from '../derive/groups.ts';
import { fmtShare, listText } from '../format.ts';
import { SvgDefs } from './kit.tsx';

export interface HhiHolder {
  name: string;
  share: number;
  group: GroupKey;
}

export interface HhiExplainerProps {
  model: SectorModel;
  /** the sector's index (0-1); omitted on the Concentration page (the general text only) */
  hhi?: number | null;
  lowerBound?: boolean;
  /** holders, largest first, with their shares of the total */
  holders?: readonly HhiHolder[];
  /** share of the total that no economy is named for */
  unattributed?: number;
  /** "exporters" (default), "producers" */
  noun?: string;
  /** what the shares are of: "world exports" (default) */
  of?: string;
  /** "Batteries — Midstream, 2024": what the sector values describe */
  context?: string;
  className?: string;
}

const SEEN_KEY = 'osi.sector.hhiExplainerSeen';

function readSeen(): boolean {
  try {
    return typeof localStorage !== 'undefined' && localStorage.getItem(SEEN_KEY) === '1';
  } catch {
    return false;
  }
}

function writeSeen() {
  try {
    if (typeof localStorage !== 'undefined') localStorage.setItem(SEEN_KEY, '1');
  } catch {
    // private window, blocked storage: the explainer simply stays open next time
  }
}

/** "0.31", "at least 0.31": the index printed with two decimals. */
export function hhiText(hhi: number, lowerBound = false): string {
  return `${lowerBound ? 'at least ' : ''}${hhi.toFixed(2)}`;
}

/** The effective number of equal holders (1 / HHI) with one decimal ("3.2"), or null. */
export function effectiveN(hhi: number | null | undefined): number | null {
  return hhi && hhi > 0 && Number.isFinite(hhi) ? Math.round((1 / hhi) * 10) / 10 : null;
}

/**
 * The sentence about one sector: "Here the index is 0.31, which behaves like 3.2 equal exporters,
 * although China alone holds 52%." (the "although" clause when the largest holder clearly exceeds an
 * equal share).
 */
export function hhiSentence(hhi: number, lowerBound: boolean, holders: readonly HhiHolder[], noun = 'exporters'): string {
  const n = effectiveN(hhi);
  const top = holders[0];
  const equal = n ? 1 / n : 1;
  const although = top && n && n > 1.05 && top.share > equal + 0.05 ? `, although ${top.name} alone holds ${fmtShare(top.share)}` : '';
  const like = n ? `, which behaves like ${lowerBound ? 'at most ' : ''}${n.toFixed(1)} equal ${noun}` : '';
  return `Here the index is ${hhiText(hhi, lowerBound)}${like}${although}.`;
}

/** The 10-segment bar: the nine largest holders, everyone else, and the part no economy is named for. */
function HoldersBar({ holders, unattributed, noun }: { holders: readonly HhiHolder[]; unattributed: number; noun: string }) {
  const top = holders.slice(0, 9);
  const shown = top.reduce((a, h) => a + h.share, 0);
  const rest = Math.max(0, 1 - shown - unattributed);
  const segs: { key: string; share: number; fill: string; title: string }[] = top.map((h) => ({ key: h.name, share: h.share, fill: groupColor(h.group), title: `${h.name}: ${fmtShare(h.share)} (${GROUP_SHORT[h.group as keyof typeof GROUP_SHORT] ?? ''})` }));
  if (rest > 1e-6) segs.push({ key: '__rest', share: rest, fill: '#6b7280', title: `All other ${noun}: ${fmtShare(rest)}` });
  if (unattributed > 1e-6) segs.push({ key: '__un', share: unattributed, fill: HATCH_URL, title: `Not attributed to any economy: ${fmtShare(unattributed)}` });
  let x = 0;
  const label = segs.map((s) => s.title).join('; ');
  return (
    <figure className="m-0">
      <svg viewBox="0 0 100 6" preserveAspectRatio="none" className="block h-5 w-full" role="img" aria-label={`Shares: ${label}`}>
        <SvgDefs />
        {segs.map((s) => {
          const w = s.share * 100;
          const r = (
            <rect key={s.key} x={x} y={0} width={Math.max(0, w - 0.3)} height={6} fill={s.fill}>
              <title>{s.title}</title>
            </rect>
          );
          x += w;
          return r;
        })}
      </svg>
      <figcaption className="mt-1 text-xs text-paper-mute">
        {listText(top.slice(0, 3).map((h) => `${h.name} ${fmtShare(h.share)}`))}
        {top.length > 3 ? `; then ${top.length - 3} more shown as segments` : ''}
        {rest > 1e-6 ? `; all other ${noun} ${fmtShare(rest)}` : ''}
        {unattributed > 1e-6 ? `; not attributed ${fmtShare(unattributed)} (hatched)` : ''}.
      </figcaption>
    </figure>
  );
}

/** The reference scale: where 10, 4, 2 and 1 equal holders sit, and this sector. */
function ReferenceScale({ hhi, lowerBound }: { hhi: number | null; lowerBound: boolean }) {
  const marks = [
    { v: 0.1, label: '10 equal' },
    { v: 0.25, label: '4 equal' },
    { v: 0.5, label: '2 equal' },
    { v: 1, label: '1 holds all' },
  ];
  const X = (v: number) => 4 + v * 92;
  const aria = `Reference scale from 0 to 1: ten equal holders 0.10, four 0.25, two 0.50, one holder 1${hhi !== null ? `; this sector ${hhiText(hhi, lowerBound)}` : ''}.`;
  return (
    <svg viewBox="0 0 100 26" className="block h-auto w-full max-w-md" role="img" aria-label={aria}>
      <line x1={X(0)} x2={X(1)} y1={10} y2={10} stroke="rgb(255 255 255 / 0.35)" strokeWidth={0.4} />
      {marks.map((m) => (
        <g key={m.v}>
          <line x1={X(m.v)} x2={X(m.v)} y1={8} y2={12} stroke="rgb(255 255 255 / 0.55)" strokeWidth={0.4} />
          <text x={X(m.v)} y={17} textAnchor={m.v === 1 ? 'end' : 'middle'} fontSize={3.2} fill="#9ca3af">
            {m.v.toFixed(2)}
          </text>
          <text x={X(m.v)} y={21.5} textAnchor={m.v === 1 ? 'end' : 'middle'} fontSize={3.2} fill="#9ca3af">
            {m.label}
          </text>
        </g>
      ))}
      <text x={X(0)} y={17} fontSize={3.2} fill="#9ca3af">
        0
      </text>
      {hhi !== null && Number.isFinite(hhi) && (
        <g>
          <path d={`M ${X(hhi)} 9 l -1.6 -3.2 h 3.2 z`} fill="#ffffff" />
          <text x={Math.min(Math.max(X(hhi), 14), 86)} y={4} textAnchor="middle" fontSize={3.4} fill="#ffffff">
            this sector {hhiText(hhi, lowerBound)}
          </text>
        </g>
      )}
    </svg>
  );
}

/** The competition-authority thresholds as one context sentence (never a verdict). */
export function hhiRefsSentence(model: Pick<SectorModel, 'hhiRefs'>): string | null {
  const refs = model.hhiRefs;
  if (!refs.length) return null;
  // two yardsticks at most: the lowest "below" threshold and the latest "above" one
  const below = refs.filter((r) => r.side === 'below').sort((a, z) => a.value - z.value)[0];
  const above = refs.filter((r) => r.side === 'above').sort((a, z) => z.year - a.year || z.value - a.value)[0];
  const parts = [below, above].filter((r): r is NonNullable<typeof r> => !!r).map((r) => `${r.side} ${r.value.toFixed(2)} as "${r.label}" (${r.authority}, ${r.year})`);
  return `For comparison, merger rules describe a market ${listText(parts)}. Those thresholds were written for firms competing in one market, not for trade between whole economies, so they are a yardstick, not a verdict.`;
}

export function HhiExplainer({ model, hhi = null, lowerBound = false, holders = [], unattributed = 0, noun = 'exporters', of = 'world exports', context, className }: HhiExplainerProps) {
  const [open, setOpen] = useState(() => !readSeen());
  useEffect(() => {
    writeSeen();
  }, []);
  const sector = hhi !== null && Number.isFinite(hhi);
  const singular = noun.replace(/s$/, '');
  const refs = hhiRefsSentence(model);
  return (
    <details className={`panel p-4 ${className ?? ''}`} open={open} onToggle={(e) => setOpen((e.currentTarget as HTMLDetailsElement).open)} data-testid="hhi-explainer">
      <summary className="cursor-pointer text-sm font-medium text-paper">What the concentration index (HHI) means</summary>
      <div className="mt-3 grid gap-4 md:grid-cols-5">
        <div className="space-y-2 text-sm text-paper-dim md:col-span-3">
          <p>
            The <Term id="hhi">concentration index</Term> summarises how much of a market a few holders control. Take each {singular}&apos;s share of {of}, square it, and add the squares. If one economy held everything, the index would be 1. Ten equal {noun} give 0.10; four give 0.25.
          </p>
          <p>
            Its inverse is the <Term id="effective_n">effective number</Term> of {noun}: how many equal-sized {noun} would give the same concentration. {sector ? hhiSentence(hhi as number, lowerBound, holders, noun) : 'A sector with an index of 0.25 behaves like four equal holders, however many small ones there are.'}
          </p>
          <p>Where part of the world total is not attributed to any economy, the index is a lower bound (&quot;at least&quot;).</p>
          {refs && <p className="text-xs text-paper-mute">{refs}</p>}
        </div>
        <div className="space-y-3 md:col-span-2">
          {sector && holders.length > 0 && (
            <div>
              <div className="eyebrow mb-1">What the number summarises{context ? `: ${context}` : ''}</div>
              <HoldersBar holders={holders} unattributed={unattributed} noun={noun} />
            </div>
          )}
          <div>
            <div className="eyebrow mb-1">Reference scale</div>
            <ReferenceScale hhi={sector ? (hhi as number) : null} lowerBound={lowerBound} />
          </div>
        </div>
      </div>
    </details>
  );
}
