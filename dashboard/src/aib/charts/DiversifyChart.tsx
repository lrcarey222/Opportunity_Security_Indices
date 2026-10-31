// The diversification scenario's chart (2026-10-19): one row per node where the scenario ran, in the order
// given (the capacity gap in USD, largest first). Each row: the node and where it sits; its world exporter
// concentration (HHI) against its benchmark on one shared HHI axis (a dumbbell: the benchmark a hollow ring,
// the current concentration a filled dot, the line between them the distance to close); and its allied
// demand split to scale (kept under the cap / restored by trade / new capacity, as the compact coverage
// strips read), with the values in text beside both (current → benchmark, as the axis title reads; the
// capped suppliers named "capped: ..."). HTML with percentage positions, so it reflows at any
// width (on a phone each row stacks: node, concentration, demand). The marks are mirrored in the legend;
// the values are in the text and in the table twin, so colour is never the only carrier.

import type { ReactNode } from 'react';
import { isNum } from '../../lib/format.ts';
import { PAPER_DIM, SIGNAL } from '../../theme/palette.ts';
import { CLASS_INFO } from '../derive/classes.ts';
import type { DemandSplit, DiversifyRow } from '../derive/diversify.ts';
import { fmtScore, fmtShare, fmtUsd } from '../derive/format.ts';
import { STEP_COLOR } from './CoverageStrip.tsx';

/** The marks: the current concentration, the benchmark, and the three parts of the demand split. */
export const DIVERSIFY_MARK = {
  current: SIGNAL,
  benchmark: PAPER_DIM,
  kept: STEP_COLOR.static,
  trade: STEP_COLOR.response,
  build: CLASS_INFO.BUILD.color,
} as const;

/** The parts of the split in words (legend, text, table). */
export const SPLIT_WORDS = { kept: 'Still supplied under the cap', trade: 'Recovered through trade', build: 'Requires new capacity' } as const;

const pct = (x: number, max = 1) => `${Math.max(0, Math.min(1, x / max)) * 100}%`;

/** "still supplied 51% · recovered through trade 1% · requires new capacity 48%". */
export function splitText(s: DemandSplit): string {
  return `still supplied ${fmtShare(s.kept, 0)} · recovered through trade ${fmtShare(s.trade, 0)} · requires new capacity ${fmtShare(s.build, 0)}`;
}

/** A node's allied demand split to scale: three segments of one 0-100% bar. */
export function DemandSplitBar({ split, label, className = 'h-3' }: { split: DemandSplit; label: string; className?: string }) {
  const parts = (['kept', 'trade', 'build'] as const).filter((k) => split[k] > 0);
  return (
    <div role="img" aria-label={label} className={`flex w-full overflow-hidden bg-ink-700/70 ${className}`} data-testid="demand-split">
      {parts.map((k) => (
        <span key={k} className="block h-full" style={{ width: pct(split[k]), background: DIVERSIFY_MARK[k] }} data-part={k} />
      ))}
    </div>
  );
}

/** One node's concentration against its benchmark on a 0-`max` HHI axis. */
export function HhiDumbbell({ current, benchmark, max, label }: { current: number | null; benchmark: number | null; max: number; label: string }) {
  const lo = isNum(current) && isNum(benchmark) ? Math.min(current, benchmark) : null;
  const hi = isNum(current) && isNum(benchmark) ? Math.max(current, benchmark) : null;
  return (
    <div role="img" aria-label={label} className="relative h-4 w-full" data-testid="hhi-dumbbell">
      <span className="absolute inset-x-0 top-1/2 block h-px -translate-y-1/2 bg-white/15" aria-hidden="true" />
      {lo !== null && hi !== null && (
        <span className="absolute top-1/2 block h-0.5 -translate-y-1/2" style={{ left: pct(lo, max), width: `calc(${pct(hi, max)} - ${pct(lo, max)})`, background: DIVERSIFY_MARK.current }} aria-hidden="true" />
      )}
      {isNum(benchmark) && (
        <span
          className="absolute top-1/2 block h-3 w-3 -translate-x-1/2 -translate-y-1/2 rounded-full border-2 bg-ink-800"
          style={{ left: pct(benchmark, max), borderColor: DIVERSIFY_MARK.benchmark }}
          aria-hidden="true"
          data-mark="benchmark"
        />
      )}
      {isNum(current) && (
        <span
          className="absolute top-1/2 block h-3 w-3 -translate-x-1/2 -translate-y-1/2 rounded-full"
          style={{ left: pct(current, max), background: DIVERSIFY_MARK.current }}
          aria-hidden="true"
          data-mark="current"
        />
      )}
    </div>
  );
}

/** The ticks of the shared HHI axis (every 0.1 up to `max`; every 0.2 above 0.6). */
export function hhiTicks(max: number): number[] {
  const step = max > 0.6 ? 0.2 : 0.1;
  const out: number[] = [];
  for (let t = 0; t <= max + 1e-9; t += step) out.push(Math.round(t * 10) / 10);
  return out;
}

function Axis({ ticks, max, fmt, title }: { ticks: number[]; max: number; fmt: (t: number) => string; title: string }) {
  return (
    <div>
      <span className="block text-paper-mute">{title}</span>
      <div className="relative mt-0.5 h-4">
        {ticks.map((t, i) => (
          // the end labels sit inside the column, so they never run into the next one
          <span
            key={t}
            className={`absolute top-0 tabular ${i === 0 ? '' : i === ticks.length - 1 && t >= max - 1e-9 ? '-translate-x-full' : '-translate-x-1/2'}`}
            style={{ left: pct(t, max) }}
          >
            {fmt(t)}
          </span>
        ))}
      </div>
    </div>
  );
}

const GRID = 'md:grid md:grid-cols-[minmax(0,15rem)_minmax(0,1fr)_minmax(0,1fr)] md:gap-x-6';

export interface DiversifyChartProps {
  rows: DiversifyRow[];
  /** the HHI axis maximum (hhiAxisMax of the rows) */
  max: number;
  /** a capped supplier's name */
  name: (iso3: string) => string;
  /** the node's place and links (its OSI cells, its compact, its Sector Profiles) */
  where: (r: DiversifyRow) => ReactNode;
  label: string;
}

export function DiversifyChart({ rows, max, name, where, label }: DiversifyChartProps) {
  const ticks = hhiTicks(max);
  return (
    <div className="text-xs">
      <div className={`hidden pb-2 ${GRID}`} aria-hidden="true" data-testid="diversify-axes">
        <span className="self-end text-paper-mute">Supply-chain step</span>
        <Axis ticks={ticks} max={max} fmt={(t) => t.toFixed(1)} title="Concentration (HHI): current → benchmark" />
        <Axis ticks={[0, 0.25, 0.5, 0.75, 1]} max={1} fmt={(t) => `${Math.round(t * 100)}%`} title="Allied demand: still supplied · recovered through trade · requires new capacity" />
      </div>
      <ol className="divide-y divide-white/5 border-t hairline" aria-label={label}>
        {rows.map((r) => {
          const { d } = r;
          const capped = d.capped.map(name).join(', ');
          const hhiWords = `concentration (HHI) ${fmtScore(d.currentHhi)} against a benchmark of ${fmtScore(d.benchmarkHhi)}${isNum(d.cap) ? `, supplier cap ${fmtShare(d.cap, 0)}` : ''}${capped ? `, capped: ${capped}` : ''}`;
          return (
            <li key={r.node.id} className={`space-y-1.5 py-2.5 md:space-y-0 md:items-center ${GRID}`} data-node={r.node.id}>
              <div className="min-w-0">
                <span className="block text-sm text-paper">{r.node.label}</span>
                <span className="block text-paper-mute">{where(r)}</span>
              </div>
              <div className="min-w-0">
                <span className="text-paper-mute md:hidden">Concentration (HHI)</span>
                <HhiDumbbell current={d.currentHhi} benchmark={d.benchmarkHhi} max={max} label={`${r.node.label}: ${hhiWords}`} />
                <span className="block tabular text-paper-dim">
                  {fmtScore(d.currentHhi)} → {fmtScore(d.benchmarkHhi)}
                  {isNum(d.cap) ? ` · cap ${fmtShare(d.cap, 0)}` : ''}
                  {capped ? ` · capped: ${capped}` : ''}
                </span>
              </div>
              <div className="min-w-0">
                <span className="text-paper-mute md:hidden">Allied demand</span>
                {r.split ? (
                  <>
                    <DemandSplitBar split={r.split} label={`${r.node.label}: ${splitText(r.split)} of allied demand`} />
                    <span className="block tabular text-paper-dim">
                      {fmtShare(r.split.kept, 0)} kept · +{fmtShare(r.split.trade, 0)} by trade · {fmtShare(r.split.build, 0)} new capacity
                      {isNum(r.gapUsd) && r.split.build > 1e-9 ? ` (${fmtUsd(r.gapUsd)})` : ''}
                    </span>
                  </>
                ) : (
                  <span className="block text-paper-mute">no figures for the share of demand met</span>
                )}
              </div>
            </li>
          );
        })}
      </ol>
    </div>
  );
}
