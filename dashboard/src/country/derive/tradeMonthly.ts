// Monthly trade by sub-component (additive 2026-10-19): the pure pieces of the Country profile's card over
// the companion trade file (TradeMonthlyDoc). Which cell to show, the indexed lines (one per sub-component,
// largest first, at most SUB_LINES, plus the cell total as a neutral line), the notes on what is left out,
// the table twin and the CSV rows. Months are x = 0..11.

import type { LineSeries } from '../../sector/charts/types.ts';
import { indexTo100, type TradeMonthlyCell, type TradeMonthlyDoc, type TradeMonthlySeries } from '../contract.ts';

export type TradeFlow = 'imports' | 'exports';

/** At most this many sub-component lines (the cell total is one more line, MAX_LINES = 7). */
export const SUB_LINES = 6;
/** The validated line colours; the 5th and 6th lines reuse the first two with a dash pattern. */
export const SUB_COLORS = ['#1889b0', '#8b9d17', '#2fa5a6', '#c96f7a'] as const;
export const TOTAL_ID = '__total__';
export const TOTAL_LABEL = 'All sub-components';

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

/** "2025-08" -> "Aug 2025". */
export function monthLabel(m: string): string {
  const [y, mm] = m.split('-');
  const i = Number(mm) - 1;
  return i >= 0 && i < 12 ? `${MONTHS[i]} ${y}` : m;
}

/** An x-axis tick over month indices: whole months only; a short year on the first tick and on January ("Aug '25", "Sep", ..., "Jan '26") so a phone-width axis does not collide. */
export function monthTick(months: readonly string[], x: number): string {
  if (!Number.isInteger(x) || x < 0 || x >= months.length) return '';
  const m = months[x];
  const short = monthLabel(m).slice(0, 3);
  return x === 0 || m.endsWith('-01') ? `${short} '${m.slice(2, 4)}` : short;
}

/** "USD 1.23 bn", "USD 456 m", "USD 0.12 m", "USD 0". */
export function fmtUsd(x: number | null | undefined): string {
  if (x === null || x === undefined || !Number.isFinite(x)) return '–';
  const a = Math.abs(x);
  if (a >= 1e9) return `USD ${(x / 1e9).toLocaleString('en-US', { maximumSignificantDigits: 3 })} bn`;
  if (a >= 1e4) return `USD ${(x / 1e6).toLocaleString('en-US', { maximumSignificantDigits: 3 })} m`;
  return `USD ${Math.round(x).toLocaleString('en-US')}`;
}

const sum = (v: readonly (number | null)[]) => v.reduce<number>((s, z) => s + (z ?? 0), 0);

/** The cell to show: the page's selected sector when the file has it (the file lists every cell with an HS6 basket), else the cell with the most trade (`fallback`: the selected sector has no basket). */
export function pickTradeCell(doc: TradeMonthlyDoc, selected: string | null): { cell: TradeMonthlyCell; fallback: boolean } {
  const hit = selected ? doc.cells.find((c) => c.key === selected) : undefined;
  if (hit) return { cell: hit, fallback: false };
  const best = [...doc.cells].sort((a, b) => sum([...b.total.imports, ...b.total.exports]) - sum([...a.total.imports, ...a.total.exports]))[0];
  return { cell: best, fallback: selected !== null };
}

const indexable = (s: TradeMonthlySeries, flow: TradeFlow) => indexTo100(s[flow])[0] === 100;

export interface TradeLines {
  lines: LineSeries[];
  /** sub-components with trade but no base (no trade or no report in the first month) */
  notIndexed: string[];
  /** indexable sub-components beyond SUB_LINES (in the table only) */
  smaller: string[];
  /** sub-components with no trade at all in this flow */
  none: string[];
}

/** The indexed lines of one cell and flow: sub-components by value over the window, then the total. */
export function tradeLines(cell: TradeMonthlyCell, flow: TradeFlow): TradeLines {
  const subs = [...cell.subs].sort((a, b) => sum(b[flow]) - sum(a[flow]) || a.name.localeCompare(b.name));
  const none = subs.filter((s) => sum(s[flow]) === 0).map((s) => s.name);
  const withTrade = subs.filter((s) => sum(s[flow]) > 0);
  const ok = withTrade.filter((s) => indexable(s, flow));
  const notIndexed = withTrade.filter((s) => !indexable(s, flow)).map((s) => s.name);
  const shown = ok.slice(0, SUB_LINES);
  const pts = (v: readonly (number | null)[]) => v.map((y, x) => ({ x, y: y ?? Number.NaN }));
  const lines: LineSeries[] = shown.map((s, i) => ({ id: s.name, label: s.name, group: 'core', color: SUB_COLORS[i % SUB_COLORS.length], points: pts(s[flow]) }));
  if (shown.length > 1 && indexable(cell.total, flow)) lines.push({ id: TOTAL_ID, label: TOTAL_LABEL, group: 'neutral', points: pts(cell.total[flow]) });
  return { lines, notIndexed, smaller: ok.slice(SUB_LINES).map((s) => s.name), none };
}

/** Months the economy did not report in full for this flow ("Jul 2026"): not reported, or a partial release (R country_monthly_table()). */
export function unreportedMonths(doc: TradeMonthlyDoc, flow: TradeFlow): string[] {
  return doc.window.months.filter((_, i) => !doc.window.declared[flow][i]).map(monthLabel);
}

/** Long rows for the table twin and the CSV: one per month and sub-component (and the total). */
export function tradeRows(doc: TradeMonthlyDoc, cell: TradeMonthlyCell, flow: TradeFlow) {
  const out: { month: string; sub_component: string; value_usd: number | null; index_first_month_100: number | null }[] = [];
  const series: [string, TradeMonthlySeries][] = [...cell.subs.map((s) => [s.name, s] as [string, TradeMonthlySeries]), [TOTAL_LABEL, cell.total]];
  for (const [name, s] of series) {
    const idx = indexTo100(s[flow]);
    doc.window.months.forEach((m, i) => out.push({ month: m, sub_component: name, value_usd: s[flow][i], index_first_month_100: idx[i] }));
  }
  return out;
}
