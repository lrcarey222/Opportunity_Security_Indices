// Formats of the Sectoral Compacts views: shares as %, differences of shares as percentage points, USD
// as $m / $bn, economies by name. NA (null / NaN) prints as an en dash. Pure.

import { DASH, isNum, MINUS } from '../../lib/format.ts';

/** A 0-1 share as a percentage: "62.5%" (digits 1) or "63%" (digits 0). */
export function fmtShare(x: number | null | undefined, digits = 1): string {
  if (!isNum(x)) return DASH;
  const s = (x * 100).toFixed(digits);
  return `${s.startsWith('-') ? MINUS + s.slice(1) : s}%`;
}

/** A difference of shares in percentage points: "+12.3 pp", "−4.0 pp", "0.0 pp". */
export function fmtPp(x: number | null | undefined, digits = 1): string {
  if (!isNum(x)) return DASH;
  const v = Number((x * 100).toFixed(digits));
  if (v === 0) return `${(0).toFixed(digits)} pp`;
  return `${v > 0 ? '+' : MINUS}${Math.abs(x * 100).toFixed(digits)} pp`;
}

/** Dollars as $m or $bn: "$1.2 bn", "$340 m", "$4.5 m", "$0.25 m". */
export function fmtUsd(x: number | null | undefined): string {
  if (!isNum(x)) return DASH;
  const a = Math.abs(x);
  const sign = x < 0 ? MINUS : '';
  if (a >= 1e9) return `${sign}$${(a / 1e9).toFixed(a >= 1e11 ? 0 : 1)} bn`;
  if (a >= 1e6) return `${sign}$${(a / 1e6).toFixed(a >= 1e8 ? 0 : 1)} m`;
  if (a === 0) return '$0';
  return `${sign}$${(a / 1e6).toFixed(2)} m`;
}

/** A score shown as a plain number with two decimals ("0.62"). */
export function fmtScore(x: number | null | undefined): string {
  return isNum(x) ? x.toFixed(2) : DASH;
}

/** The name of an economy (its code when the document has no name for it). */
export function nameOf(names: Readonly<Record<string, string>>, iso3: string | null | undefined): string {
  if (!iso3) return DASH;
  return names[iso3] ?? iso3;
}

/** "Japan, Korea and Germany". */
export function listWords(items: readonly string[]): string {
  if (items.length <= 1) return items[0] ?? '';
  if (items.length === 2) return `${items[0]} and ${items[1]}`;
  return `${items.slice(0, -1).join(', ')} and ${items[items.length - 1]}`;
}

/** "1 node" / "3 nodes". */
export const plural = (n: number, one: string, many = `${one}s`): string => `${n} ${n === 1 ? one : many}`;

/** "Batteries, Electric Vehicles · Midstream". */
export function techStage(techs: readonly string[], stage: string): string {
  return `${techs.join(', ') || DASH} · ${stage}`;
}

/**
 * A short display title for a compact. The exporter names a compact by its technologies and every node,
 * cut to "…, and N more" when long (up to ~150 characters); such a name becomes "{technologies}: {first
 * node} and {N} more". Shorter names are kept whole. Display only.
 */
export function compactTitle(c: { name: string; nodes: readonly string[] }): string {
  if (!/, and \d+ more$/.test(c.name)) return c.name;
  const i = c.name.indexOf(': ');
  if (i < 0) return c.name;
  const first = c.name.slice(i + 2).split(', ')[0];
  const more = c.nodes.length - 1;
  return more > 0 ? `${c.name.slice(0, i)}: ${first} and ${more} more` : `${c.name.slice(0, i)}: ${first}`;
}
