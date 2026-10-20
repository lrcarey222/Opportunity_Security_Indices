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

/** "Japan, Korea and Germany" ("Japan, Korea or Germany" with `or`). */
export function listWords(items: readonly string[], conj: 'and' | 'or' = 'and'): string {
  if (items.length <= 1) return items[0] ?? '';
  if (items.length === 2) return `${items[0]} ${conj} ${items[1]}`;
  return `${items.slice(0, -1).join(', ')} ${conj} ${items[items.length - 1]}`;
}

/** "1 node" / "3 nodes". */
export const plural = (n: number, one: string, many = `${one}s`): string => `${n} ${n === 1 ? one : many}`;

/** "Batteries, Electric Vehicles · Midstream". */
export function techStage(techs: readonly string[], stage: string): string {
  return `${techs.join(', ') || DASH} · ${stage}`;
}

/** At most this many node labels in a short compact title (more: "{first} and N more", "{first} (and N more)" when the first holds a comma). */
export const TITLE_NODES = 3;

/** Node labels in one line: ", " between them, "; " when a label holds a comma ("Electric buses, trucks & tractors"). */
export const joinLabels = (labels: readonly string[]): string => labels.join(labels.some((l) => l.includes(',')) ? '; ' : ', ');

/**
 * A display title for a compact, built from its structured fields, never by parsing compact.name (node
 * labels can contain ", " and ": ", sector names "/"): "{sectors or technologies, ' / '}: {node labels}".
 * The labels are the compact's nodes in its own order, joined with ", " (with "; " when a label contains a
 * comma, so "Electric buses, trucks & tractors" stays one label); more than TITLE_NODES become "{first
 * label} and N more" unless `full` ("{first label} (and N more)" when that label holds a comma, so the count
 * never reads as part of it: "Electric buses, trucks & tractors (and 3 more)"). Nodes the document lacks are skipped; with none found, the exported
 * name. `nodes`: the document's nodes, or a lookup by id.
 */
export function compactTitle(
  c: { name: string; nodes: readonly string[]; technologies?: readonly string[] },
  nodes: readonly { id: string; label: string; techs?: readonly string[] }[] | ((id: string) => { label: string; techs?: readonly string[] } | null | undefined),
  opts: { full?: boolean } = {},
): string {
  const find = typeof nodes === 'function' ? nodes : (id: string) => nodes.find((n) => n.id === id);
  const own = c.nodes.map((id) => find(id)).filter((n): n is { label: string; techs?: readonly string[] } => !!n);
  if (!own.length) return c.name;
  const labels = [...new Set(own.map((n) => n.label))];
  const techs = c.technologies?.length ? [...c.technologies] : [...new Set(own.flatMap((n) => n.techs ?? []))].sort((a, b) => a.localeCompare(b, 'en'));
  const more = labels.length - 1;
  const short = labels[0].includes(',') ? `${labels[0]} (and ${more} more)` : `${labels[0]} and ${more} more`;
  const list = opts.full || labels.length <= TITLE_NODES ? joinLabels(labels) : short;
  return techs.length ? `${techs.join(' / ')}: ${list}` : list;
}
