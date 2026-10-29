// Formats of the Sectoral Compacts views: shares as %, differences of shares as percentage points, USD
// as $m / $bn, economies by name. NA (null / NaN) prints as an en dash. Pure.

import { DASH, isNum, MINUS } from '../../lib/format.ts';
import { parseCellSlug } from '../../lib/stages.ts';

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

/** At most this many node labels in a short compact title (more: "{first} and N more", "{first} (and N more)" when the first holds a comma). */
export const TITLE_NODES = 3;

/** At most this many OSI technologies in a compact title (more: "{first} + N more"). */
export const TITLE_TECHS = 2;

/** Node labels in one line: ", " between them, "; " when a label holds a comma ("Electric buses, trucks & tractors"). */
export const joinLabels = (labels: readonly string[]): string => labels.join(labels.some((l) => l.includes(',')) ? '; ' : ', ');

const techNameOfSlug = (slug: string) =>
  slug
    .split('-')
    .filter(Boolean)
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(' ');

/**
 * The OSI technologies of a set of nodes, from their six-stage cells (node.yfCells; each node's
 * technologies as equals): the technology most of the nodes sit in first, then alphabetical. Nodes with no
 * basket cell add none.
 */
export function compactTechs(nodes: readonly { yfCells?: readonly string[] }[]): string[] {
  const count = new Map<string, number>();
  for (const n of nodes) {
    const own = new Set<string>();
    for (const slug of n.yfCells ?? []) {
      const c = parseCellSlug(slug);
      if (c && !c.legacy) own.add(techNameOfSlug(c.techSlug));
    }
    for (const t of own) count.set(t, (count.get(t) ?? 0) + 1);
  }
  return [...count.keys()].sort((a, b) => count.get(b)! - count.get(a)! || a.localeCompare(b, 'en'));
}

/** OSI technologies in a title: up to TITLE_TECHS joined with " / ", more as "{first} + N more" ("Electric Grid + 4 more"). */
export function techTitle(techs: readonly string[]): string {
  return techs.length <= TITLE_TECHS ? techs.join(' / ') : `${techs[0]} + ${techs.length - 1} more`;
}

/**
 * A display title for a compact, built from its structured fields, never by parsing compact.name (node
 * labels can contain ", " and ": "): "{OSI technologies}: {node labels}". The technologies are those of
 * the compact's nodes' six-stage cells (compactTechs(): node.yfCells, as equals, the one most of its nodes
 * sit in first), up to TITLE_TECHS joined with " / ", more as "{first} + N more"; a compact none of whose
 * nodes has an OSI basket cell has no prefix. The labels are the compact's nodes in its own order, joined
 * with ", " (with "; " when a label contains a comma, so "Electric buses, trucks & tractors" stays one
 * label); more than TITLE_NODES become "{first label} and N more" unless `full` ("{first label} (and N
 * more)" when that label holds a comma, so the count never reads as part of it). Nodes the document lacks
 * are skipped; with none found, the exported name. `nodes`: the document's nodes, or a lookup by id. The
 * dictionary sectors (compact.technologies, node.techs in a dictionary bundle) are not read (2026-10-29,
 * design A1: the views present nodes by OSI technology x stage).
 */
export function compactTitle(
  c: { name: string; nodes: readonly string[] },
  nodes: readonly { id: string; label: string; yfCells?: readonly string[] }[] | ((id: string) => { label: string; yfCells?: readonly string[] } | null | undefined),
  opts: { full?: boolean } = {},
): string {
  const find = typeof nodes === 'function' ? nodes : (id: string) => nodes.find((n) => n.id === id);
  const own = c.nodes.map((id) => find(id)).filter((n): n is { label: string; yfCells?: readonly string[] } => !!n);
  if (!own.length) return c.name;
  const labels = [...new Set(own.map((n) => n.label))];
  const techs = compactTechs(own);
  const more = labels.length - 1;
  const short = labels[0].includes(',') ? `${labels[0]} (and ${more} more)` : `${labels[0]} and ${more} more`;
  const list = opts.full || labels.length <= TITLE_NODES ? joinLabels(labels) : short;
  return techs.length ? `${techTitle(techs)}: ${list}` : list;
}
