// The supply-chain navigator (IA refactor 2026-10-16, brief s.7): a technology's stages with their
// trade sub-components (sector trade chunk, ./subs.ts) and the Allied Industrial Base nodes of the
// same cell (AIB bundle nodes[].yfCells = "<tech>-<stage>", yfSubcomponents = "<tech>-<stage>::<name>").
// Pure; nothing here computes a new measure, it only lines the two taxonomies up by name.
//
//   * a node matches a sub-component when one of its names (its yfSubcomponents for the cell, else its
//     label) equals the sub-component's name, ignoring case; failing that, when it equals one part of
//     the name split on "&" and "," ("Polysilicon & Wafers" holds the nodes "Polysilicon" and "Wafers");
//   * a node matches at most one sub-component; nodes with no match are listed under their stage.

import type { AibNode } from '../../aib/contract.ts';

/** The parts of a sub-component name: "Racking, Mounting & Trackers" -> racking, mounting, trackers. */
export function subParts(name: string): string[] {
  return name
    .split(/[&,]/)
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
}

/** The AIB nodes of one sector cell ("solar-midstream"). */
export function nodesOfCell(nodes: readonly AibNode[], cellSlug: string): AibNode[] {
  return nodes.filter((n) => n.yfCells.includes(cellSlug));
}

/** The names a node goes by in one cell: its yfSubcomponents for that cell, else its label (lower case). */
export function nodeNames(n: AibNode, cellSlug: string): string[] {
  const pre = `${cellSlug}::`;
  const own = n.yfSubcomponents.filter((s) => s.startsWith(pre)).map((s) => s.slice(pre.length).trim().toLowerCase());
  return own.length ? own : [n.label.trim().toLowerCase()];
}

export interface CellNodeMatch {
  /** sub-component name -> its nodes (in the nodes' order) */
  bySub: Map<string, AibNode[]>;
  /** nodes of the cell matching no sub-component */
  unmatched: AibNode[];
}

/** Line the AIB nodes of a cell up with the cell's sub-components (exact names first, then name parts). */
export function matchNodes(subs: readonly string[], nodes: readonly AibNode[], cellSlug: string): CellNodeMatch {
  const bySub = new Map<string, AibNode[]>(subs.map((s) => [s, []]));
  const unmatched: AibNode[] = [];
  for (const n of nodesOfCell(nodes, cellSlug)) {
    const names = nodeNames(n, cellSlug);
    const exact = subs.find((s) => names.includes(s.trim().toLowerCase()));
    const part = exact ?? subs.find((s) => subParts(s).some((p) => names.includes(p)));
    if (part !== undefined) bySub.get(part)!.push(n);
    else unmatched.push(n);
  }
  return { bySub, unmatched };
}

/** True when a node's name differs from the sub-component it sits under (so the view names it). */
export function nodeNamedApart(n: AibNode, sub: string): boolean {
  return n.label.trim().toLowerCase() !== sub.trim().toLowerCase();
}
