// The supply-chain navigator (IA refactor 2026-10-16, brief s.7): a technology's six stages with their
// trade sub-components (sector trade chunk, ./subs.ts) and the Allied Industrial Base nodes of the
// same cell (AIB bundle nodes[].yfCells = "<tech>-<stage>", yfSubcomponents = "<tech>-<stage>::<name>").
// Pure; nothing here computes a new measure, it only lines the two taxonomies up.
//
// The AIB nodes reach a cell through the OSI crosswalk, never through their own sector or technology: an
// electro-industrial dictionary node ("Rare-earth permanent magnets", sector Magnets) sits in every OSI
// cell its codes sit in (its crosswalk cells: Electric Vehicles and Wind Components), and in every OSI
// sub-component of those cells its codes belong to (its crosswalk sub-components). The mapping is
// many-to-many: one node can sit under several sub-components of a cell and one sub-component hold
// several nodes.
//
//   * a node that names its own sub-components for the cell (its crosswalk sub-components) sits under each of them
//     whose name matches, ignoring case;
//   * otherwise a node goes by its names (those sub-components, else its label in an older bundle): the
//     sub-component of the same name, failing that one whose name split on "&" and "," has one of them as a
//     part ("Polysilicon & Wafers" holds the nodes "Polysilicon" and "Wafers"), at most one;
//   * nodes with no match are listed under their stage.
//
// Stages (2026-10-28, design A2): the Sector cells and the AIB nodes are both six-stage, so a cell is
// joined through the nodes' six-stage cells and sub-components (node.yfCells / yfSubcomponents) directly.
// The nodes' three-stage cells (node.yfCellsLegacy, kept optional in the AIB contract for older bundles)
// are not read. A node's own stage (node.stage) can differ from the stage of a cell its codes sit in.

import type { AibNode } from '../../aib/contract.ts';

/** The parts of a sub-component name: "Racking, Mounting & Trackers" -> racking, mounting, trackers. */
export function subParts(name: string): string[] {
  return name
    .split(/[&,]/)
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
}

/** The AIB nodes of one sector cell ("solar-assembly"): those with codes in it (node.yfCells), each once, in the nodes' order. */
export function nodesOfCell(nodes: readonly AibNode[], cellSlug: string): AibNode[] {
  return nodes.filter((n) => n.yfCells.includes(cellSlug));
}

/** The OSI sub-components a node names for one cell (its sub-component ids there, lower case, each once; [] when none). */
export function ownSubcomponents(n: AibNode, cellSlug: string): string[] {
  const pre = `${cellSlug}::`;
  const names = n.yfSubcomponents
    .filter((s) => s.startsWith(pre))
    .map((s) => s.slice(pre.length).trim().toLowerCase());
  return [...new Set(names)];
}

/** The names a node goes by in one cell: its own sub-components for that cell (ownSubcomponents), else its label (lower case). */
export function nodeNames(n: AibNode, cellSlug: string): string[] {
  const own = ownSubcomponents(n, cellSlug);
  return own.length ? own : [n.label.trim().toLowerCase()];
}

export interface CellNodeMatch {
  /** sub-component name -> its nodes (in the nodes' order); a node can sit under several */
  bySub: Map<string, AibNode[]>;
  /** nodes of the cell matching no sub-component */
  unmatched: AibNode[];
}

/**
 * Line the AIB nodes of a cell up with the cell's sub-components: a node under every sub-component its
 * own crosswalk ids name; a node without them by its label (exact name first, then name parts, one).
 */
export function matchNodes(subs: readonly string[], nodes: readonly AibNode[], cellSlug: string): CellNodeMatch {
  const bySub = new Map<string, AibNode[]>(subs.map((s) => [s, []]));
  const unmatched: AibNode[] = [];
  const key = (s: string) => s.trim().toLowerCase();
  for (const n of nodesOfCell(nodes, cellSlug)) {
    const names = nodeNames(n, cellSlug);
    // its own sub-components: every one named; else (or when none is a sub-component of the chunk) by name, one
    let hits = ownSubcomponents(n, cellSlug).length ? subs.filter((s) => names.includes(key(s))) : [];
    if (!hits.length) {
      const exact = subs.find((s) => names.includes(key(s)));
      const part = exact ?? subs.find((s) => subParts(s).some((p) => names.includes(p)));
      hits = part !== undefined ? [part] : [];
    }
    if (hits.length) for (const s of hits) bySub.get(s)!.push(n);
    else unmatched.push(n);
  }
  return { bySub, unmatched };
}

/** The sub-components of a cell each node sits under (node id -> names, in the cell's order). */
export function subsOfNodes(m: CellNodeMatch): Map<string, string[]> {
  const out = new Map<string, string[]>();
  for (const [sub, ns] of m.bySub) for (const n of ns) out.set(n.id, [...(out.get(n.id) ?? []), sub]);
  return out;
}

/** True when a node's name differs from the sub-component it sits under (so the view names it). */
export function nodeNamedApart(n: AibNode, sub: string): boolean {
  return n.label.trim().toLowerCase() !== sub.trim().toLowerCase();
}
