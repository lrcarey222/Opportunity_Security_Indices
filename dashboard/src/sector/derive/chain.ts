// The supply-chain navigator (IA refactor 2026-10-16, brief s.7): a technology's stages with their
// trade sub-components (sector trade chunk, ./subs.ts) and the Allied Industrial Base nodes of the
// same cell (AIB bundle nodes[].yfCells = "<tech>-<stage>", yfSubcomponents = "<tech>-<stage>::<name>";
// a three-stage cell through the nodes' three-stage cells, see Stages below).
// Pure; nothing here computes a new measure, it only lines the two taxonomies up.
//
// The AIB nodes reach a cell through the OSI crosswalk, never through their own sector or technology: an
// electro-industrial dictionary node ("Rare-earth permanent magnets", sector Magnets) sits in every OSI
// cell its codes sit in (its crosswalk cells: Electric Vehicles and Wind Midstream at three stages,
// Electric Vehicles and Wind Components at six), and in every OSI sub-component of those cells its codes
// belong to (its crosswalk sub-components). The mapping is many-to-many: one node can sit under several
// sub-components of a cell and one sub-component hold several nodes.
//
//   * a node that names its own sub-components for the cell (its crosswalk sub-components) sits under each of them
//     whose name matches, ignoring case;
//   * otherwise a node goes by its names (those sub-components, else its label in an older bundle): the
//     sub-component of the same name, failing that one whose name split on "&" and "," has one of them as a
//     part ("Polysilicon & Wafers" holds the nodes "Polysilicon" and "Wafers"), at most one;
//   * nodes with no match are listed under their stage.
//
// Stages (2026-10-28): the AIB bundle is six-stage (node.stage Refining, node.yfCells
// "batteries-refining"), while the Sector bundle keeps the paper's three stages (cells
// "batteries-midstream"). A three-stage cell is therefore joined through each node's three-stage cells
// and sub-components (node.yfCellsLegacy / yfSubcomponentsLegacy: the stage the paper filed the node's
// codes under, written by the AIB export since 2026-10-28), read with legacyCellsOf() /
// legacySubcomponentsOf() of ../../lib/stages.ts. In a bundle without them the join falls back to the
// roll-up of the node's six-stage cells, which is approximate: Refining rolls up to Upstream, while the
// paper filed refined battery materials, polysilicon and electrical steel under Midstream, so those
// nodes would land on the Upstream cell. A six-stage cell slug (for a Sector bundle at six stages) is
// joined through yfCells / yfSubcomponents directly. The node's own stage, as the views show it, stays
// six-stage.

import type { AibNode } from '../../aib/contract.ts';
import { isLegacyCellSlug, legacyCellsOf, legacySubcomponentsOf } from '../../lib/stages.ts';

/** The parts of a sub-component name: "Racking, Mounting & Trackers" -> racking, mounting, trackers. */
export function subParts(name: string): string[] {
  return name
    .split(/[&,]/)
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
}

/**
 * A node's cells in the form of a cell slug: its three-stage cells for a three-stage slug
 * ("solar-midstream"; yfCellsLegacy, else the approximate roll-up of yfCells), its six-stage cells
 * (yfCells) otherwise.
 */
export function nodeCellsFor(n: Pick<AibNode, 'yfCells' | 'yfCellsLegacy'>, cellSlug: string): readonly string[] {
  return isLegacyCellSlug(cellSlug) ? legacyCellsOf(n) : n.yfCells;
}

/** The same for a node's sub-component ids ("<cell>::<name>"): yfSubcomponentsLegacy or its roll-up for a three-stage slug, else yfSubcomponents. */
export function nodeSubcomponentsFor(n: Pick<AibNode, 'yfSubcomponents' | 'yfSubcomponentsLegacy'>, cellSlug: string): readonly string[] {
  return isLegacyCellSlug(cellSlug) ? legacySubcomponentsOf(n) : n.yfSubcomponents;
}

/**
 * The AIB nodes of one sector cell ("solar-midstream"): those with codes in it (the crosswalk; for a
 * three-stage cell, the nodes' three-stage cells), each once, in the nodes' order.
 */
export function nodesOfCell(nodes: readonly AibNode[], cellSlug: string): AibNode[] {
  return nodes.filter((n) => nodeCellsFor(n, cellSlug).includes(cellSlug));
}

/** The OSI sub-components a node names for one cell (its sub-component ids there, lower case, each once; [] when none). */
export function ownSubcomponents(n: AibNode, cellSlug: string): string[] {
  const pre = `${cellSlug}::`;
  const names = nodeSubcomponentsFor(n, cellSlug)
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
