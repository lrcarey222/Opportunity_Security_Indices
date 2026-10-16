// Node rows: the classification overview of every node, a compact's nodes in order, their table and
// CSV rows, and the compact's concentration (the largest optimized supplier share across its nodes).
// Pure.

import { isNum } from '../../lib/format.ts';
import { AIB_CLASSES, type AibClass, type AibCompact, type AibNode } from '../contract.ts';
import { classInfo, classOrder, type ClassInfo } from './classes.ts';
import { nameOf } from './format.ts';

/** Every technology named by the nodes, alphabetical. */
export function technologiesOf(nodes: readonly AibNode[]): string[] {
  return [...new Set(nodes.flatMap((n) => n.techs))].sort((a, b) => a.localeCompare(b, 'en'));
}

const STAGE_ORDER = ['Upstream', 'Midstream', 'Downstream'];
const stageRank = (s: string) => (STAGE_ORDER.includes(s) ? STAGE_ORDER.indexOf(s) : STAGE_ORDER.length);

/** Nodes in reading order: lost supply, largest first; then stage and label. */
export function byLostSupply(a: AibNode, b: AibNode): number {
  const la = isNum(a.lostUsd) ? a.lostUsd : -1;
  const lb = isNum(b.lostUsd) ? b.lostUsd : -1;
  return lb - la || stageRank(a.stage) - stageRank(b.stage) || a.label.localeCompare(b.label, 'en');
}

export interface ClassGroup {
  cls: string;
  info: ClassInfo;
  nodes: AibNode[];
  /** the group's lost supply (USD) */
  lostUsd: number;
}

/**
 * The nodes grouped by class in the view order (every class of the order, empty groups included, then
 * any unknown class), optionally only those of one technology. Nodes within a group by lost supply.
 */
export function nodesByClass(nodes: readonly AibNode[], tech: string | null = null): ClassGroup[] {
  const pick = tech ? nodes.filter((n) => n.techs.includes(tech)) : [...nodes];
  const classes = [...new Set<string>([...AIB_CLASSES, ...pick.map((n) => n.cls)])].sort((a, b) => classOrder(a) - classOrder(b) || a.localeCompare(b));
  return classes.map((cls) => {
    const ns = pick.filter((n) => n.cls === cls).sort(byLostSupply);
    return { cls, info: classInfo(cls), nodes: ns, lostUsd: ns.reduce((s, n) => s + (isNum(n.lostUsd) ? n.lostUsd : 0), 0) };
  });
}

/** A compact's nodes in its own order (ids the document lacks are skipped). */
export function compactNodes(compact: AibCompact, nodeOf: (id: string) => AibNode | null): AibNode[] {
  return compact.nodes.map(nodeOf).filter((n): n is AibNode => n !== null);
}

/** The node with the largest optimized supplier share, with that share and supplier (null if none). */
export function concentration(nodes: readonly AibNode[]): { node: AibNode; share: number; supplier: string | null } | null {
  let best: { node: AibNode; share: number; supplier: string | null } | null = null;
  for (const n of nodes) {
    if (!isNum(n.largestShare)) continue;
    if (!best || n.largestShare > best.share) best = { node: n, share: n.largestShare, supplier: n.largest };
  }
  return best;
}

/** True when the nodes of a compact get more than one treatment class. */
export const mixedTreatments = (nodes: readonly AibNode[]): boolean => new Set(nodes.map((n) => n.cls)).size > 1;

/** One row of the node tables and CSVs: plain values, names resolved. */
export interface NodeRow {
  id: string;
  node: string;
  technologies: string;
  stage: string;
  cls: string;
  reason: string | null;
  exposure: number | null;
  static: number | null;
  existing: number | null;
  optimized: number | null;
  dividend: number | null;
  residual: number | null;
  demandUsd: number | null;
  lostUsd: number | null;
  baselineLargest: string | null;
  baselineLargestShare: number | null;
  optimizedLargest: string | null;
  optimizedLargestShare: number | null;
  hhi: number | null;
  hosts: string;
  treatment: string | null;
  quality: string | null;
  flags: string;
  compact: string | null;
}

export function nodeRow(n: AibNode, names: Readonly<Record<string, string>>): NodeRow {
  return {
    id: n.id,
    node: n.label,
    technologies: n.techs.join('; '),
    stage: n.stage,
    cls: n.cls,
    reason: n.reason,
    exposure: n.exposure,
    static: n.static,
    existing: n.existing,
    optimized: n.optimized,
    dividend: n.dividend,
    residual: n.residual,
    demandUsd: n.demandUsd,
    lostUsd: n.lostUsd,
    baselineLargest: n.baseLargest ? nameOf(names, n.baseLargest) : null,
    baselineLargestShare: n.baseLargestShare,
    optimizedLargest: n.largest ? nameOf(names, n.largest) : null,
    optimizedLargestShare: n.largestShare,
    hhi: n.hhi,
    hosts: n.hosts.map((h) => nameOf(names, h)).join('; '),
    treatment: n.treatment,
    quality: n.quality,
    flags: n.flags.join('; '),
    compact: n.compact,
  };
}

/** The CSV columns of a node row, in plain words. */
export function nodeCsvRow(r: NodeRow): Record<string, unknown> {
  return {
    node_id: r.id,
    sub_sector: r.node,
    technologies: r.technologies,
    stage: r.stage,
    classification: r.cls,
    classification_reason: r.reason,
    shock_exposure: r.exposure,
    static_coverage: r.static,
    existing_coverage: r.existing,
    optimized_coverage: r.optimized,
    coordination_dividend: r.dividend,
    residual_gap: r.residual,
    demand_usd: r.demandUsd,
    lost_usd: r.lostUsd,
    baseline_largest_supplier: r.baselineLargest,
    baseline_largest_share: r.baselineLargestShare,
    optimized_largest_supplier: r.optimizedLargest,
    optimized_largest_share: r.optimizedLargestShare,
    optimized_hhi: r.hhi,
    investment_hosts: r.hosts,
    recommended_treatment: r.treatment,
    data_quality: r.quality,
    data_quality_flags: r.flags,
    compact: r.compact,
  };
}

/** The counts of nodes per class (every class of the order). */
export function classCounts(nodes: readonly AibNode[]): Record<AibClass, number> {
  const out = Object.fromEntries(AIB_CLASSES.map((c) => [c, 0])) as Record<AibClass, number>;
  for (const n of nodes) if (n.cls in out) out[n.cls as AibClass] += 1;
  return out;
}
