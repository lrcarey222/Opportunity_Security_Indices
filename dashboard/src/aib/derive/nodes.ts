// Node rows: the classification overview of every node (by default only the nodes in the strategic
// scope), a compact's nodes in order, their table and CSV rows (with the physical-capability flag of every
// class they carry and the growth-bound flag), and the compact's concentration (the largest optimized
// supplier share across its nodes). Pure.

import { isNum } from '../../lib/format.ts';
import { AIB_CLASSES, inStrategicScope, type AibClass, type AibCompact, type AibNode } from '../contract.ts';
import { classInfo, classOrder, policyNeedsText, type ClassInfo } from './classes.ts';
import { nameOf } from './format.ts';
import { constraintClasses, growthFlag, warnedGrowthBounds, warnedVariants } from './sensitivity.ts';
import { nodeInCell, nodeStageRank, osiFields, technologiesOfNodes, type Taxonomy } from './taxonomy.ts';

/** The nodes of the strategic scope, or every node with `all`. */
export function scopedNodes(nodes: readonly AibNode[], all = false): AibNode[] {
  return all ? [...nodes] : nodes.filter(inStrategicScope);
}

/** How many nodes lie outside the strategic scope. */
export const outOfScopeCount = (nodes: readonly AibNode[]): number => nodes.filter((n) => !inStrategicScope(n)).length;

/**
 * Every OSI technology the nodes' six-stage cells sit in, alphabetical, then 'Not in the OSI basket' when a
 * node has no basket cell (taxonomy.ts technologiesOfNodes(): a node's technologies are equals).
 */
export function technologiesOf(nodes: readonly AibNode[]): string[] {
  return technologiesOfNodes(nodes);
}

/** Nodes in reading order: lost supply, largest first; then stage (chain order, of their earliest cell; no cell last) and label. */
export function byLostSupply(a: AibNode, b: AibNode): number {
  const la = isNum(a.lostUsd) ? a.lostUsd : -1;
  const lb = isNum(b.lostUsd) ? b.lostUsd : -1;
  return lb - la || nodeStageRank(a) - nodeStageRank(b) || a.label.localeCompare(b.label, 'en');
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
 * any unknown class), optionally only those with a cell of one OSI technology and / or at one stage
 * (taxonomy.ts nodeInCell(): a node in several cells is in each; 'Not in the OSI basket': the nodes with no
 * basket cell, under no stage). Nodes within a group by lost supply.
 */
export function nodesByClass(nodes: readonly AibNode[], tech: string | null = null, stage: string | null = null): ClassGroup[] {
  const pick = nodes.filter((n) => nodeInCell(n, tech, stage));
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
  /** its OSI technologies, "; "-joined (the technologies of its six-stage cells, as equals), or 'Not in the OSI basket' */
  technologies: string;
  /** the OSI cells its codes sit in ("; "-joined six-stage crosswalk ids; "" when none) */
  osiCells: string;
  /** its electro-industrial dictionary sector (provenance; null in a master bundle or without the taxonomy) */
  dictionarySector: string | null;
  /** the stages of its OSI cells, "; "-joined in chain order ("" when none) */
  stage: string;
  /** the AIB model's own stage for it (node.stage; it can differ from the stages of its cells) */
  modelStage: string;
  cls: string;
  reason: string | null;
  /** the multi-label policy needs in words ("BUILD + MANAGE SPECIALIZATION", "none"; null in an older bundle) */
  policyNeeds: string | null;
  warning: string | null;
  physicalValidation: string | null;
  physicalData: string | null;
  inStrategicScope: boolean;
  /** the class under production-constrained and trade-only supply, and whether each carries the warning */
  productionConstrainedCls: string | null;
  tradeOnlyCls: string | null;
  productionConstrainedWarning: boolean | null;
  tradeOnlyWarning: boolean | null;
  /** the variants / growth bounds whose class carries the warning, "; "-joined ("" when none; null in an older bundle) */
  warnedVariants: string | null;
  warnedGrowthBounds: string | null;
  /** COORDINATE under some existing-relationship growth bounds only (null in an older bundle) */
  coordinateSensitiveToGrowth: boolean | null;
  /** that flag in words, from the headline class (null when not flagged) */
  growthFlag: string | null;
  n1Reoptimized: number | null;
  instantaneousN1: number | null;
  diversion: number | null;
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
  /** the leading scaling hosts, then the specialty and development hosts ('' when none or not exported) */
  hosts: string;
  specialtyHosts: string;
  developmentHosts: string;
  treatment: string | null;
  quality: string | null;
  flags: string;
  compact: string | null;
}

/** A node's table and CSV row; `tx` (the bundle's taxonomy) adds the dictionary sector of a dictionary node (provenance). */
export function nodeRow(n: AibNode, names: Readonly<Record<string, string>>, tx?: Taxonomy): NodeRow {
  const modes = constraintClasses(n);
  const num = (x: number | null | undefined) => (isNum(x) ? x : null);
  const place = osiFields({ dictionary: !!tx?.dictionary }, n);
  return {
    id: n.id,
    node: n.label,
    technologies: place.technologies,
    osiCells: place.osiCells,
    dictionarySector: place.dictionarySector,
    stage: place.stages,
    modelStage: place.modelStage,
    cls: n.cls,
    reason: n.reason,
    policyNeeds: policyNeedsText(n.policyNeeds),
    warning: n.warning ?? null,
    physicalValidation: n.physicalValidation ?? null,
    physicalData: n.physicalData ?? null,
    inStrategicScope: inStrategicScope(n),
    productionConstrainedCls: modes?.productionConstrained ?? null,
    tradeOnlyCls: modes?.tradeOnly ?? null,
    productionConstrainedWarning: modes?.productionConstrainedWarning ?? null,
    tradeOnlyWarning: modes?.tradeOnlyWarning ?? null,
    warnedVariants: warnedVariants(n)?.join('; ') ?? null,
    warnedGrowthBounds: warnedGrowthBounds(n)?.join('; ') ?? null,
    coordinateSensitiveToGrowth: typeof n.coordinateSensitiveToGrowth === 'boolean' ? n.coordinateSensitiveToGrowth : null,
    growthFlag: growthFlag(n),
    n1Reoptimized: num(n.n1Reoptimized),
    instantaneousN1: num(n.instantaneousN1),
    diversion: num(n.diversion),
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
    specialtyHosts: (n.specialtyHosts ?? []).map((h) => nameOf(names, h)).join('; '),
    developmentHosts: (n.developmentHosts ?? []).map((h) => nameOf(names, h)).join('; '),
    treatment: n.treatment,
    quality: n.quality,
    flags: n.flags.join('; '),
    compact: n.compact,
  };
}

/**
 * The CSV columns of a node row, in plain words: its OSI technologies (or 'Not in the OSI basket') and the
 * six-stage OSI cells its codes sit in, then its electro-industrial dictionary sector as provenance (empty
 * in a master bundle), the stages of its cells and the AIB model's own stage for it (aib_model_stage).
 */
export function nodeCsvRow(r: NodeRow): Record<string, unknown> {
  return {
    node_id: r.id,
    sub_sector: r.node,
    technologies: r.technologies,
    osi_cells: r.osiCells,
    dictionary_sector: r.dictionarySector,
    stage: r.stage,
    aib_model_stage: r.modelStage,
    classification: r.cls,
    classification_reason: r.reason,
    policy_needs: r.policyNeeds,
    classification_warning: r.warning,
    physical_validation: r.physicalValidation,
    physical_data: r.physicalData,
    in_strategic_scope: r.inStrategicScope,
    class_production_constrained: r.productionConstrainedCls,
    class_production_constrained_warning: r.productionConstrainedWarning,
    class_trade_only: r.tradeOnlyCls,
    class_trade_only_warning: r.tradeOnlyWarning,
    warning_under_variants: r.warnedVariants,
    warning_under_growth_bounds: r.warnedGrowthBounds,
    coordinate_sensitive_to_growth: r.coordinateSensitiveToGrowth,
    growth_bound_flag: r.growthFlag,
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
    n1_reoptimized_coverage: r.n1Reoptimized,
    instantaneous_n1_retained_coverage: r.instantaneousN1,
    diversion_share_of_demand: r.diversion,
    investment_hosts: r.hosts,
    specialty_hosts: r.specialtyHosts,
    development_hosts: r.developmentHosts,
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
