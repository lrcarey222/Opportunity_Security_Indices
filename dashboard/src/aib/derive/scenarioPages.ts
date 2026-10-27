// The scenario toggle on the Overview, the Sectoral Compacts landing, a compact's page, its node blocks and
// the Managed Interdependence explainer (page work A, 2026-10-19). Built on derive/scenarios.ts (the
// foundation's uniform figures): a compact as solved under Managed Interdependence (the scenario file's
// coalition, members, roles, metrics, path and relationships over the main document's compact), the nodes
// grouped by their class under a scenario (Managed Interdependence: the nodes it leaves unchanged apart),
// the three-way rows of the Overview, the compacts table and its CSV, a node's three-way CSV columns, and
// the plain words of a node the scenario leaves unchanged. Every set of figures weighs the same nodes in the
// three scenarios. Pure (no React, no IO).

import { isNum } from '../../lib/format.ts';
import { AIB_CLASSES, type AibCompact, type AibManagedScenarioDoc, type AibMeta, type AibNode, type AibScenarioDoc } from '../contract.ts';
import { classInfo, classOrder, type ClassInfo } from './classes.ts';
import { ALREADY_DIVERSIFIED, DIVERSIFIED, NO_BENCHMARK, WITHIN_CAP } from './diversify.ts';
import { byLostSupply, type ClassGroup } from './nodes.ts';
import {
  classMixText, compactDetail, compactScenario, comparableNodes, nodeScenario, scenarioInfo, scenarioTotals, SCENARIO_ORDER, type AibScenarioId, type CompactScenario, type NodeScenario,
  type ScenarioTotals,
} from './scenarios.ts';

// ---- A compact under Managed Interdependence ------------------------------------------------------------

/** The Managed Interdependence file of a loaded scenario file (null for any other, or none). */
export const managedOf = (d: AibScenarioDoc | null | undefined): AibManagedScenarioDoc | null => (d && d.scenario === 'managed' ? d : null);

/**
 * A compact as solved under Managed Interdependence: the main document's compact (same id, nodes, sectors
 * and stages) with the scenario file's type, archetype, policy needs and secondary class (from the
 * scenario's classes, compact gain and coalition size; an older file without them keeps the main
 * document's), coalition, demand anchors, suppliers, hosts, technology and bridge partners, associates,
 * roles, metrics, path and trade and investment relationships, and the class mix of compact.managed. Null
 * when the file has no entry for it or no coalition (the scenario ran on none of its nodes: compactDetail()
 * gives the reason) or no metrics.
 */
export function managedCompact(c: AibCompact, managed: AibManagedScenarioDoc | null | undefined): AibCompact | null {
  const d = compactDetail(c, 'managed', managed);
  if (d.status !== 'ready' || !d.coalition || !d.metrics) return null;
  const { coalition, anchors, suppliers, hosts, technologyPartners, bridges, associates, roles, metrics, path, trade, investment } = d;
  const m = managed?.compacts.find((x) => x.id === c.id);
  const typed = Boolean(m?.type);
  return {
    ...c,
    type: m?.type ?? c.type,
    archetype: m?.archetype ?? c.archetype,
    policyNeeds: typed ? (m?.policyNeeds ?? []) : c.policyNeeds,
    coalition,
    anchors,
    suppliers,
    hosts,
    technologyPartners,
    bridges,
    associates,
    roles,
    metrics,
    path,
    trade,
    investment,
    classMix: c.managed?.classMix ?? null,
    secondaryClass: typed ? (m?.secondaryClass ?? null) : null,
  };
}

/**
 * Why Managed Interdependence leaves a compact unchanged (it ran on none of its nodes), from its nodes'
 * statuses in the scenario (null: not assessed). The words of the scenario file's coalitionReason (the R
 * mirror is aib_scenario_coalition_reason in R/aib/aib_compacts.R), so the pages say the same before the
 * file is loaded.
 */
export function compactUnchangedReason(statuses: readonly (string | null | undefined)[]): string {
  const s = [...new Set(statuses.filter((x): x is string => typeof x === 'string'))];
  if (!s.length) return 'the scenario did not assess any node of this compact';
  if (s.every((x) => x === ALREADY_DIVERSIFIED)) return 'no node of this compact is above its benchmark by more than a small tolerance';
  if (s.every((x) => x === NO_BENCHMARK)) return 'no node of this compact has a concentration benchmark';
  if (s.every((x) => x === ALREADY_DIVERSIFIED || x === WITHIN_CAP || x === NO_BENCHMARK)) return 'no node of this compact has a supplier above its benchmark cap in the allied market';
  return 'the scenario did not run on any node of this compact';
}

/** The same for a compact's nodes (node.diversify.status; `nodes`: those the document has). */
export const compactUnchangedReasonOf = (nodes: readonly AibNode[]): string => compactUnchangedReason(nodes.map((n) => n.diversify?.status ?? null));

/** The compacts solved under Managed Interdependence (those with a coalition), in the main document's order. */
export function managedCompacts(compacts: readonly AibCompact[], managed: AibManagedScenarioDoc | null | undefined): AibCompact[] {
  return compacts.map((c) => managedCompact(c, managed)).filter((c): c is AibCompact => c !== null);
}

// ---- Nodes under a scenario --------------------------------------------------------------------------

/** The key of the group of nodes a scenario leaves unchanged (Managed Interdependence: nothing to shift). */
export const UNCHANGED = 'UNCHANGED';

/** Why Managed Interdependence leaves a node unchanged, in a few words (the coverage rows' note). */
export function unchangedWords(status: string | null): string {
  switch (status) {
    case ALREADY_DIVERSIFIED:
      return 'unchanged: already as diversified as its benchmark';
    case WITHIN_CAP:
      return 'unchanged: no supplier to the allied market is above the cap';
    case NO_BENCHMARK:
      return 'unchanged: no benchmark to compare it with';
    case null:
      return 'unchanged: not assessed in this scenario';
    default:
      return status === DIVERSIFIED ? '' : `unchanged: ${status}`;
  }
}

/** The note of a node's Managed Interdependence row where the scenario did not run (null where it ran, and for the other scenarios). */
export function nodeScenarioNote(r: Pick<NodeScenario, 'id' | 'ran' | 'status' | 'split'>): string | null {
  if (r.id !== 'managed' || r.ran) return null;
  return unchangedWords(r.status);
}

/** A node's figures under every scenario as a map (the toggled one is read from it). */
export function nodeScenarioMap(n: AibNode): Record<AibScenarioId, NodeScenario> {
  return Object.fromEntries(SCENARIO_ORDER.map((id) => [id, nodeScenario(n, id)])) as Record<AibScenarioId, NodeScenario>;
}

/** A group of nodes by their class under a scenario (a ClassGroup; `unchanged`: the nodes the scenario leaves as they are). */
export interface ScenarioClassGroup extends ClassGroup {
  unchanged: boolean;
}

const UNCHANGED_INFO: ClassInfo = {
  ...classInfo('INSUFFICIENT DATA'),
  label: 'Unchanged',
  meaning: 'Not more concentrated than its benchmark, no supplier above the cap in the allied market, or no benchmark: Managed Interdependence leaves it as it is.',
};

/**
 * The nodes grouped by their class under a scenario, in the class order (every class of the order, empty
 * groups included), optionally only those of one technology (sector); each group's lost (or shifted)
 * supply is the scenario's. Decoupling: the headline classes. Managed Interdependence: the class under the
 * scenario where it ran, then the unchanged nodes in their own group. Nodes within a group by that lost
 * supply, largest first.
 */
export function scenarioClassGroups(nodes: readonly AibNode[], id: AibScenarioId, tech: string | null = null): ScenarioClassGroup[] {
  const pick = tech ? nodes.filter((n) => n.techs.includes(tech)) : [...nodes];
  const rows = pick.map((n) => nodeScenario(n, id));
  const lost = new Map(rows.map((r) => [r.node.id, isNum(r.lostUsd) ? r.lostUsd : 0]));
  const order = (a: AibNode, b: AibNode) => (lost.get(b.id) ?? 0) - (lost.get(a.id) ?? 0) || byLostSupply(a, b);
  const keyOf = (r: NodeScenario): string => (id === 'managed' ? (r.ran && r.cls ? r.cls : UNCHANGED) : id === 'baseline' ? UNCHANGED : r.node.cls);
  const keys = [...new Set<string>([...AIB_CLASSES, ...rows.map(keyOf).filter((k) => k !== UNCHANGED)])].sort((a, b) => classOrder(a) - classOrder(b) || a.localeCompare(b));
  if (rows.some((r) => keyOf(r) === UNCHANGED)) keys.push(UNCHANGED);
  return keys.map((k) => {
    const ns = rows.filter((r) => keyOf(r) === k).map((r) => r.node).sort(order);
    return { cls: k, info: k === UNCHANGED ? UNCHANGED_INFO : classInfo(k), nodes: ns, lostUsd: ns.reduce((s, n) => s + (lost.get(n.id) ?? 0), 0), unchanged: k === UNCHANGED };
  });
}

/** The nodes with the largest value of a scenario figure (where the scenario ran and the value is known), largest first. */
export function topScenarioNodes(rows: readonly NodeScenario[], key: (r: NodeScenario) => number | null, n = 5): NodeScenario[] {
  return rows
    .filter((r) => r.ran && isNum(key(r)))
    .sort((a, b) => (key(b) as number) - (key(a) as number) || a.node.label.localeCompare(b.node.label, 'en'))
    .slice(0, n);
}

// ---- The Overview's three-way rows ------------------------------------------------------------------

/** One scenario's totals as the Overview reads them (ScenarioTotals plus restored-by-trade USD and the class mix in words). */
export interface OverviewRow extends ScenarioTotals {
  label: string;
  /** what optimized trade restores, USD a year: each node's restored-by-trade share times its demand, summed */
  restoredUsd: number;
  /** the classes' shares of the lost (or shifted) supply ("BUILD 67% · LOW INTERVENTION 33%"; "" when nothing is lost) */
  classMix: string;
  /** the nodes that lose or shift supply */
  affectedNodes: number;
}

/** What optimized trade restores over some nodes under a scenario, USD a year (Baseline: 0). */
export function restoredUsd(rows: readonly NodeScenario[]): number {
  return rows.reduce((s, r) => s + (r.split && isNum(r.demandUsd) ? r.split.trade * r.demandUsd : 0), 0);
}

/** The three scenarios over a set of nodes (the comparable ones, the same in all three), in display order. */
export function overviewRows(nodes: readonly AibNode[], meta?: AibMeta | null): OverviewRow[] {
  const comparable = comparableNodes(nodes);
  return SCENARIO_ORDER.map((id) => {
    const t = scenarioTotals(nodes, id);
    const rows = comparable.map((n) => nodeScenario(n, id));
    return {
      ...t,
      label: scenarioInfo(meta, id).label,
      restoredUsd: restoredUsd(rows),
      classMix: classMixText(t.byClass.map((g) => ({ cls: g.cls, usd: g.lostUsd }))),
      affectedNodes: rows.filter((r) => isNum(r.lostUsd) && r.lostUsd > 0).length,
    };
  });
}

// ---- Compacts -----------------------------------------------------------------------------------------

/** A compact's lost (or shifted) supply under a scenario, for ordering (unknown last). */
export function compactLostUnder(c: AibCompact, id: AibScenarioId, nodeOf?: ((id: string) => AibNode | null) | null): number | null {
  const v = compactScenario(c, id, nodeOf).lostUsd;
  return isNum(v) ? v : null;
}

/**
 * The compacts in the order a scenario reads them: its lost (or shifted) supply, largest first (a compact
 * the scenario leaves unchanged last), then the document's order. Decoupling keeps the export's order
 * (already by the supply the shock removes) where the values tie.
 */
export function compactsByScenario(compacts: readonly AibCompact[], id: AibScenarioId, nodeOf?: ((id: string) => AibNode | null) | null): AibCompact[] {
  const lost = new Map(compacts.map((c) => [c.id, compactLostUnder(c, id, nodeOf)]));
  const pos = new Map(compacts.map((c, i) => [c.id, i]));
  const v = (c: AibCompact) => lost.get(c.id) ?? -1;
  return [...compacts].sort((a, b) => v(b) - v(a) || (pos.get(a.id) ?? 0) - (pos.get(b.id) ?? 0));
}

/** One compact's row of the compacts table and its CSV: its figures under the three scenarios. */
export interface CompactScenarioRow {
  compact: AibCompact;
  baseline: CompactScenario;
  managed: CompactScenario;
  decoupling: CompactScenario;
}

export function compactScenarioRows(compacts: readonly AibCompact[], nodeOf?: ((id: string) => AibNode | null) | null): CompactScenarioRow[] {
  return compacts.map((c) => ({
    compact: c,
    baseline: compactScenario(c, 'baseline', nodeOf),
    managed: compactScenario(c, 'managed', nodeOf),
    decoupling: compactScenario(c, 'decoupling', nodeOf),
  }));
}

/** The CSV row of a compact under the three scenarios (plain values; `title`: its display title). */
export function compactScenarioCsvRow(r: CompactScenarioRow, title: string): Record<string, unknown> {
  const c = r.compact;
  const part = (p: string, s: CompactScenario) => ({
    [`${p}_lost_or_shifted_usd`]: s.lostUsd,
    [`${p}_share_lost_or_shifted`]: s.exposure,
    [`${p}_kept`]: s.split?.kept ?? null,
    [`${p}_restored_by_trade`]: s.split?.trade ?? null,
    [`${p}_new_capacity`]: s.split?.build ?? null,
    [`${p}_gap_usd`]: s.residualUsd,
    [`${p}_class_mix`]: s.classMix,
  });
  return {
    compact: c.id,
    title,
    type: c.type,
    nodes: c.nodes.length,
    allied_demand_usd: r.decoupling.demandUsd ?? r.baseline.demandUsd,
    baseline_hhi: r.baseline.hhi,
    baseline_top_suppliers: r.baseline.suppliers.map(([iso, s]) => `${iso} ${isNum(s) ? Math.round(s * 1000) / 10 : ''}%`).join('; '),
    managed_nodes_run: r.managed.nodesRun,
    ...part('managed', r.managed),
    ...part('decoupling', r.decoupling),
  };
}

// ---- A node's three-way CSV columns ---------------------------------------------------------------------

/** A node's figures under Baseline and Managed Interdependence as CSV columns (Decoupling: the node's own columns). */
export function nodeScenarioCsv(n: AibNode): Record<string, unknown> {
  const b = nodeScenario(n, 'baseline');
  const m = nodeScenario(n, 'managed');
  return {
    baseline_hhi: b.hhi,
    baseline_top3_share: b.top3Share,
    managed_status: m.status,
    managed_ran: m.ran,
    managed_class: m.ran ? m.cls : null,
    managed_share_shifted: m.ran ? m.exposure : null,
    managed_shifted_usd: m.ran ? m.lostUsd : null,
    managed_static_coverage: m.ran ? m.static : null,
    managed_existing_coverage: m.ran ? m.existing : null,
    managed_optimized_coverage: m.ran ? m.optimized : null,
    managed_gap_usd: m.ran ? m.residualUsd : null,
    managed_optimized_hhi: m.ran ? m.hhi : null,
  };
}

/** The scenario figures of a node the pages print beside its label: the toggled scenario's class, or "Unchanged". */
export function scenarioClassOf(r: NodeScenario): string | null {
  if (r.id === 'baseline') return null;
  if (r.id === 'managed') return r.ran ? r.cls : null;
  return r.cls;
}
