// Allied Industrial Base partnerships and overview (IA refactor, 2026-10-16): the model's trade and
// investment relationships gathered across every compact and scoped by economy / technology / stage /
// sub-sector; the eligible investment hosts and specialized suppliers of the nodes; the overview's class
// counts and top nodes; the baseline top partners of the `ties` chunk. No new score: every number is one
// the export or the ties chunk already carries (sums of the model's own flows at most). Pure.
// Stages (2026-10-28): the filter's stage and the nodes are six-stage (Extraction ... Recovery, in chain
// order); the partner data the filter is read against (the ties and pcell chunks, the engine's cells, the
// aid of the Sector data) are still kept by the paper's three-stage cells, so those lookups read the
// matching nodes' three-stage cells (taxonomy.ts legacyOsiCellsOfNode()).

import { isNum, stageSlug } from '../../lib/format.ts';
import { byStageRank, isLegacyStage, LEGACY_STAGES, rollUpStage, type LegacyStage } from '../../lib/stages.ts';
import {
  coalitionMembersOf, type AibBaselineScenarioDoc, type AibDoc, type AibInvestmentEdge, type AibInvestmentFlows, type AibManagedScenarioDoc, type AibNetwork, type AibNode, type AibRelationship, type AibRole,
  type AibTradeEdge,
} from '../contract.ts';
import { compactDetail, nodeScenario, nodeSuppliersToday } from './scenarios.ts';
import { legacyOsiCellsOfNode, type OsiCell } from './taxonomy.ts';

export interface PartnershipFilter {
  /** an economy (ISO3) that must take part */
  country: string | null;
  tech: string | null;
  stage: string | null;
  /** a node id */
  node: string | null;
}

export const NO_FILTER: PartnershipFilter = { country: null, tech: null, stage: null, node: null };

/** The node passes the technology / stage / sub-sector part of the filter. */
export function nodeInScope(n: AibNode, f: PartnershipFilter): boolean {
  if (f.node && n.id !== f.node) return false;
  if (f.tech && !n.techs.includes(f.tech)) return false;
  if (f.stage && n.stage !== f.stage) return false;
  return true;
}

/**
 * The filter's choices from the document: technologies, stages (the nodes' own, in chain order: no
 * Deployment, which no AIB node has), nodes (scoped by tech / stage), economies.
 */
export function filterChoices(doc: Pick<AibDoc, 'nodes' | 'compacts' | 'countries'>, f: PartnershipFilter) {
  const techs = [...new Set(doc.nodes.flatMap((n) => n.techs))].sort((a, b) => a.localeCompare(b, 'en'));
  const stages = [...new Set(doc.nodes.map((n) => n.stage))].sort(byStageRank);
  const nodes = doc.nodes
    .filter((n) => nodeInScope(n, { ...f, node: null }))
    .sort((a, b) => a.label.localeCompare(b.label, 'en'));
  const iso = new Set<string>();
  for (const c of doc.compacts) {
    for (const e of c.trade) iso.add(e.from).add(e.to);
    for (const e of c.investment) iso.add(e.investor).add(e.host);
    for (const r of c.roles) iso.add(r.iso3);
  }
  for (const n of doc.nodes) for (const h of n.hosts) iso.add(h);
  const countries = [...iso].map((x) => ({ iso3: x, name: doc.countries[x] ?? x })).sort((a, b) => a.name.localeCompare(b.name, 'en'));
  return { techs, stages, nodes, countries };
}

export interface PairTrade {
  producer: string;
  consumer: string;
  /** added trade (USD a year), summed over the nodes and compacts in scope */
  value: number;
  relationship: AibRelationship;
  nodes: string[];
  compacts: string[];
  /** the largest share of one node's demand the relationship serves */
  share: number | null;
  /** the mean Export Opportunity score of its edges (null when none carries one) */
  opportunity: number | null;
}

/** Every exporter -> importer relationship (by new / expanded) across the compacts, largest first. */
export function pairTradeFlows(doc: Pick<AibDoc, 'nodes' | 'compacts'>, f: PartnershipFilter): PairTrade[] {
  return pairTradeFlowsOf(doc.nodes, doc.compacts, f);
}

/** pairTradeFlows over any compacts' trade (the main document's, or a scenario's: scenarioCompacts()). */
export function pairTradeFlowsOf(nodeList: readonly AibNode[], compacts: readonly { id: string; trade: readonly AibTradeEdge[] }[], f: PartnershipFilter): PairTrade[] {
  const nodes = new Map(nodeList.map((n) => [n.id, n]));
  const acc = new Map<string, PairTrade & { oppSum: number; oppN: number }>();
  for (const c of compacts) {
    for (const e of c.trade) {
      const n = nodes.get(e.node);
      if (!n || !nodeInScope(n, f)) continue;
      if (f.country && e.from !== f.country && e.to !== f.country) continue;
      if (!isNum(e.usd) || e.usd <= 0 || e.from === e.to) continue;
      const k = `${e.from}|${e.to}|${e.relationship}`;
      const p = acc.get(k) ?? { producer: e.from, consumer: e.to, value: 0, relationship: e.relationship, nodes: [], compacts: [], share: null, opportunity: null, oppSum: 0, oppN: 0 };
      p.value += e.usd;
      if (!p.nodes.includes(e.node)) p.nodes.push(e.node);
      if (!p.compacts.includes(c.id)) p.compacts.push(c.id);
      if (isNum(e.share)) p.share = p.share === null ? e.share : Math.max(p.share, e.share);
      if (isNum(e.opportunity)) {
        p.oppSum += e.opportunity;
        p.oppN += 1;
      }
      acc.set(k, p);
    }
  }
  return [...acc.values()]
    .map(({ oppSum, oppN, ...p }) => ({ ...p, opportunity: oppN ? oppSum / oppN : null }))
    .sort((a, b) => b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer));
}

export interface PairInvestment {
  producer: string;
  consumer: string;
  /** supply from the simulated new capacity (USD a year) */
  value: number;
  nodes: string[];
  compacts: string[];
  /** the largest Friendshore score of its edges */
  friendshore: number | null;
}

/** Every investor -> host relationship across the compacts, largest first. */
export function pairInvestmentFlows(doc: Pick<AibDoc, 'nodes' | 'compacts'>, f: PartnershipFilter): PairInvestment[] {
  return pairInvestmentFlowsOf(doc.nodes, doc.compacts, f);
}

/** pairInvestmentFlows over any compacts' investment (the main document's, or a scenario's: scenarioCompacts()). */
export function pairInvestmentFlowsOf(nodeList: readonly AibNode[], compacts: readonly { id: string; investment: readonly AibInvestmentEdge[] }[], f: PartnershipFilter): PairInvestment[] {
  const nodes = new Map(nodeList.map((n) => [n.id, n]));
  const acc = new Map<string, PairInvestment>();
  for (const c of compacts) {
    for (const e of c.investment) {
      const n = nodes.get(e.node);
      if (!n || !nodeInScope(n, f)) continue;
      if (f.country && e.investor !== f.country && e.host !== f.country) continue;
      if (!isNum(e.usd) || e.usd <= 0 || e.investor === e.host) continue;
      const k = `${e.investor}|${e.host}`;
      const p = acc.get(k) ?? { producer: e.investor, consumer: e.host, value: 0, nodes: [], compacts: [], friendshore: null };
      p.value += e.usd;
      if (!p.nodes.includes(e.node)) p.nodes.push(e.node);
      if (!p.compacts.includes(c.id)) p.compacts.push(c.id);
      if (isNum(e.friendshore)) p.friendshore = p.friendshore === null ? e.friendshore : Math.max(p.friendshore, e.friendshore);
      acc.set(k, p);
    }
  }
  return [...acc.values()].sort((a, b) => b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer));
}

/** The coordination dividend of a node: the exported one, else optimized minus existing coverage. */
export function dividendOf(n: AibNode): number | null {
  if (isNum(n.dividend)) return n.dividend;
  return isNum(n.optimized) && isNum(n.existing) ? n.optimized - n.existing : null;
}

/** The nodes in scope where new capacity was simulated (eligible hosts), largest resilience gain first. */
export function investmentNodes(nodes: readonly AibNode[], f: PartnershipFilter): AibNode[] {
  return nodes
    .filter((n) => nodeInScope(n, f) && n.hosts.length > 0 && (!f.country || n.hosts.includes(f.country)))
    .sort((a, b) => (isNum(b.bestGain) ? b.bestGain : -2) - (isNum(a.bestGain) ? a.bestGain : -2) || a.label.localeCompare(b.label, 'en'));
}

export interface HostRow {
  iso3: string;
  /** nodes where the economy is the best-ranked host */
  first: string[];
  /** nodes where it is an eligible host at all */
  eligible: string[];
}

/** Each eligible host of the nodes in scope: where it ranks first and where it is eligible. */
export function hostRows(nodes: readonly AibNode[], f: PartnershipFilter): HostRow[] {
  const acc = new Map<string, HostRow>();
  for (const n of investmentNodes(nodes, { ...f, country: null })) {
    n.hosts.forEach((h, i) => {
      const r = acc.get(h) ?? { iso3: h, first: [], eligible: [] };
      if (i === 0) r.first.push(n.id);
      r.eligible.push(n.id);
      acc.set(h, r);
    });
  }
  return [...acc.values()]
    .filter((r) => !f.country || r.iso3 === f.country)
    .sort((a, b) => b.first.length - a.first.length || b.eligible.length - a.eligible.length || a.iso3.localeCompare(b.iso3));
}

export interface SupplierRow {
  iso3: string;
  /** nodes where the economy is the largest supplier of the optimized solution */
  nodes: string[];
  /** its largest share among them */
  top: number | null;
}

/** Specialization: the economies that are the largest optimized supplier of a node in scope. */
export function specializedSuppliers(nodes: readonly AibNode[], f: PartnershipFilter): SupplierRow[] {
  const acc = new Map<string, SupplierRow>();
  for (const n of nodes) {
    if (!nodeInScope(n, f) || !n.largest) continue;
    if (f.country && n.largest !== f.country) continue;
    const r = acc.get(n.largest) ?? { iso3: n.largest, nodes: [], top: null };
    r.nodes.push(n.id);
    if (isNum(n.largestShare)) r.top = r.top === null ? n.largestShare : Math.max(r.top, n.largestShare);
    acc.set(n.largest, r);
  }
  return [...acc.values()].sort((a, b) => b.nodes.length - a.nodes.length || (b.top ?? 0) - (a.top ?? 0) || a.iso3.localeCompare(b.iso3));
}

/** The BUILD nodes in scope (where new capacity is needed), largest residual gap first. */
export function buildGaps(nodes: readonly AibNode[], f: PartnershipFilter): AibNode[] {
  return nodes
    .filter((n) => n.cls === 'BUILD' && nodeInScope(n, f) && (!f.country || n.hosts.includes(f.country) || n.largest === f.country))
    .sort((a, b) => (isNum(b.residual) ? b.residual : -1) - (isNum(a.residual) ? a.residual : -1) || a.label.localeCompare(b.label, 'en'));
}

/** The `n` nodes with the highest value of `key` (nulls never listed). */
export function topNodes(nodes: readonly AibNode[], key: (n: AibNode) => number | null, n = 5): AibNode[] {
  return nodes
    .filter((x) => isNum(key(x)))
    .sort((a, b) => (key(b) as number) - (key(a) as number) || a.label.localeCompare(b.label, 'en'))
    .slice(0, n);
}

/** A baseline top-partner row of the `ties` chunk (views/Partners/ties.ts TopRow). */
export interface TopPairRow {
  reporter_iso: string;
  side: 'fs' | 'po';
  partner_iso: string;
  value: number | null;
  rank: number | null;
}

/**
 * The baseline top partners of one side (Friendshore or Partnership Opportunity): the economy's own ranked
 * partners when one is chosen, else the highest-scoring pairs; `n` rows.
 */
export function topPairs<R extends TopPairRow>(rows: readonly R[], side: 'fs' | 'po', country: string | null, n = 15): R[] {
  const own = rows.filter((r) => r.side === side && r.reporter_iso !== r.partner_iso && (!country || r.reporter_iso === country));
  const sorted = country
    ? own.sort((a, b) => (a.rank ?? Infinity) - (b.rank ?? Infinity) || (b.value ?? -Infinity) - (a.value ?? -Infinity))
    : own.sort((a, b) => (b.value ?? -Infinity) - (a.value ?? -Infinity) || a.reporter_iso.localeCompare(b.reporter_iso));
  return sorted.slice(0, n);
}

/**
 * The AIB filter (sector or technology, stage, node) read as OSI cells, the unit the partner indices are
 * kept by (Export Opportunity, Friendshore, Development Potential, the factory projects): through the
 * matching nodes' crosswalk, their three-stage cells (legacyOsiCellsOfNode(): node.yfCellsLegacy, else the
 * roll-up of node.yfCells; largest share of the node's trade first), since the partner data are still kept
 * by the paper's three stages while the filter's stage and the nodes are six-stage. In a dictionary bundle
 * never by name (an EIS sector can share a name with an OSI technology, "Solar", and still sit in another
 * stage). In a master bundle the node's technologies are OSI technologies and a node can sit in two
 * (rare-earth compounds: Electric Vehicles and Wind), so the filter's technology also limits the cells:
 * Wind + Refining reads Wind's cell, never the Electric Vehicles cell of the same node (the filter's stage
 * limits the nodes; a three-stage cell is not compared with a six-stage stage). One three-stage cell is
 * listed once, however many six-stage cells read it (nuclear-extraction and nuclear-recovery both read
 * nuclear-upstream). Only the cells this build keeps partner indices for count (`scored`: the OSI basket
 * has Semiconductors, the partner indices do not); the others are listed apart.
 */
export interface OsiCellPick {
  /** the nodes the filter matches (the economy part of the filter aside) */
  nodes: AibNode[];
  /**
   * the dominant OSI cell (three-stage): the first scored cell of the matching node with the largest lost
   * supply (then demand); null when the filter names only a stage (it spans many cells) or no matching
   * node has a scored cell
   */
  cell: OsiCell | null;
  /** the node the dominant cell comes from */
  from: AibNode | null;
  /** every scored OSI cell (three-stage) the matching nodes' codes sit in, the dominant first */
  cells: OsiCell[];
  /** the OSI cells the matching nodes' codes sit in that this build keeps no partner indices for */
  unscored: OsiCell[];
  /** matching nodes with no basket code (no OSI cell: their partner scores are neutral) */
  unmapped: AibNode[];
  /** matching nodes whose codes sit only in cells without partner indices */
  unscoredNodes: AibNode[];
}

export interface CellPickOptions {
  /** node.techs are OSI technologies (a master bundle): the filter's technology limits the cells */
  osiTech: boolean;
  /** the three-stage OSI cells (slugs) this build keeps partner indices for; null or absent: not known, every cell counts */
  scored?: ReadonlySet<string> | null;
}

/** The OSI cells (three-stage slugs) of the engine's cell list: the cells the partner indices are kept for. */
export const scoredCellsOf = (cells: readonly { tech: string; supply_chain: string }[]): Set<string> => new Set(cells.map((c) => stageSlug(c.tech, c.supply_chain)));

const usd = (x: number | null | undefined) => (isNum(x) ? x : -1);

/** The OSI cells of the filter; null when it names no sector, stage or node (every cell is in scope). */
export function osiCellPick(nodes: readonly AibNode[], f: PartnershipFilter, opts: CellPickOptions): OsiCellPick | null {
  if (!f.tech && !f.stage && !f.node) return null;
  const match = nodes.filter((n) => nodeInScope(n, { ...f, country: null }));
  // master: the filter's own technology (a node's other technology is another cell)
  const inFilter = (c: OsiCell) => !opts.osiTech || !f.tech || c.tech === f.tech;
  const isScored = (c: OsiCell) => !opts.scored || opts.scored.has(c.slug);
  const cellsOf = new Map(match.map((n) => [n, legacyOsiCellsOfNode(n).filter(inFilter)]));
  const scoredOf = (n: AibNode) => (cellsOf.get(n) ?? []).filter(isScored);
  const mapped = match.filter((n) => scoredOf(n).length > 0);
  const from = [...mapped].sort((a, b) => usd(b.lostUsd) - usd(a.lostUsd) || usd(b.demandUsd) - usd(a.demandUsd) || a.label.localeCompare(b.label, 'en'))[0] ?? null;
  const cells: OsiCell[] = [];
  const unscored: OsiCell[] = [];
  const seen = new Set<string>();
  for (const n of from ? [from, ...match.filter((x) => x !== from)] : match) {
    for (const c of cellsOf.get(n) ?? []) {
      if (seen.has(c.slug)) continue;
      seen.add(c.slug);
      (isScored(c) ? cells : unscored).push(c);
    }
  }
  const named = !!(f.tech || f.node);
  return {
    nodes: match,
    cell: named && from ? (scoredOf(from)[0] ?? null) : null,
    from: named ? from : null,
    cells,
    unscored,
    unmapped: match.filter((n) => !(cellsOf.get(n) ?? []).length),
    unscoredNodes: match.filter((n) => (cellsOf.get(n) ?? []).length > 0 && !scoredOf(n).length),
  };
}

export interface DpRow {
  iso3: string;
  /** the highest Development Potential among the sectors in scope */
  value: number;
  /** the OSI sector (technology, the paper's stage) it is reached in */
  tech: string;
  stage: string;
  /** BUILD nodes in scope where the economy is an eligible host */
  hostIn: string[];
}

/**
 * Development Potential by economy (the ties chunk's baseline values; `cells` = the engine's cell list,
 * three-stage, cell_id = position + 1): each economy's highest value in the OSI cells in scope, highest
 * first, with the BUILD nodes in scope where it is an eligible host. The cells in scope are the three-stage
 * cells the matching nodes' codes sit in (osiCellPick over the engine's cells; in a master bundle within
 * the filter's technology); every cell without a filter; none when no matching node has a scored cell.
 */
export function dpRows(
  dp: readonly { partner_iso: string; cell_id: number; value: number | null }[],
  cells: readonly { tech: string; supply_chain: string }[],
  nodes: readonly AibNode[],
  f: PartnershipFilter,
  opts: Pick<CellPickOptions, 'osiTech'>,
): DpRow[] {
  const pick = osiCellPick(nodes, f, { osiTech: opts.osiTech, scored: scoredCellsOf(cells) });
  const slugs = pick ? new Set(pick.cells.map((c) => c.slug)) : null;
  if (slugs && !slugs.size) return [];
  const gaps = buildGaps(nodes, { ...f, country: null });
  const acc = new Map<string, DpRow>();
  for (const d of dp) {
    if (!isNum(d.value)) continue;
    if (f.country && d.partner_iso !== f.country) continue;
    const cell = cells[d.cell_id - 1];
    if (!cell) continue;
    if (slugs && !slugs.has(stageSlug(cell.tech, cell.supply_chain))) continue;
    const prev = acc.get(d.partner_iso);
    if (!prev || d.value > prev.value) acc.set(d.partner_iso, { iso3: d.partner_iso, value: d.value, tech: cell.tech, stage: cell.supply_chain, hostIn: [] });
  }
  for (const r of acc.values()) r.hostIn = gaps.filter((n) => n.hosts.includes(r.iso3)).map((n) => n.id);
  return [...acc.values()].sort((a, b) => b.value - a.value || a.iso3.localeCompare(b.iso3));
}

/**
 * The paper's stage (Upstream, Midstream, Downstream) a three-stage reading of the filter takes, such as
 * the aid of the Sector data (its cells are three-stage): null when the filter names neither a stage nor a
 * sub-sector (every stage). The matching nodes vote with the stage of their first three-stage cell (the
 * largest share of their trade; legacyOsiCellsOfNode()); when none has a cell, with the roll-up of their
 * own stage; the most common stage wins, ties in chain order; no matching node: the roll-up of the
 * filter's stage. A Refining node of refined battery materials, which the paper files under Midstream,
 * reads Midstream (industry), rare-earth oxides read Upstream (mining), as the roll-up has it.
 */
export function legacyStageOf(nodes: readonly AibNode[], f: PartnershipFilter): LegacyStage | null {
  if (!f.stage && !f.node) return null;
  const match = nodes.filter((n) => nodeInScope(n, { ...f, country: null }));
  const own = match.map((n) => legacyOsiCellsOfNode(n)[0]?.stage).filter(isLegacyStage);
  const votes = own.length ? own : match.map((n) => rollUpStage(n.stage)).filter(isLegacyStage);
  if (!votes.length) return f.stage ? rollUpStage(f.stage) : null;
  const count = (s: LegacyStage) => votes.filter((v) => v === s).length;
  let best: LegacyStage = LEGACY_STAGES[0];
  for (const s of LEGACY_STAGES) if (count(s) > count(best)) best = s;
  return best;
}

// ---- The partnership lenses (2026-10-19) ----------------------------------------------------------------
// The AIB model lens follows the scenario toggle (Decoupling: the main document; Managed Interdependence:
// node.diversify and the scenario-managed file); the Today lens of trade reads the scenario-baseline file
// (observed 2024 supplier -> buyer pairs and each node's top suppliers). Pure.

/** The scenario the AIB lens reads: the toggle's two choices. */
export type LensScenario = 'decoupling' | 'managed';

/** A compact's relationships and coalition under one scenario. */
export interface ScenarioCompactRel {
  id: string;
  nodes: string[];
  trade: AibTradeEdge[];
  investment: AibInvestmentEdge[];
  /** the coalition's members (null: no coalition under the scenario) */
  members: string[] | null;
  /** why there is no coalition (null when there is one) */
  reason: string | null;
  roles: AibRole[];
  /** members no single other candidate can replace */
  criticalPartners: string[];
  /** non-members whose loss pushes a node below the target (marginal ones included) */
  criticalExternal: string[];
}

/**
 * The compacts under a scenario: Decoupling, the main document; Managed Interdependence, its file joined to
 * the main document's compacts (same ids and nodes; null while the file is not loaded). A compact the file
 * does not list has no relationships and no coalition, with the reason.
 */
export function scenarioCompacts(doc: Pick<AibDoc, 'compacts'>, id: LensScenario, managed?: AibManagedScenarioDoc | null): ScenarioCompactRel[] | null {
  if (id === 'managed' && !managed) return null;
  return doc.compacts.map((c) => {
    const d = compactDetail(c, id, managed);
    const co = d.coalition;
    const members =
      id === 'decoupling' ? coalitionMembersOf(c) : co ? (co.members.length ? co.members : [...new Set(d.roles.filter((r) => r.member).map((r) => r.iso3))].sort()) : null;
    return {
      id: c.id,
      nodes: c.nodes,
      trade: d.trade,
      investment: d.investment,
      members,
      reason: d.reason,
      roles: d.roles,
      criticalPartners: co?.criticalPartners ?? [],
      criticalExternal: [...new Set([...(co?.criticalExternal ?? []), ...(co?.criticalExternalMarginal ?? [])])],
    };
  });
}

/** A node as the AIB lens reads it under a scenario: its class, gap and the hosts for new capacity. */
export interface LensNode {
  node: AibNode;
  /** the class under the scenario */
  cls: string | null;
  /** the share of demand only new capacity fills */
  residual: number | null;
  lostUsd: number | null;
  /** the eligible hosts, best first */
  hosts: string[];
  /** the resilience gain of the best-ranked host */
  bestGain: number | null;
  /** the largest supplier after coordination, and its share */
  largest: string | null;
  largestShare: number | null;
  /** Managed Interdependence leaves the node as it is today (no class, gap or hosts; today's largest supplier) */
  unchanged?: boolean;
}

/**
 * Every node under a scenario. Decoupling: the node's own fields. Managed Interdependence: the nodes the
 * scenario ran on (node.diversify: class, gap, leading hosts; the best host's gain from the scenario's
 * investment flows when given); the others as unchanged (no class, no gap, no hosts: nothing is restored or
 * built there; today's largest supplier, so the supplier view counts them as /aib/roles does). The host and
 * BUILD derivations leave unchanged nodes out (they have no hosts and no class).
 */
export function lensNodes(nodes: readonly AibNode[], id: LensScenario, flows?: AibInvestmentFlows | null): LensNode[] {
  if (id === 'decoupling') {
    return nodes.map((n) => ({ node: n, cls: n.cls, residual: n.residual, lostUsd: n.lostUsd, hosts: n.hosts, bestGain: n.bestGain, largest: n.largest, largestShare: n.largestShare }));
  }
  const ranked = new Map<string, { host: string; rank: number; gain: number | null }[]>();
  for (const h of flows?.hosts ?? []) ranked.set(h.node, [...(ranked.get(h.node) ?? []), { host: h.host, rank: h.rank, gain: h.gain }]);
  const out: LensNode[] = [];
  for (const n of nodes) {
    const s = nodeScenario(n, 'managed');
    if (!s.ran) {
      out.push({ node: n, cls: null, residual: 0, lostUsd: 0, hosts: [], bestGain: null, largest: s.largest, largestShare: s.largestShare, unchanged: true });
      continue;
    }
    const r = [...(ranked.get(n.id) ?? [])].sort((a, b) => a.rank - b.rank);
    const hosts = n.diversify?.hosts?.length ? n.diversify.hosts : r.map((x) => x.host);
    out.push({ node: n, cls: s.cls, residual: s.residual, lostUsd: s.lostUsd, hosts, bestGain: r[0]?.gain ?? null, largest: s.largest, largestShare: s.largestShare });
  }
  return out;
}

/** The lens nodes in scope with eligible hosts (and the economy among them), largest resilience gain first. */
export function lensInvestmentNodes(rows: readonly LensNode[], f: PartnershipFilter): LensNode[] {
  return rows
    .filter((r) => nodeInScope(r.node, f) && r.hosts.length > 0 && (!f.country || r.hosts.includes(f.country)))
    .sort((a, b) => (isNum(b.bestGain) ? b.bestGain : -2) - (isNum(a.bestGain) ? a.bestGain : -2) || a.node.label.localeCompare(b.node.label, 'en'));
}

/** hostRows() over lens nodes: where each eligible host ranks first and where it is eligible. */
export function lensHostRows(rows: readonly LensNode[], f: PartnershipFilter): HostRow[] {
  const acc = new Map<string, HostRow>();
  for (const r of lensInvestmentNodes(rows, { ...f, country: null })) {
    r.hosts.forEach((h, i) => {
      const x = acc.get(h) ?? { iso3: h, first: [], eligible: [] };
      if (i === 0) x.first.push(r.node.id);
      x.eligible.push(r.node.id);
      acc.set(h, x);
    });
  }
  return [...acc.values()]
    .filter((r) => !f.country || r.iso3 === f.country)
    .sort((a, b) => b.first.length - a.first.length || b.eligible.length - a.eligible.length || a.iso3.localeCompare(b.iso3));
}

/** buildGaps() over lens nodes: the BUILD nodes in scope under the scenario, largest residual gap first. */
export function lensBuildGaps(rows: readonly LensNode[], f: PartnershipFilter): LensNode[] {
  return rows
    .filter((r) => r.cls === 'BUILD' && nodeInScope(r.node, f) && (!f.country || r.hosts.includes(f.country) || r.largest === f.country))
    .sort((a, b) => (isNum(b.residual) ? b.residual : -1) - (isNum(a.residual) ? a.residual : -1) || a.node.label.localeCompare(b.node.label, 'en'));
}

/** specializedSuppliers() over lens nodes: the largest supplier after coordination under the scenario. */
export function lensSpecializedSuppliers(rows: readonly LensNode[], f: PartnershipFilter): SupplierRow[] {
  const acc = new Map<string, SupplierRow>();
  for (const r of rows) {
    if (!nodeInScope(r.node, f) || !r.largest) continue;
    if (f.country && r.largest !== f.country) continue;
    const x = acc.get(r.largest) ?? { iso3: r.largest, nodes: [], top: null };
    x.nodes.push(r.node.id);
    if (isNum(r.largestShare)) x.top = x.top === null ? r.largestShare : Math.max(x.top, r.largestShare);
    acc.set(r.largest, x);
  }
  return [...acc.values()].sort((a, b) => b.nodes.length - a.nodes.length || (b.top ?? 0) - (a.top ?? 0) || a.iso3.localeCompare(b.iso3));
}

/** The economies named as hosts across lens nodes (a best-ranked host counts twice), most first. */
export function hostsNamed(rows: readonly LensNode[]): string[] {
  const score = new Map<string, number>();
  for (const r of rows) r.hosts.forEach((h, i) => score.set(h, (score.get(h) ?? 0) + (i === 0 ? 2 : 1)));
  return [...score.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([k]) => k);
}

// ---- Today's trade (scenario-baseline) ------------------------------------------------------------------

export interface TodaySupplier {
  iso3: string;
  /** observed 2024 supply to the allied market, USD a year, summed over the nodes in scope */
  usd: number;
  /** the nodes in scope where it is among the top suppliers */
  nodes: string[];
}

/**
 * Today's largest suppliers to the allied market over the nodes in scope (the economy part of the filter
 * aside): each node's top ten observed suppliers (the scenario-baseline file; the top five of
 * node.baseline while it loads), summed in USD.
 */
export function todaySuppliers(nodes: readonly AibNode[], f: PartnershipFilter, baseline?: AibBaselineScenarioDoc | null): TodaySupplier[] {
  const acc = new Map<string, TodaySupplier>();
  for (const n of nodes) {
    if (!nodeInScope(n, f)) continue;
    for (const s of nodeSuppliersToday(n, baseline)) {
      if (!isNum(s.usd) || s.usd <= 0) continue;
      const x = acc.get(s.iso3) ?? { iso3: s.iso3, usd: 0, nodes: [] };
      x.usd += s.usd;
      x.nodes.push(n.id);
      acc.set(s.iso3, x);
    }
  }
  return [...acc.values()].sort((a, b) => b.usd - a.usd || a.iso3.localeCompare(b.iso3));
}

export interface TodayPair {
  producer: string;
  consumer: string;
  /** observed 2024 trade of the pair across every strategic node it trades in (USD a year) */
  value: number;
  /** the observed share of node demand, summed over the pair's nodes */
  share: number;
  /** the pair's nodes in scope */
  nodes: string[];
  /** how many nodes the pair trades in */
  nAll: number;
  /**
   * under a sector, stage or sub-sector filter: the supplier's own observed supply to the allied market over
   * the nodes in scope (each node's top suppliers, todaySuppliers(); 0 when it is not among them), what the
   * pairs are ranked by; null without such a filter
   */
  scopeUsd: number | null;
}

/**
 * Who supplies whom today: the observed supplier -> buyer pairs of the baseline network (strategic-scope
 * nodes, demand-scope buyers) that trade in a node in scope and, with an economy, involve it. The value
 * covers every node of the pair (the network sums them, and the file has no per-node pair values); with a
 * sector, stage or sub-sector filter `nodes` says which of them are in scope, and the pairs are ranked by
 * their supplier's own supply over the nodes in scope (scopeUsd, from the node-level supplier lists of
 * `baseline`, else node.baseline), then by value, so the list and the partners it names are about the
 * selection rather than the pair's trade elsewhere.
 */
export function todayPairs(network: AibNetwork | null | undefined, nodes: readonly AibNode[], f: PartnershipFilter, baseline?: AibBaselineScenarioDoc | null): TodayPair[] {
  if (!network) return [];
  const scoped = new Set(nodes.filter((n) => nodeInScope(n, f)).map((n) => n.id));
  const filtered = !!(f.tech || f.stage || f.node);
  const supply = filtered ? new Map(todaySuppliers(nodes, { ...f, country: null }, baseline).map((s) => [s.iso3, s.usd])) : null;
  const out: TodayPair[] = [];
  for (const e of network.edges) {
    if (e.from === e.to || !isNum(e.tradeUsd) || e.tradeUsd <= 0) continue;
    if (f.country && e.from !== f.country && e.to !== f.country) continue;
    const inScope = e.nodes.filter((x) => scoped.has(x));
    if (!inScope.length) continue;
    out.push({ producer: e.from, consumer: e.to, value: e.tradeUsd, share: e.trade, nodes: inScope, nAll: e.nodes.length, scopeUsd: supply ? (supply.get(e.from) ?? 0) : null });
  }
  return out.sort(
    (a, b) => (supply ? (b.scopeUsd ?? 0) - (a.scopeUsd ?? 0) : 0) || b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer),
  );
}

// ---- What each lens names (the cross-lens summary) -------------------------------------------------------

/** The partners of `iso` in producer -> consumer pairs (either side), in the pairs' order, once each. */
export function partnersInPairs(pairs: readonly { producer: string; consumer: string }[], iso: string): string[] {
  const out: string[] = [];
  for (const p of pairs) {
    const other = p.producer === iso ? p.consumer : p.consumer === iso ? p.producer : null;
    if (other && other !== iso && !out.includes(other)) out.push(other);
  }
  return out;
}

/** One side of producer -> consumer pairs, summed by value, largest first. */
export function sideTotals(pairs: readonly { producer: string; consumer: string; value: number }[], side: 'producer' | 'consumer'): string[] {
  const acc = new Map<string, number>();
  for (const p of pairs) acc.set(p[side], (acc.get(p[side]) ?? 0) + p.value);
  return [...acc.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([k]) => k);
}

/** The coalitions of the compacts in scope under a scenario (the Geopolitical lens). */
export interface LensCoalitions {
  /** the compacts in scope (a node in scope; with an economy, it holds a seat, a role or is a critical dependency) */
  compacts: ScenarioCompactRel[];
  /** members across those coalitions (with an economy: its co-members, where it holds a seat), most compacts first */
  members: { iso3: string; n: number }[];
  /** critical supply partners and critical dependencies outside, across them */
  criticalPartners: string[];
  criticalExternal: string[];
  /** with an economy: the compacts whose coalition it sits in, and those that list it as a critical dependency outside */
  seats: string[];
  dependency: string[];
}

/** The compact coalitions in scope under a scenario; with an economy, its seats and co-members. */
export function lensCoalitions(compacts: readonly ScenarioCompactRel[], nodes: readonly AibNode[], f: PartnershipFilter): LensCoalitions {
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const iso = f.country;
  const inScope = compacts.filter((c) =>
    c.nodes.some((id) => {
      const n = byId.get(id);
      return !!n && nodeInScope(n, { ...f, country: null });
    }),
  );
  const touches = (c: ScenarioCompactRel) => !iso || (c.members ?? []).includes(iso) || c.roles.some((r) => r.iso3 === iso) || c.criticalExternal.includes(iso);
  const list = inScope.filter(touches);
  const seats = iso ? list.filter((c) => (c.members ?? []).includes(iso)).map((c) => c.id) : [];
  const dependency = iso ? list.filter((c) => c.criticalExternal.includes(iso)).map((c) => c.id) : [];
  const count = new Map<string, number>();
  for (const c of iso ? list.filter((x) => seats.includes(x.id)) : list) for (const m of c.members ?? []) if (m !== iso) count.set(m, (count.get(m) ?? 0) + 1);
  const uniq = (xs: string[]) => [...new Set(xs)].filter((x) => x !== iso);
  return {
    compacts: list,
    members: [...count.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).map(([iso3, n]) => ({ iso3, n })),
    criticalPartners: uniq(list.flatMap((c) => c.criticalPartners)),
    criticalExternal: uniq(list.flatMap((c) => c.criticalExternal)),
    seats,
    dependency,
  };
}
