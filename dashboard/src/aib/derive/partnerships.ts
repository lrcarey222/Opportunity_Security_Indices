// Allied Industrial Base partnerships and overview (IA refactor, 2026-10-16): the model's trade and
// investment relationships gathered across every compact and scoped by economy / technology / stage /
// sub-sector; the eligible investment hosts and specialized suppliers of the nodes; the overview's class
// counts and top nodes; the baseline top partners of the `ties` chunk. No new score: every number is one
// the export or the ties chunk already carries (sums of the model's own flows at most). Pure.

import { isNum, stageSlug } from '../../lib/format.ts';
import type { AibDoc, AibNode, AibRelationship } from '../contract.ts';

export interface PartnershipFilter {
  /** an economy (ISO3) that must take part */
  country: string | null;
  tech: string | null;
  stage: string | null;
  /** a node id */
  node: string | null;
}

export const NO_FILTER: PartnershipFilter = { country: null, tech: null, stage: null, node: null };

const STAGE_ORDER = ['Upstream', 'Midstream', 'Downstream'];

/** The node passes the technology / stage / sub-sector part of the filter. */
export function nodeInScope(n: AibNode, f: PartnershipFilter): boolean {
  if (f.node && n.id !== f.node) return false;
  if (f.tech && !n.techs.includes(f.tech)) return false;
  if (f.stage && n.stage !== f.stage) return false;
  return true;
}

/** The filter's choices from the document: technologies, stages, nodes (scoped by tech / stage), economies. */
export function filterChoices(doc: Pick<AibDoc, 'nodes' | 'compacts' | 'countries'>, f: PartnershipFilter) {
  const techs = [...new Set(doc.nodes.flatMap((n) => n.techs))].sort((a, b) => a.localeCompare(b, 'en'));
  const stages = [...new Set(doc.nodes.map((n) => n.stage))].sort((a, b) => STAGE_ORDER.indexOf(a) - STAGE_ORDER.indexOf(b));
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
  const nodes = new Map(doc.nodes.map((n) => [n.id, n]));
  const acc = new Map<string, PairTrade & { oppSum: number; oppN: number }>();
  for (const c of doc.compacts) {
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
  const nodes = new Map(doc.nodes.map((n) => [n.id, n]));
  const acc = new Map<string, PairInvestment>();
  for (const c of doc.compacts) {
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

export interface DpRow {
  iso3: string;
  /** the highest Development Potential among the sectors in scope */
  value: number;
  /** the sector it is reached in */
  tech: string;
  stage: string;
  /** BUILD nodes in scope where the economy is an eligible host */
  hostIn: string[];
}

/**
 * Development Potential by economy (the ties chunk's baseline values; `cells` = the engine's cell list,
 * cell_id = position + 1): each economy's highest value in the sectors in scope, highest first, with the
 * BUILD nodes in scope where it is an eligible host.
 */
export function dpRows(
  dp: readonly { partner_iso: string; cell_id: number; value: number | null }[],
  cells: readonly { tech: string; supply_chain: string }[],
  nodes: readonly AibNode[],
  f: PartnershipFilter,
): DpRow[] {
  const scoped = nodes.filter((n) => nodeInScope(n, { ...f, country: null }));
  const slugs = f.node ? new Set(scoped.flatMap((n) => n.yfCells)) : null;
  const gaps = buildGaps(nodes, { ...f, country: null });
  const acc = new Map<string, DpRow>();
  for (const d of dp) {
    if (!isNum(d.value)) continue;
    if (f.country && d.partner_iso !== f.country) continue;
    const cell = cells[d.cell_id - 1];
    if (!cell) continue;
    if (f.tech && cell.tech !== f.tech) continue;
    if (f.stage && cell.supply_chain !== f.stage) continue;
    if (slugs && !slugs.has(stageSlug(cell.tech, cell.supply_chain))) continue;
    const prev = acc.get(d.partner_iso);
    if (!prev || d.value > prev.value) acc.set(d.partner_iso, { iso3: d.partner_iso, value: d.value, tech: cell.tech, stage: cell.supply_chain, hostIn: [] });
  }
  for (const r of acc.values()) r.hostIn = gaps.filter((n) => n.hosts.includes(r.iso3)).map((n) => n.id);
  return [...acc.values()].sort((a, b) => b.value - a.value || a.iso3.localeCompare(b.iso3));
}
