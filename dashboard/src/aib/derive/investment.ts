// Investor -> host flows of the marginal investment simulation (2026-10-17; doc.investmentFlows, scripts/32
// allied_investment_pairs + the ranked hosts of allied_investment_partnerships, headline variant). Each
// ranked host of a node is a separate simulation of the increment of capacity: only rank 1 is the
// selected host, so flows are summed across nodes for rank 1 only; alternatives (ranks 2..) are listed,
// never added up. A modelled result: nothing here follows the Perspective. Pure.
// The scenarios (2026-10-19): the flows follow the toggle (Decoupling doc.investmentFlows, Managed
// Interdependence its scenario file's; scenarios.ts scenarioInvestmentFlows()); flowsDocOf() pairs them
// with the main document's nodes, so every function here reads either. Baseline has none: no new capacity
// exists today.

import { TERMS } from '../../content/vocabulary.ts';
import { isNum } from '../../lib/format.ts';
import type { AibDoc, AibInvestmentFlows, AibInvestmentHost, AibInvestmentPair, AibNode } from '../contract.ts';
import { fmtPp } from './format.ts';
import { nodeInScope, type PartnershipFilter } from './partnerships.ts';

export type RankChoice = 'best' | 'all';

export interface InvestmentFilter extends PartnershipFilter {
  /** best: the selected (rank 1) host of each node only; all: every ranked host, as alternatives */
  rank?: RankChoice;
  /** the node ids to keep (a compact's nodes); null for every node */
  nodes?: readonly string[] | null;
  /** the economy must be the investor (or the host) rather than either */
  side?: 'investor' | 'host' | null;
}

/** The flows in scope: node filter (technology, stage, sub-sector, a node set), economy (investor or host), rank. Largest first. */
export function investmentPairs(doc: Pick<AibDoc, 'nodes' | 'investmentFlows'>, f: InvestmentFilter): AibInvestmentPair[] {
  const pairs = doc.investmentFlows?.pairs ?? [];
  const nodes = new Map(doc.nodes.map((n) => [n.id, n]));
  const set = f.nodes ? new Set(f.nodes) : null;
  const rank = f.rank ?? 'best';
  return pairs
    .filter((p) => {
      if (rank === 'best' && p.rank !== 1) return false;
      if (set && !set.has(p.node)) return false;
      const n = nodes.get(p.node);
      if (!n || !nodeInScope(n, f)) return false;
      if (f.country) {
        if (f.side === 'investor') return p.investor === f.country;
        if (f.side === 'host') return p.host === f.country;
        return p.investor === f.country || p.host === f.country;
      }
      return true;
    })
    .sort((a, b) => b.usd - a.usd || a.rank - b.rank || a.investor.localeCompare(b.investor) || a.host.localeCompare(b.host));
}

export interface InvestorHostFlow {
  producer: string;
  consumer: string;
  /** supply from the new capacity at the best-ranked hosts (USD a year), summed over the nodes */
  value: number;
  nodes: string[];
}

/**
 * Investor -> host flows for a Sankey: rank-1 pairs summed by (investor, host) over the nodes; pairs of
 * other ranks are ignored (they are alternatives of the same simulation, not additional supply).
 */
export function investorHostFlows(pairs: readonly AibInvestmentPair[], top = 15): InvestorHostFlow[] {
  const acc = new Map<string, InvestorHostFlow>();
  for (const p of pairs) {
    if (p.rank !== 1 || !isNum(p.usd) || p.usd <= 0) continue;
    const k = `${p.investor}|${p.host}`;
    const f = acc.get(k) ?? { producer: p.investor, consumer: p.host, value: 0, nodes: [] };
    f.value += p.usd;
    if (!f.nodes.includes(p.node)) f.nodes.push(p.node);
    acc.set(k, f);
  }
  return [...acc.values()].sort((a, b) => b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer)).slice(0, top);
}

export interface PartnerTotal {
  iso3: string;
  usd: number;
  nodes: string[];
}

/** An economy's rank-1 flows by partner: the hosts it invests in, or the investors its capacity serves. */
export function partnerTotals(pairs: readonly AibInvestmentPair[], iso3: string, side: 'investor' | 'host'): PartnerTotal[] {
  const acc = new Map<string, PartnerTotal>();
  for (const p of pairs) {
    if (p.rank !== 1) continue;
    if ((side === 'investor' ? p.investor : p.host) !== iso3) continue;
    const other = side === 'investor' ? p.host : p.investor;
    const t = acc.get(other) ?? { iso3: other, usd: 0, nodes: [] };
    t.usd += p.usd;
    if (!t.nodes.includes(p.node)) t.nodes.push(p.node);
    acc.set(other, t);
  }
  return [...acc.values()].sort((a, b) => b.usd - a.usd || a.iso3.localeCompare(b.iso3));
}

/**
 * The alignment screen of a node in words (node.screenedHost, 2026-10-20): the best host it keeps out, that
 * host's gain and what the screen costs, e.g. "Iran (+16.7 pp of coverage) is not a host: the best eligible
 * host adds 4.2 pp less". Costs under 0.05 pp read as none. null where no host was screened.
 */
export function screenedHostNote(n: Pick<AibNode, 'screenedHost'>, name: (iso3: string) => string): string | null {
  const s = n.screenedHost;
  if (!s) return null;
  const who = `${name(s.host)} (${fmtPp(s.gain)} of coverage) is not a host`;
  return isNum(s.cost) && s.cost >= 0.0005
    ? `${who}: the best eligible host adds ${(s.cost * 100).toFixed(1)} pp less`
    : `${who}: an eligible host adds as much`;
}

/** The ranked hosts of a node, best first. */
export function hostsOfNode(doc: Pick<AibDoc, 'investmentFlows'>, node: string): AibInvestmentHost[] {
  return (doc.investmentFlows?.hosts ?? []).filter((h) => h.node === node).sort((a, b) => a.rank - b.rank);
}

/** The nodes where an economy is a ranked host, best rank first, then largest gain. */
export function hostRanksOf(doc: Pick<AibDoc, 'investmentFlows'>, iso3: string): AibInvestmentHost[] {
  return (doc.investmentFlows?.hosts ?? [])
    .filter((h) => h.host === iso3)
    .sort((a, b) => a.rank - b.rank || (b.gain ?? -1) - (a.gain ?? -1) || a.node.localeCompare(b.node));
}

/** The rank of a host in a node's simulation ("best host", "2nd of 5"). */
export function rankWords(rank: number, maxRank?: number): string {
  if (rank === 1) return 'best host';
  const suf = rank === 2 ? 'nd' : rank === 3 ? 'rd' : 'th';
  return `${rank}${suf}${maxRank ? ` of ${maxRank}` : ''}`;
}

/** A scenario's flows as the functions here read them: the main document's nodes and meta with those flows (none: null). */
export function flowsDocOf<D extends Pick<AibDoc, 'nodes' | 'meta'>>(doc: D, flows: AibInvestmentFlows | null | undefined): Pick<AibDoc, 'nodes' | 'meta' | 'investmentFlows'> {
  return { nodes: doc.nodes, meta: doc.meta, investmentFlows: flows ?? undefined };
}

/** The document carries the flows (a bundle from before 2026-10-17 does not). */
export const hasInvestmentFlows = (doc: Pick<AibDoc, 'investmentFlows'>): boolean => !!doc.investmentFlows;

/** The headline variant in a short phrase, for the modelled-result note. */
export function variantPhrase(doc: Pick<AibDoc, 'investmentFlows' | 'meta'>, shockLabel: string): string {
  const v = doc.investmentFlows?.variant;
  const inc = v && isNum(v.increment) ? `, an increment of up to ${Math.round(v.increment * 100)}% of node demand at each host (at most its current output)` : '';
  return `${shockLabel}; ${TERMS.supplyAssumption}: ${v?.scenario ?? doc.meta.headlineScenario}${v?.supplyConstraint ? `, ${v.supplyConstraint.replace(/_/g, '-')} supply` : ''}${inc}`;
}

/** Node lookup helper shared by the views: the label, else the id. */
export const labelOf = (nodeOf: (id: string) => AibNode | undefined) => (id: string) => nodeOf(id)?.label ?? id;
