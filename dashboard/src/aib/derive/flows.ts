// Partnership flows for the Sankey charts: a compact's trade relationships (exporter -> importer, new or
// expanded) and investment relationships (investor -> host), optionally one node only, summed by pair
// and kept to the largest. Pure.

import { isNum } from '../../lib/format.ts';
import type { AibCompact, AibRelationship } from '../contract.ts';

export interface TradeFlow {
  producer: string;
  consumer: string;
  /** added trade (USD), summed over the selected nodes */
  value: number;
  relationship: AibRelationship;
  /** the nodes the flow serves */
  nodes: string[];
}

export interface InvestmentFlow {
  producer: string;
  consumer: string;
  /** supply from the new capacity (USD) */
  value: number;
  nodes: string[];
}

/** The top `top` trade flows by (exporter, importer, relationship); `node` null for every node. */
export function tradeFlows(c: AibCompact, node: string | null = null, top = 15): TradeFlow[] {
  const acc = new Map<string, TradeFlow>();
  for (const e of c.trade) {
    if (node && e.node !== node) continue;
    if (!isNum(e.usd) || e.usd <= 0 || e.from === e.to) continue;
    const k = `${e.from}|${e.to}|${e.relationship}`;
    const f = acc.get(k) ?? { producer: e.from, consumer: e.to, value: 0, relationship: e.relationship, nodes: [] };
    f.value += e.usd;
    if (!f.nodes.includes(e.node)) f.nodes.push(e.node);
    acc.set(k, f);
  }
  return [...acc.values()].sort((a, b) => b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer)).slice(0, top);
}

/** The top `top` investment flows by (investor, host); `node` null for every node. */
export function investmentFlows(c: AibCompact, node: string | null = null, top = 15): InvestmentFlow[] {
  const acc = new Map<string, InvestmentFlow>();
  for (const e of c.investment) {
    if (node && e.node !== node) continue;
    if (!isNum(e.usd) || e.usd <= 0 || e.investor === e.host) continue;
    const k = `${e.investor}|${e.host}`;
    const f = acc.get(k) ?? { producer: e.investor, consumer: e.host, value: 0, nodes: [] };
    f.value += e.usd;
    if (!f.nodes.includes(e.node)) f.nodes.push(e.node);
    acc.set(k, f);
  }
  return [...acc.values()].sort((a, b) => b.value - a.value || a.producer.localeCompare(b.producer) || a.consumer.localeCompare(b.consumer)).slice(0, top);
}

/** The nodes that have any trade or investment relationship in a compact, in the compact's order. */
export function nodesWithRelationships(c: AibCompact): string[] {
  const used = new Set([...c.trade.map((e) => e.node), ...c.investment.map((e) => e.node)]);
  return c.nodes.filter((n) => used.has(n));
}
