// Allied network derivations (pure): stage lists, specialization rows, a main-thread comparison
// against the paper's exact design (when it is shipped), and the per-stage selections the charts use.

import { has } from '../../engine/table.ts';
import type { Table } from '../../engine/types.ts';
import { numOr, rowsOf, strOr } from '../common.tsx';

export interface StageRef {
  tech: string;
  supply_chain: string;
}

export interface SpecRow {
  tech: string;
  supply_chain: string;
  node: string;
  share: number;
  selected: boolean;
  score: number | null;
}

export const stageId = (s: StageRef): string => `${s.tech}|${s.supply_chain}`;

/** A specialization table as rows (scenario tables use iso3c, the baseline summary network_node). */
export function specRows(t: Table | null | undefined): SpecRow[] {
  if (!t) return [];
  const col = has(t, 'iso3c') ? 'iso3c' : 'network_node';
  return rowsOf(t).map((r) => ({
    tech: String(r.tech),
    supply_chain: String(r.supply_chain),
    node: String(r[col]),
    share: numOr(r.production_share) ?? 0,
    selected: r.selected === true,
    score: numOr(r.producer_score),
  }));
}

/** Stages of a diagnostics table, in its order. */
export function stagesOf(diag: Table | null | undefined): StageRef[] {
  return rowsOf(diag).map((r) => ({ tech: String(r.tech), supply_chain: String(r.supply_chain) }));
}

export function hhi(shares: number[]): number {
  return shares.reduce((a, s) => a + s * s, 0);
}

export interface StageCompareRow {
  tech: string;
  supply_chain: string;
  n_producers: number;
  n_producers_ref: number;
  producers_added: string;
  producers_removed: string;
  jaccard: number | null;
  hhi: number | null;
  hhi_ref: number | null;
  hhi_delta: number | null;
  max_abs_share_delta: number | null;
  flow_turnover: number | null;
  changed: boolean;
}

export interface NodeCompareRow {
  tech: string;
  supply_chain: string;
  node: string;
  share: number;
  share_ref: number;
  delta: number;
}

/** Compare two designs by their specialization (used for the paper's exact design; flows are not
 * shipped with it, so flow turnover is not available). */
export function compareSpecs(cur: SpecRow[], ref: SpecRow[]): { stage: StageCompareRow[]; node: NodeCompareRow[] } {
  const stages = new Map<string, StageRef>();
  for (const r of [...cur, ...ref]) stages.set(stageId(r), { tech: r.tech, supply_chain: r.supply_chain });
  const stageRows: StageCompareRow[] = [];
  const nodeRows: NodeCompareRow[] = [];
  for (const [id, s] of stages) {
    const a = new Map(cur.filter((r) => stageId(r) === id).map((r) => [r.node, r.share]));
    const b = new Map(ref.filter((r) => stageId(r) === id).map((r) => [r.node, r.share]));
    const pa = new Set([...a].filter(([, v]) => v > 0).map(([k]) => k));
    const pb = new Set([...b].filter(([, v]) => v > 0).map(([k]) => k));
    const union = new Set([...pa, ...pb]);
    const inter = [...pa].filter((k) => pb.has(k)).length;
    let maxd = 0;
    for (const k of new Set([...a.keys(), ...b.keys()])) {
      const d = (a.get(k) ?? 0) - (b.get(k) ?? 0);
      if (Math.abs(d) > maxd) maxd = Math.abs(d);
      if ((a.get(k) ?? 0) > 0 || (b.get(k) ?? 0) > 0) nodeRows.push({ tech: s.tech, supply_chain: s.supply_chain, node: k, share: a.get(k) ?? 0, share_ref: b.get(k) ?? 0, delta: d });
    }
    const h = pa.size ? hhi([...a.values()]) : null;
    const hr = pb.size ? hhi([...b.values()]) : null;
    const added = [...pa].filter((k) => !pb.has(k)).sort();
    const removed = [...pb].filter((k) => !pa.has(k)).sort();
    stageRows.push({
      tech: s.tech,
      supply_chain: s.supply_chain,
      n_producers: pa.size,
      n_producers_ref: pb.size,
      producers_added: added.join(' '),
      producers_removed: removed.join(' '),
      jaccard: union.size ? inter / union.size : null,
      hhi: h,
      hhi_ref: hr,
      hhi_delta: h !== null && hr !== null ? h - hr : null,
      max_abs_share_delta: maxd,
      flow_turnover: null,
      changed: added.length > 0 || removed.length > 0 || maxd > 1e-9,
    });
  }
  return { stage: stageRows, node: nodeRows };
}

/** The worker's comparison tables as typed rows. */
export function workerCompare(cmp: { stage: Table; node: Table } | null): { stage: StageCompareRow[]; node: NodeCompareRow[] } | null {
  if (!cmp) return null;
  return {
    stage: rowsOf(cmp.stage).map((r) => ({
      tech: String(r.tech),
      supply_chain: String(r.supply_chain),
      n_producers: numOr(r.n_producers) ?? 0,
      n_producers_ref: numOr(r.n_producers_ref) ?? 0,
      producers_added: strOr(r.producers_added) ?? '',
      producers_removed: strOr(r.producers_removed) ?? '',
      jaccard: numOr(r.jaccard),
      hhi: numOr(r.hhi),
      hhi_ref: numOr(r.hhi_ref),
      hhi_delta: numOr(r.hhi_delta),
      max_abs_share_delta: numOr(r.max_abs_share_delta),
      flow_turnover: numOr(r.flow_turnover),
      changed: r.changed === true,
    })),
    node: rowsOf(cmp.node).map((r) => ({
      tech: String(r.tech),
      supply_chain: String(r.supply_chain),
      node: String(r.iso3c),
      share: numOr(r.production_share) ?? 0,
      share_ref: numOr(r.production_share_ref) ?? 0,
      delta: numOr(r.share_delta) ?? 0,
    })),
  };
}

export interface FlowRow {
  tech: string;
  supply_chain: string;
  producer: string;
  consumer: string;
  share: number;
  edge_weight: number | null;
  fs: number | null;
  po: number | null;
}

/** Flows: reporter_iso is the producer, partner_iso the consumer. */
export function flowRows(t: Table | null | undefined): FlowRow[] {
  return rowsOf(t).map((r) => ({
    tech: String(r.tech),
    supply_chain: String(r.supply_chain),
    producer: String(r.reporter_iso),
    consumer: String(r.partner_iso),
    share: numOr(r.flow_share) ?? 0,
    edge_weight: numOr(r.edge_weight),
    fs: numOr(r.friendshore_index),
    po: numOr(r.opportunity_index),
  }));
}

/** Mean |share difference| and median Jaccard between two designs (the greedy-vs-exact gap). */
export function designGap(stage: StageCompareRow[], node: NodeCompareRow[]): { meanAbs: number; medianJaccard: number } | null {
  if (!stage.length) return null;
  const js = stage.map((s) => s.jaccard).filter((x): x is number => x !== null).sort((a, b) => a - b);
  const m = js.length >> 1;
  const med = js.length ? (js.length % 2 ? js[m] : (js[m - 1] + js[m]) / 2) : NaN;
  const meanAbs = node.length ? node.reduce((a, r) => a + Math.abs(r.delta), 0) / node.length : 0;
  return { meanAbs, medianJaccard: med };
}
