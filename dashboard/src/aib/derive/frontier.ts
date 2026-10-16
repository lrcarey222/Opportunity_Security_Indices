// The specialization-resilience frontier of a node: the optimized solution re-solved under each
// supplier-share limit (no candidate exporter may provide more than 25 / 40 / 60 / 100% of the supply),
// with its coverage, the coverage if it then also lost its largest supplier (a second shock), and the
// supply-weighted specialization of the exporters it uses. Two readings come with it: the tightest limit
// that costs at most 2 points of coverage, and the loosest limit that keeps coverage at 80% or more after
// a second shock (the least diversification that buys that resilience). Pure.

import { isNum } from '../../lib/format.ts';
import type { AibNode } from '../contract.ts';
import { nameOf } from './format.ts';

export type FrontierKey = 'coverage' | 'secondShock' | 'specialization';

/** The three lines, in legend order (colours: the validated categorical triple; dashes as a second cue). */
export const FRONTIER_SERIES: readonly { key: FrontierKey; label: string; color: string; dash: string | null }[] = [
  { key: 'coverage', label: 'Coverage', color: '#1889b0', dash: null },
  { key: 'secondShock', label: 'Coverage if the largest supplier is also lost', color: '#8b9d17', dash: '6,4' },
  { key: 'specialization', label: 'Specialization score', color: '#c96f7a', dash: '2,3' },
];

export interface FrontierRow {
  cap: number;
  coverage: number | null;
  secondShock: number | null;
  specialization: number | null;
  largest: string | null;
  largestName: string | null;
  largestShare: number | null;
}

/** The frontier of a node by supplier limit, tightest first. */
export function frontierRows(n: AibNode, names: Readonly<Record<string, string>>): FrontierRow[] {
  return [...n.frontier]
    .sort((a, b) => a.cap - b.cap)
    .map((f) => ({
      cap: f.cap,
      coverage: isNum(f.coverage) ? f.coverage : null,
      secondShock: isNum(f.secondShock) ? f.secondShock : null,
      specialization: isNum(f.specialization) ? f.specialization : null,
      largest: f.largest,
      largestName: f.largest ? nameOf(names, f.largest) : null,
      largestShare: isNum(f.largestShare) ? f.largestShare : null,
    }));
}

/** One point per line and limit with a value (the chart's data). */
export function frontierPoints(rows: readonly FrontierRow[]): { cap: number; key: FrontierKey; value: number }[] {
  return rows.flatMap((r) => FRONTIER_SERIES.filter((s) => isNum(r[s.key])).map((s) => ({ cap: r.cap, key: s.key, value: r[s.key] as number })));
}

/** The nodes that have a frontier to show. */
export const nodesWithFrontier = (nodes: readonly AibNode[]): AibNode[] => nodes.filter((n) => n.frontier.length > 0);

/** "40%" for a supplier limit; "no limit" for 100%. */
export function capLabel(cap: number | null | undefined): string {
  if (!isNum(cap)) return '–';
  return cap >= 1 - 1e-9 ? 'no limit (100%)' : `${Math.round(cap * 100)}%`;
}

/** The two readings of a node's frontier in words (null fields: the export has none). */
export function frontierReadings(n: AibNode): { costless: string | null; required: string | null } {
  const costless = n.tightestCostless === undefined ? null : isNum(n.tightestCostless) ? capLabel(n.tightestCostless) : '–';
  const required = n.minRequiredDiversification === undefined ? null : isNum(n.minRequiredDiversification) ? capLabel(n.minRequiredDiversification) : 'no tested limit reaches it';
  return { costless, required };
}
