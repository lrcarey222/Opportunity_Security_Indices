// The specialization-resilience frontier of a node: the optimized solution re-solved under each
// supplier-share limit (no candidate exporter may provide more than 25 / 40 / 60 / 100% of the supply),
// with its coverage, two readings of N-1 resilience and the supply-weighted specialization of the
// exporters it uses. N-1 re-optimized is the true test: the largest remaining supplier is removed and the
// trade re-optimized. Instantaneous N-1 retained coverage is mechanical: coverage x (1 - the largest
// supplier's share), nothing re-routed. Two readings come with the frontier: the tightest limit that
// costs at most 2 points of coverage, and the loosest limit that keeps coverage at 80% or more after the
// N-1 test (the least diversification that buys that resilience; the exporter reads it on the
// re-optimized N-1). An older bundle wrote only the instantaneous reading, as secondShock. Pure.

import { isNum } from '../../lib/format.ts';
import type { AibNode } from '../contract.ts';
import { nameOf } from './format.ts';

export type FrontierKey = 'coverage' | 'n1Reoptimized' | 'instantaneousN1' | 'specialization';

export const N1_REOPTIMIZED_LABEL = 'N-1 re-optimized (largest remaining supplier removed, trade re-optimized)';
export const INSTANTANEOUS_N1_LABEL = 'Instantaneous N-1 retained coverage (coverage × (1 − largest share))';

/**
 * The four lines, in legend order. Colours: blue, olive and rose from the validated categorical triple
 * (coverage, the true N-1 test, specialization) and the teal of the validated stage triple for the
 * mechanical N-1 reading (blue / olive / teal pass as adjacent pairs); every line has its own dash, so
 * colour is never the only cue. `short` labels the line ends.
 */
export const FRONTIER_SERIES: readonly { key: FrontierKey; label: string; short: string; color: string; dash: string | null }[] = [
  { key: 'coverage', label: 'Coverage', short: 'Coverage', color: '#1889b0', dash: null },
  { key: 'n1Reoptimized', label: N1_REOPTIMIZED_LABEL, short: 'N-1 re-optimized', color: '#8b9d17', dash: '6,4' },
  { key: 'instantaneousN1', label: INSTANTANEOUS_N1_LABEL, short: 'Instantaneous N-1', color: '#2fa5a6', dash: '1,3' },
  { key: 'specialization', label: 'Specialization score', short: 'Specialization score', color: '#c96f7a', dash: '2,3' },
];

export interface FrontierRow {
  cap: number;
  coverage: number | null;
  n1Reoptimized: number | null;
  instantaneousN1: number | null;
  /** the supplier the N-1 test removes, and its name */
  n1Removed: string | null;
  n1RemovedName: string | null;
  specialization: number | null;
  largest: string | null;
  largestName: string | null;
  largestShare: number | null;
}

const num = (x: number | null | undefined): number | null => (isNum(x) ? x : null);

/** The frontier of a node by supplier limit, tightest first. */
export function frontierRows(n: AibNode, names: Readonly<Record<string, string>>): FrontierRow[] {
  return [...n.frontier]
    .sort((a, b) => a.cap - b.cap)
    .map((f) => {
      const removed = f.n1Removed ?? null;
      return {
        cap: f.cap,
        coverage: num(f.coverage),
        n1Reoptimized: num(f.n1Reoptimized),
        // an older bundle wrote the instantaneous reading as secondShock
        instantaneousN1: num(f.instantaneousN1) ?? num(f.secondShock),
        n1Removed: removed,
        n1RemovedName: removed ? nameOf(names, removed) : null,
        specialization: num(f.specialization),
        largest: f.largest,
        largestName: f.largest ? nameOf(names, f.largest) : null,
        largestShare: num(f.largestShare),
      };
    });
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

/** True when the node's frontier carries the true (re-optimized) N-1 test, which the resilience reading then uses. */
export const hasReoptimizedN1 = (n: AibNode): boolean => n.frontier.some((f) => isNum(f.n1Reoptimized));

/** The two readings of a node's frontier in words (null fields: the export has none). */
export function frontierReadings(n: AibNode): { costless: string | null; required: string | null } {
  const costless = n.tightestCostless === undefined ? null : isNum(n.tightestCostless) ? capLabel(n.tightestCostless) : '–';
  const required = n.minRequiredDiversification === undefined ? null : isNum(n.minRequiredDiversification) ? capLabel(n.minRequiredDiversification) : 'no tested limit reaches it';
  return { costless, required };
}
