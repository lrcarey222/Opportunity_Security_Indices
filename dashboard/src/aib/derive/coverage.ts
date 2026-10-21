// Coverage steps: the share of the demand scope's baseline traded demand served after the shock,
// from the static shock (nothing reroutes) through existing relationships and the trade compact to the
// trade + investment response. Pure.

import { isNum } from '../../lib/format.ts';
import type { AibMetrics, AibNode } from '../contract.ts';

export type CoverageKey = 'static' | 'existing' | 'trade' | 'investment';

export interface CoverageStep {
  key: CoverageKey;
  /** the step as the chart and the table name it */
  label: string;
  /** a short label (card strips) */
  short: string;
  /** what the step assumes */
  meaning: string;
  value: number | null;
}

export const COVERAGE_STEPS: readonly Omit<CoverageStep, 'value'>[] = [
  { key: 'static', label: 'Shock (static)', short: 'Shock', meaning: 'the shock alone: surviving shipments stay where they were and nothing is rerouted' },
  { key: 'existing', label: 'Existing relationships', short: 'Existing', meaning: 'surviving suppliers ship more along the trade relationships that already exist' },
  {
    key: 'trade',
    label: 'Trade compact',
    short: 'Trade',
    meaning: "every economy keeps its existing-relationship response, and the compact's minimum-coalition members also open or expand relationships among themselves",
  },
  {
    key: 'investment',
    label: 'Trade + investment response',
    short: '+ Investment',
    meaning: 'the trade compact plus the synthetic capacity increment (capped at each host’s current output) at up to two member hosts in each BUILD node',
  },
];

/** The first step under Managed Interdependence (2026-10-19): it removes no supplier; the cap takes the excess away. */
export const MANAGED_STATIC_STEP: Omit<CoverageStep, 'value'> = {
  key: 'static',
  label: 'Cap (static)',
  short: 'Cap',
  meaning: 'the cap alone: suppliers above it in the allied market lose the excess and nothing is rerouted',
};

/** The four coverage steps of a compact (demand-weighted over its nodes); `managed`: the first step is the cap of Managed Interdependence. */
export function coverageSteps(m: AibMetrics, managed = false): CoverageStep[] {
  const v: Record<CoverageKey, number | null> = { static: m.static, existing: m.existing, trade: m.trade, investment: m.investment };
  return COVERAGE_STEPS.map((s) => ({ ...(managed && s.key === 'static' ? MANAGED_STATIC_STEP : s), value: isNum(v[s.key]) ? v[s.key] : null }));
}

/** The reference beside the steps: coverage if every economy coordinated (null when the export has none). */
export function globalOptimum(m: AibMetrics): number | null {
  return isNum(m.globalOptimum) ? m.globalOptimum : null;
}

export const GLOBAL_OPTIMUM_LABEL = 'If every economy coordinated';

/**
 * Compact vs fragmented action: the demand anchors' coverage through their existing relationships and
 * under the compact's minimum coalition, and the gain (the exported one, else the difference); null
 * when the export carries neither coverage.
 */
export function compactVsFragmented(m: AibMetrics): { fragmented: number | null; compact: number | null; gain: number | null } | null {
  const fragmented = isNum(m.anchorFragmented) ? m.anchorFragmented : null;
  const compact = isNum(m.anchorCompact) ? m.anchorCompact : null;
  if (fragmented === null && compact === null) return null;
  const gain = isNum(m.compactGain) ? m.compactGain : fragmented !== null && compact !== null ? compact - fragmented : null;
  return { fragmented, compact, gain };
}

/** The three coverage steps of one node (it has no compact-level investment response). */
export function nodeCoverageSteps(n: AibNode): CoverageStep[] {
  const v: Partial<Record<CoverageKey, number | null>> = { static: n.static, existing: n.existing, trade: n.optimized };
  return COVERAGE_STEPS.filter((s) => s.key !== 'investment').map((s) => ({ ...s, value: isNum(v[s.key]) ? (v[s.key] as number) : null }));
}

/** The largest step-to-step gain of a set of steps (null when fewer than two values). */
export function largestGain(steps: readonly CoverageStep[]): { from: CoverageStep; to: CoverageStep; gain: number } | null {
  let best: { from: CoverageStep; to: CoverageStep; gain: number } | null = null;
  for (let i = 1; i < steps.length; i++) {
    const a = steps[i - 1].value;
    const b = steps[i].value;
    if (!isNum(a) || !isNum(b)) continue;
    if (!best || b - a > best.gain) best = { from: steps[i - 1], to: steps[i], gain: b - a };
  }
  return best;
}
