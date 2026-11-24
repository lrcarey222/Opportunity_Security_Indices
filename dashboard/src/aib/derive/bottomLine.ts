// The Overview's bottom line (external review readiness, 2026-11-23): "Where does coordination actually change
// the outcome?" as three short lists read from the toggled scenario's node results, the same rows the
// Overview's class tiles, "Hardest-hit supply-chain steps" and "Where acting together recovers most" read
// (nodeScenario): the largest coordination dividends; the Coordinate trade steps, by the share of demand new
// or expanded trade recovers; the Build capacity steps, by the remaining supply gap (the low-intervention
// count is the class tile's own). Nothing is re-classified or re-scored: a step's class is the scenario's own. Pure.

import type { NodeScenario } from './scenarios.ts';
import { topScenarioNodes } from './scenarioPages.ts';

export interface CoordinationBottomLine {
  /** the steps where acting together recovers the most demand over existing relationships (dividend > 0) */
  dividend: NodeScenario[];
  /** the Coordinate trade steps, by the share of demand new or expanded trade recovers */
  trade: NodeScenario[];
  /** the Build capacity steps, by the share of demand still uncovered after coordinated trade */
  build: NodeScenario[];
}

const positive = (v: number | null): number | null => (v !== null && v > 0 ? v : null);

/** The bottom line over the rows of one scenario (null under Today's network: nothing is disrupted, nothing to recover). */
export function coordinationBottomLine(rows: readonly NodeScenario[], n = 3): CoordinationBottomLine | null {
  const ran = rows.filter((r) => r.ran && r.cls && r.id !== 'baseline');
  if (!ran.length) return null;
  const of = (cls: string) => ran.filter((r) => r.cls === cls);
  return {
    dividend: topScenarioNodes(ran, (r) => positive(r.dividend), n),
    trade: topScenarioNodes(of('COORDINATE'), (r) => positive(r.split?.trade ?? null), n),
    build: topScenarioNodes(of('BUILD'), (r) => positive(r.residual), n),
  };
}
