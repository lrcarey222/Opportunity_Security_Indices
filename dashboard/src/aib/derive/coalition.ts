// The coalition curve: the greedy minimum sufficient coalition of a compact, step by step. Step 0 is the
// demand anchors alone; each later step adds the economy that most reduces the shortfall. The minimum
// (and mean) coverage of the anchors' demand over the compact's nodes is read against the coverage
// targets (90 / 95 / 99%); the first step at or above a target is that target's coalition. Pure.

import { isNum } from '../../lib/format.ts';
import { nodeCoverageOf, type AibCompact } from '../contract.ts';
import { nameOf } from './format.ts';

export interface CurvePoint {
  step: number;
  /** members in the coalition at this step */
  n: number;
  /** the economy added (null at step 0) */
  added: string | null;
  /** its name, or "Demand anchors" at step 0 */
  addedName: string;
  members: string[];
  min: number | null;
  mean: number | null;
  /** node id -> coverage */
  nodes: Record<string, number>;
}

export interface TargetMark {
  target: number;
  /** the first point reaching the target, or null when the path never does */
  point: CurvePoint | null;
}

const TOL = 1e-9;

export function coalitionCurve(c: AibCompact, names: Readonly<Record<string, string>>): CurvePoint[] {
  return [...c.path]
    .sort((a, b) => a.step - b.step)
    .map((s) => ({
      step: s.step,
      n: s.members.length,
      added: s.added,
      addedName: s.added ? nameOf(names, s.added) : 'Demand anchors',
      members: s.members,
      min: isNum(s.minCoverage) ? s.minCoverage : null,
      mean: isNum(s.meanCoverage) ? s.meanCoverage : null,
      nodes: nodeCoverageOf(s),
    }));
}

/** Each target with the first point whose minimum coverage reaches it. */
export function targetMarks(points: readonly CurvePoint[], targets: readonly number[]): TargetMark[] {
  return [...targets]
    .sort((a, b) => a - b)
    .map((t) => ({ target: t, point: points.find((p) => p.min !== null && p.min >= t - TOL) ?? null }));
}

/** The y-axis floor of the curve: the lowest value shown, rounded down to 10%, at most 0.8. */
export function curveFloor(points: readonly CurvePoint[]): number {
  const vals = points.flatMap((p) => [p.min, p.mean]).filter(isNum);
  if (!vals.length) return 0;
  return Math.max(0, Math.min(0.8, Math.floor(Math.min(...vals) * 10) / 10));
}

/** "90%" for 0.9, "99%" for 0.99. */
export const targetLabel = (t: number): string => `${Math.round(t * 1000) / 10}%`;
