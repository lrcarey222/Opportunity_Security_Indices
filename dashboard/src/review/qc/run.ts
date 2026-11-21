// One run of the shipped-bundle QC layer (docs/QC_SYSTEM_DESIGN.md 6.1, 6.3): the bundle checks, the choice-rule
// reproduction, per-cell and per-node robustness and the golden cases, on decoded bundles. Both Node CLIs
// (dashboard/scripts/review/run-bundle-qc.mjs and build-review-data.mjs) call this so the CSVs in qc/results and
// the admin dashboard's qc_summary.json always describe the same run. Golden results become check rows
// (BQ-GOLDEN-00 file validity, BQ-GOLDEN-01 results), so "any row fails" is the single exit criterion. Pure: no IO.

import type { GoldenCase, GoldenResult, RobustnessRecord } from '../model.ts';
import type { PostureThresholds } from '../../posture/contract.ts';
import type { DecodedBundles, RawBundles } from './bundles.ts';
import { qcSummary, runBundleQc, type QcRow, type QcStatus } from './bundleQc.ts';
import { reproduceShippedChoices, type ReproductionRow, type ReproductionSummary } from './choiceRule.ts';
import { evaluateGoldenCases, goldenSummary } from './golden.ts';
import { aibNodeRobustness, postureRobustness, summariseRobustness, SPECS_NOT_RUNNABLE_ON_BUNDLE, type PostureRobustnessRun, type RobustnessSummary } from './sensitivity.ts';

export interface ShippedQcInput {
  decoded: DecodedBundles;
  raw?: RawBundles | null;
  /** problems the loader met (missing files) */
  loadProblems?: readonly string[];
  /** DEFAULT_THRESHOLDS of posture/contract.ts (bundleQc parseDefaultThresholds on its source) */
  fallbackThresholds?: Partial<PostureThresholds> | null;
  releaseId?: string | null;
  /** the parsed golden cases and their validation errors; null when no golden file was given */
  golden?: { cases: readonly GoldenCase[]; errors: readonly string[] } | null;
}

export interface ShippedQcRun {
  releaseId: string | null;
  rows: QcRow[];
  status: Record<QcStatus, number>;
  reproduction: { rows: ReproductionRow[]; summary: ReproductionSummary } | null;
  posture: PostureRobustnessRun | null;
  postureSummary: RobustnessSummary | null;
  aib: RobustnessRecord[];
  aibSummary: RobustnessSummary | null;
  golden: { cases: readonly GoldenCase[]; results: GoldenResult[]; summary: ReturnType<typeof goldenSummary> } | null;
}

const classLine = (s: RobustnessSummary) => `stable ${s.by_class.stable}, sensitive ${s.by_class.sensitive}, fragile ${s.by_class.fragile}, not testable ${s.by_class.not_testable}; fence change ${s.fence_change}`;

/** Run the whole shipped-bundle layer once. */
export function runShippedQc(input: ShippedQcInput): ShippedQcRun {
  const p = input.decoded.posture;
  const reproduction = p ? reproduceShippedChoices(p.core, p.details) : null;
  const rows = runBundleQc({ decoded: input.decoded, raw: input.raw, loadProblems: input.loadProblems, reproduction, fallbackThresholds: input.fallbackThresholds, releaseId: input.releaseId });
  const posture = p ? postureRobustness(p.core, p.details) : null;
  const postureSummary = posture ? summariseRobustness(posture.records) : null;
  const aib = input.decoded.aib ? input.decoded.aib.doc.nodes.map((n) => aibNodeRobustness(n, 'decoupling')) : [];
  const aibSummary = input.decoded.aib ? summariseRobustness(aib) : null;
  const add = (r: Omit<QcRow, 'layer'>) => rows.push({ ...r, layer: 'bundle' });
  if (postureSummary)
    add({
      check_id: 'BQ-ROBUST-01', area: 'robustness', status: 'info', n: postureSummary.records,
      detail: `posture cells under the bundle-layer specs: ${classLine(postureSummary)}. Not runnable on the bundle (pipeline layer): ${SPECS_NOT_RUNNABLE_ON_BUNDLE.join('; ')}.`,
      examples: Object.entries(postureSummary.by_spec).sort((a, b) => b[1] - a[1]).map(([k, v]) => `${k}: ${v} cells flip${postureSummary.indeterminate_by_spec[k] ? ` (+${postureSummary.indeterminate_by_spec[k]} possible)` : ''}`),
    });
  if (aibSummary)
    add({
      check_id: 'BQ-ROBUST-02', area: 'robustness', status: 'info', n: aibSummary.records,
      detail: `AIB nodes (Decoupling) under the shipped class variants and growth bounds: ${classLine(aibSummary)}`,
      examples: Object.entries(aibSummary.by_spec).sort((a, b) => b[1] - a[1]).map(([k, v]) => `${k}: ${v} nodes change class`),
    });
  let golden: ShippedQcRun['golden'] = null;
  if (input.golden) {
    const errs = [...input.golden.errors];
    add({
      check_id: 'BQ-GOLDEN-00', area: 'golden', status: errs.length ? 'fail' : 'pass', n: errs.length,
      detail: errs.length ? `${errs.length} validation errors in config/qc/golden_cases.yml` : `${input.golden.cases.length} golden cases, valid`, examples: errs.slice(0, 8),
    });
    const results = evaluateGoldenCases(input.golden.cases, input.decoded);
    const summary = goldenSummary(results);
    const bad = results.filter((r) => r.status === 'fail' || r.status === 'missing');
    add({
      check_id: 'BQ-GOLDEN-01', area: 'golden', status: bad.length ? 'fail' : 'pass', n: bad.length,
      detail: bad.length
        ? `${bad.length} of ${results.length} golden cases fail or are missing (a change fails until NEIS re-approves the case): pass ${summary.pass}, fail ${summary.fail}, missing ${summary.missing}, not applicable ${summary.not_applicable}`
        : `all ${summary.pass} golden cases pass${summary.not_applicable ? ` (${summary.not_applicable} not applicable)` : ''}`,
      examples: bad.map((r) => `${r.id} ${r.status}: ${r.failures.join('; ')}`).slice(0, 8),
    });
    golden = { cases: input.golden.cases, results, summary };
  }
  return { releaseId: input.releaseId ?? null, rows, status: qcSummary(rows), reproduction, posture, postureSummary, aib, aibSummary, golden };
}

/** True when any check row fails (the CLIs' exit criterion; golden failures included). */
export const qcFailed = (run: Pick<ShippedQcRun, 'rows'>): boolean => run.rows.some((r) => r.status === 'fail');
