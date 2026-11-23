// Golden regression cases on the shipped bundles (docs/QC_SYSTEM_DESIGN.md 4.5, 6.1): every case of
// config/qc/golden_cases.yml is evaluated against the decoded bundles through the case snapshot builder, so a
// golden expectation reads exactly the fact a reviewer would see. exact: true requires the choice to equal
// expect.choice; otherwise the choice must be in expect.choice_in (or equal expect.choice); fence and numeric
// ranges are checked whenever given. A subject the release does not contain is 'missing', a numeric fact the
// release does not carry is a failure (never skipped). Pure: no IO.

import type { GoldenCase, GoldenResult } from '../model.ts';
import type { DecodedBundles } from './bundles.ts';
import { buildCaseSnapshot, snapshotFound, type SnapshotContext } from './snapshot.ts';

const fmt = (v: unknown): string => (v === null || v === undefined ? 'null' : typeof v === 'number' ? String(Math.round(v * 1e6) / 1e6) : String(v));

/** Evaluate one golden case. */
export function evaluateGoldenCase(g: GoldenCase, b: DecodedBundles, ctx: SnapshotContext = {}): GoldenResult {
  const ex = g.expect ?? {};
  if (ex.choice === undefined && ex.choice_in === undefined && ex.fence === undefined && ex.numeric === undefined) {
    return { id: g.id, status: 'not_applicable', observed: {}, failures: ['the case states no expectation'] };
  }
  const snap = buildCaseSnapshot({ kind: g.kind, key: g.key }, b, ctx);
  if (!snapshotFound(snap)) return { id: g.id, status: 'missing', observed: { choice: null }, failures: [snap.conclusion.reason ?? 'the subject is not in this release'] };
  const observed: Record<string, number | string | null> = { choice: snap.conclusion.value };
  const failures: string[] = [];
  const choice = snap.conclusion.value;
  if (g.exact && ex.choice !== undefined) {
    if (choice !== ex.choice) failures.push(`choice ${choice}, expected exactly ${ex.choice}`);
  } else if (ex.choice_in !== undefined || ex.choice !== undefined) {
    const allowed = [...(ex.choice_in ?? []), ...(ex.choice !== undefined ? [ex.choice] : [])];
    if (!allowed.includes(choice)) failures.push(`choice ${choice}, expected one of ${allowed.join(', ')}`);
  }
  if (ex.fence !== undefined) {
    const f = snap.facts.fence?.value ?? null;
    observed.fence = typeof f === 'string' ? f : null;
    if (g.kind !== 'posture_cell') failures.push(`expect.fence applies to posture cells only (kind ${g.kind})`);
    else if (f !== ex.fence) failures.push(`fence ${fmt(f)}, expected ${ex.fence}`);
  }
  for (const [id, range] of Object.entries(ex.numeric ?? {})) {
    const fact = snap.facts[id];
    const v = fact?.value;
    observed[id] = typeof v === 'number' ? v : v === undefined ? null : typeof v === 'boolean' ? String(v) : v;
    if (!fact) failures.push(`${id}: not a fact of a ${g.kind} case`);
    else if (typeof v !== 'number' || !Number.isFinite(v)) failures.push(`${id}: not a number in this release (${fact.note ?? fmt(v)})`);
    else if (v < range[0] || v > range[1]) failures.push(`${id} ${fmt(v)} outside [${range[0]}, ${range[1]}]`);
  }
  return { id: g.id, status: failures.length ? 'fail' : 'pass', observed, failures };
}

/** Evaluate every golden case, in the file's order. */
export function evaluateGoldenCases(cases: readonly GoldenCase[], b: DecodedBundles, ctx: SnapshotContext = {}): GoldenResult[] {
  return cases.map((g) => evaluateGoldenCase(g, b, ctx));
}

/** Counts by status. */
export function goldenSummary(results: readonly GoldenResult[]): Record<'pass' | 'fail' | 'missing' | 'not_applicable', number> {
  const out = { pass: 0, fail: 0, missing: 0, not_applicable: 0 };
  for (const r of results) out[r.status]++;
  return out;
}
