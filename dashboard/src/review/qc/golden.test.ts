// Tests of the golden-case runner (qc/golden.ts): exact match, allowed set, fence and numeric ranges, a numeric fact
// the release does not carry (a failure, never skipped), a subject not in the release ('missing'), no expectation
// ('not_applicable'); and config/qc/golden_cases.yml on the shipped bundles (a failure fails CI until re-approved).

import { existsSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { parse } from 'yaml';
import { describe, expect, it } from 'vitest';
import type { GoldenCase } from '../model.ts';
import { validateGoldenCases } from '../manifest.ts';
import { evaluateGoldenCase, evaluateGoldenCases, goldenSummary } from './golden.ts';
import { HAVE_REAL, loadReal, mkDecoded, mkPosture, triageTrace } from './__tests__/fixtures.ts';

const gc = (o: Partial<GoldenCase> & Pick<GoldenCase, 'expect'>): GoldenCase => ({
  id: 'GC_900', category: 'near_threshold', kind: 'posture_cell', key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }, exact: true,
  change_requires_review: true, rationale: 'test', approved: { by: 'test', on: '2026-11-20', release: null }, ...o,
});

describe('evaluateGoldenCase', () => {
  const b = mkDecoded({
    posture: mkPosture([
      { iso3: 'KOR', tech: 'Batteries', stage: 'Components', choice: 'catch_up', need: 0.73, prize: 0.79, cp: 0.71, share: 0.06, pathway: 'yes', trace: triageTrace({ need: 0.73, prize: 0.79, cp: 0.71, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }) },
    ]),
  });
  it('exact: passes on the same choice and fails on another', () => {
    expect(evaluateGoldenCase(gc({ expect: { choice: 'catch_up', fence: 'medium' } }), b).status).toBe('pass');
    const r = evaluateGoldenCase(gc({ expect: { choice: 'preserve' } }), b);
    expect(r).toMatchObject({ status: 'fail', observed: { choice: 'catch_up' } });
    expect(r.failures[0]).toMatch(/expected exactly preserve/);
  });
  it('allowed set when not exact', () => {
    expect(evaluateGoldenCase(gc({ exact: false, expect: { choice_in: ['preserve', 'catch_up'] } }), b).status).toBe('pass');
    expect(evaluateGoldenCase(gc({ exact: false, expect: { choice_in: ['preserve', 'partner'] } }), b).status).toBe('fail');
  });
  it('numeric ranges, and a numeric fact the release lacks is a failure', () => {
    expect(evaluateGoldenCase(gc({ expect: { numeric: { cp: [0.7, 0.72] } } }), b).status).toBe('pass');
    const out = evaluateGoldenCase(gc({ expect: { numeric: { cp: [0.75, 1] } } }), b);
    expect(out.failures[0]).toMatch(/cp 0.71 outside \[0.75, 1\]/);
    const na = evaluateGoldenCase(gc({ expect: { numeric: { deployment_need: [0, 1] } } }), b);
    expect(na.status).toBe('fail');
    expect(na.failures[0]).toMatch(/not a number in this release/);
  });
  it('a subject not in the release is missing; no expectation is not_applicable', () => {
    expect(evaluateGoldenCase(gc({ key: { iso3: 'ZZZ', tech: 'Batteries', stage: 'Components' }, expect: { choice: 'watch' } }), b).status).toBe('missing');
    expect(evaluateGoldenCase(gc({ expect: {} }), b).status).toBe('not_applicable');
  });
  it('summarises by status', () => {
    const rs = evaluateGoldenCases([gc({ expect: { choice: 'catch_up' } }), gc({ id: 'GC_901', expect: { choice: 'watch' } })], b);
    expect(goldenSummary(rs)).toEqual({ pass: 1, fail: 1, missing: 0, not_applicable: 0 });
  });
});

const GOLDEN = fileURLToPath(new URL('../../../../config/qc/golden_cases.yml', import.meta.url));

describe.skipIf(!HAVE_REAL || !existsSync(GOLDEN))('config/qc/golden_cases.yml on the shipped bundles', () => {
  it('is valid and every case passes (a change fails until NEIS re-approves the case)', () => {
    const doc = parse(readFileSync(GOLDEN, 'utf8')) as { cases: GoldenCase[] };
    const v = validateGoldenCases(doc);
    expect(v.errors).toEqual([]);
    const rs = evaluateGoldenCases(doc.cases, loadReal().decoded);
    expect(rs.filter((r) => r.status !== 'pass').map((r) => `${r.id} ${r.status}: ${r.failures.join('; ')}`)).toEqual([]);
  });
});
