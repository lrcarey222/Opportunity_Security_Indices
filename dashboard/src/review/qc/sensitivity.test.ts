// Tests of the bundle-layer robustness classification (qc/sensitivity.ts, design 4.6): fragile / sensitive /
// stable / not_testable on synthetic posture cells, fence_change, possible flips the bundle cannot decide, AIB
// node classes from the shipped variants and growth bounds, and the summary over the real bundle.

import { describe, expect, it } from 'vitest';
import { thresholdsFromCore } from './choiceRule.ts';
import { aibNodeRobustness, postureRobustness, postureSpecs, summariseRobustness } from './sensitivity.ts';
import { HAVE_REAL, loadReal, mkNode, mkPosture, triageTrace, type CellSpec } from './__tests__/fixtures.ts';

const ly = (n: number, result = 'yes') => [
  { step: 'secure', test: 'secure_override', result: 'no' },
  { step: 'large_yard', test: 'large_yard_case', value: String(n), threshold: '5', result },
  { step: 'choice', test: 'choice', result: 'watch' },
];

function robust(specs: CellSpec[]) {
  const p = mkPosture(specs);
  const run = postureRobustness(p.core, p.details);
  return Object.fromEntries(run.records.map((r) => [`${r.case_key.tech}|${r.case_key.stage}`, r]));
}

describe('posture cell robustness', () => {
  const r = robust([
    // high need just over the bar, low prize, low position: Partner; need +0.05 makes it Watch (Medium -> Low fence)
    { tech: 'Batteries', choice: 'partner', need: 0.69, prize: 0.3, cp: 0.1, share: 0, pathway: 'no', trace: triageTrace({ need: 0.69, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'partner' }) },
    // a large-yard Watch on exactly 5 signals with high need: min_signals 6 sends it to the triage (Partner)
    { tech: 'Solar', choice: 'watch', need: 0.9, prize: 0.3, cp: 0.1, share: 0, pathway: 'no', trace: ly(5) },
    // far from every bar
    { tech: 'Wind', choice: 'watch', need: 0.2, prize: 0.2, cp: 0.1, share: 0, pathway: 'no', trace: triageTrace({ need: 0.2, prize: 0.2, cp: 0.1, pos: 'not_top_quarter', choice: 'watch' }) },
    { tech: 'Coal', choice: 'not_assessed', need: null, prize: 0.3, trace: triageTrace({ need: null, prize: 0.3, choice: 'not_assessed' }) },
    { tech: 'Oil', choice: 'secure', need: 0.99, prize: 0.3, trace: [{ step: 'secure', test: 'secure_override', result: 'yes' }, { step: 'choice', test: 'choice', result: 'secure' }] },
    // need shipped as 0.620: exactly the need_minus bar, so the bundle cannot say which side the true value is on
    { tech: 'Gas', choice: 'watch', need: 0.62, prize: 0.3, cp: 0.1, share: 0, pathway: 'no', trace: triageTrace({ need: 0.62, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'watch' }) },
  ]);
  it('fragile: a local threshold move flips it, with the driver and the fence change', () => {
    const x = r['Batteries|Refining'];
    expect(x.class).toBe('fragile');
    expect(x.local_flips.map((f) => f.spec_id)).toContain('need_plus');
    expect(x.local_flips.find((f) => f.spec_id === 'need_plus')).toMatchObject({ choice: 'watch', fence_changed: true });
    expect(x.fence_change).toBe(true);
    expect(x.drivers).toContain('supply_chain_need');
  });
  it('sensitive: only a structural spec (large-yard min_signals 6) flips it', () => {
    const x = r['Solar|Refining'];
    expect(x.class).toBe('sensitive');
    expect(x.local_flips).toEqual([]);
    expect(x.structural_flips.map((f) => [f.spec_id, f.choice])).toEqual([['large_min6', 'partner']]);
    expect(x.drivers).toEqual(['large_yard_min_signals']);
  });
  it('stable: no spec flips it', () => {
    const x = r['Wind|Refining'];
    expect(x.class).toBe('stable');
    expect(x.specs_run).toBe(postureSpecs(thresholdsFromCore(mkPosture([{ choice: 'watch' }]).core)).length);
    expect(x.fence_change).toBe(false);
  });
  it('not_testable: not assessed and Secure, each with the reason', () => {
    expect(r['Coal|Refining']).toMatchObject({ class: 'not_testable', specs_run: 0 });
    expect(r['Coal|Refining'].note).toMatch(/not assessed/);
    expect(r['Oil|Refining']).toMatchObject({ class: 'not_testable' });
    expect(r['Oil|Refining'].note).toMatch(/security override/);
  });
  it('a possible flip the bundle cannot decide is never stable', () => {
    const x = r['Gas|Refining'];
    expect(x.class).toBe('fragile');
    expect(x.local_flips).toEqual([]);
    expect(x.indeterminate.map((i) => i.spec_id)).toEqual(['need_minus']);
    expect(x.note).toMatch(/cannot decide/);
  });
  it('specs: local thresholds +/- 0.05, strict cp, structural large-yard 4 / 6, position_first off, the TS fallback deployment 0.67', () => {
    const ids = postureSpecs({ need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, largeMin: 5, positionFirst: true, cpStrict: false }).map((s) => `${s.kind}:${s.spec_id}`);
    expect(ids).toEqual([
      'local:need_minus', 'local:need_plus', 'local:prize_minus', 'local:prize_plus', 'local:cp_minus', 'local:cp_plus', 'local:cp_strict', 'local:deployment_minus',
      'local:deployment_plus', 'structural:large_min4', 'structural:large_min6', 'structural:position_first_off', 'structural:deployment_fallback',
    ]);
  });
});

describe('AIB node robustness from the shipped variants', () => {
  const variants = (o: Record<string, string | null>) => ({ china_50: 'BUILD', largest_subsector: 'BUILD', largest_stage: 'BUILD', largest_technology: 'BUILD', diversify_benchmark: null, conservative: 'BUILD', release: 'BUILD', production_constrained: 'BUILD', trade_only: 'BUILD', ...o });
  const growth = (cls125 = 'BUILD') => [
    { label: '1.25x', existing: 0.6, dividend: 0.1, cls: cls125 },
    { label: '2x', existing: 0.6, dividend: 0.1, cls: 'BUILD' },
  ];
  it('china_50 or a growth bound changing the class is local (fragile)', () => {
    expect(aibNodeRobustness(mkNode({ id: 'a', variants: variants({ china_50: 'LOW INTERVENTION' }), growth: growth() })).class).toBe('fragile');
    const g = aibNodeRobustness(mkNode({ id: 'b', variants: variants({}), growth: growth('COORDINATE') }));
    expect(g).toMatchObject({ class: 'fragile', drivers: ['existing_relationship_growth'] });
  });
  it('release semantics, conservative supply, trade-only and other shocks are structural (sensitive)', () => {
    const x = aibNodeRobustness(mkNode({ id: 'c', variants: variants({ release: 'LOW INTERVENTION' }), growth: growth() }));
    expect(x.class).toBe('sensitive');
    expect(x.structural_flips[0]).toMatchObject({ spec_id: 'release', choice: 'LOW INTERVENTION', fence_changed: true });
  });
  it('stable when no variant differs; not_testable for INSUFFICIENT DATA and for scenarios without variants', () => {
    expect(aibNodeRobustness(mkNode({ id: 'd', variants: variants({}), growth: growth() })).class).toBe('stable');
    expect(aibNodeRobustness(mkNode({ id: 'e', cls: 'INSUFFICIENT DATA', variants: variants({}) })).class).toBe('not_testable');
    expect(aibNodeRobustness(mkNode({ id: 'f', variants: variants({}) }), 'managed').class).toBe('not_testable');
  });
});

describe.skipIf(!HAVE_REAL)('robustness over the shipped bundle', () => {
  it('classifies every cell; not_testable is exactly not assessed + Secure; fragile always has a local driver', () => {
    const { decoded } = loadReal();
    const p = decoded.posture!;
    const run = postureRobustness(p.core, p.details);
    const s = summariseRobustness(run.records);
    expect(s.records).toBe(p.core.cells.length);
    const na = p.core.cells.filter((c) => c.posture === 'N').length;
    const sec = p.core.cells.filter((c) => c.posture === 'S').length;
    expect(s.by_class.not_testable).toBe(na + sec);
    for (const r of run.records.filter((x) => x.class === 'fragile')) expect(r.local_flips.length + r.indeterminate.filter((i) => i.kind === 'local').length).toBeGreaterThan(0);
    for (const r of run.records.filter((x) => x.class === 'stable')) expect(r.local_flips.length + r.structural_flips.length + r.indeterminate.length).toBe(0);
    const aib = decoded.aib!.doc.nodes.map((n) => aibNodeRobustness(n));
    expect(aib.length).toBe(decoded.aib!.doc.nodes.length);
  });
});
