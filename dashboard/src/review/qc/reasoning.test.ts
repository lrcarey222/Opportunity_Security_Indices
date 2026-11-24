// Tests of the reasoning reveal (qc/reasoning.ts), UX v2: every posture branch of the choice rule (Secure,
// Deployment Accelerate / Watch, the large-yard Watch, triage Preserve, Catch Up, Partner and Watch, not assessed)
// names the rule actually applied and at least one neighbouring alternative with why_not, would_need and a margin;
// AIB nodes replay the class rule; compacts the materiality rule with the post-reveal chain; sector chains the lead
// stage and the chokepoint view; the partnership memo's boxes. Synthetic cells for the branches the shipped
// campaign lacks, the shipped bundles for the rest.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME } from '../../content/vocabulary.ts';
import type { CaseDefinition, CaseReasoning } from '../model.ts';
import { aibClassRule, buildCaseReasoning } from './reasoning.ts';
import { buildCaseSnapshot } from './snapshot.ts';
import { deriveCompactThreat } from './threat.ts';
import { HAVE_REAL, loadReal, mkDecoded, mkPosture } from './__tests__/fixtures.ts';
import type { DecodedBundles } from './bundles.ts';

const reason = (kind: CaseDefinition['kind'], key: CaseDefinition['key'], b: DecodedBundles): CaseReasoning => {
  const s = buildCaseSnapshot({ kind, key }, b);
  const r = buildCaseReasoning({ kind, key, explorer_path: '/x' }, b, s);
  expect(r, `${kind} ${JSON.stringify(key)}`).not.toBeNull();
  return r!;
};
const wellFormed = (r: CaseReasoning) => {
  expect(r.rule.length).toBeGreaterThan(20);
  expect(r.alternatives.length).toBeGreaterThan(0);
  for (const a of r.alternatives) {
    expect(a.value).not.toBe(r.conclusion.value);
    expect(a.why_not.length).toBeGreaterThan(10);
    expect(a.would_need.length).toBeGreaterThan(10);
  }
  expect(r.technical_path).toBe('/x');
};

type Step = { step: string; test: string; value?: number | string | null; threshold?: number | string | null; result: string };
const sec = (r = 'no'): Step => ({ step: 'secure', test: 'secure_override', result: r });

describe('posture branches on synthetic cells', () => {
  const p = mkPosture([
    // Deployment Accelerate
    { iso3: 'AAA', tech: 'Solar', stage: 'Deployment', choice: 'accelerate', need: 0.72, trace: [sec(), { step: 'need', test: 'deployment_need', value: 0.72, threshold: 0.6, result: 'high' }, { step: 'choice', test: 'choice', result: 'accelerate' }] },
    // the large-yard Watch: the commodity test decides before need and value are read
    { iso3: 'BBB', tech: 'Solar', stage: 'Components', choice: 'watch', need: 0.8, prize: 0.7, cp: 0.4, share: 0.02, pathway: 'yes', trace: [sec(), { step: 'large_yard', test: 'large_yard_case', value: 6, threshold: 5, result: 'yes' }, { step: 'choice', test: 'choice', result: 'watch' }] },
    // not assessed: need missing
    { iso3: 'CCC', tech: 'Solar', stage: 'Components', choice: 'not_assessed', need: null, prize: 0.7, trace: [sec(), { step: 'need', test: 'supply_chain_need', value: null, threshold: 0.67, result: 'not_assessed' }, { step: 'choice', test: 'choice', result: 'not_assessed' }], extra: { not_assessed_reason: 'Supply-chain need is not measured for this node.' } },
  ]);
  const b = mkDecoded({ posture: p });
  it('Deployment Accelerate: the deployment rule, Watch as the neighbour', () => {
    const r = reason('posture_cell', { iso3: 'AAA', tech: 'Solar', stage: 'Deployment' }, b);
    expect(r.conclusion.value).toBe('accelerate');
    expect(r.rule).toMatch(/Deployment & Use/);
    expect(r.steps.map((s) => s.id)).toContain('deployment');
    expect(r.alternatives[0]).toMatchObject({ value: 'watch' });
    expect(r.alternatives[0].margin).toMatch(/0\.720 vs bar 0\.6/);
    wellFormed(r);
  });
  it('the large-yard Watch: the commodity rule, and the triage result without it', () => {
    const r = reason('posture_cell', { iso3: 'BBB', tech: 'Solar', stage: 'Components' }, b);
    expect(r.conclusion.value).toBe('watch');
    expect(r.rule).toMatch(/commodity/);
    expect(r.steps.find((s) => s.id === 'large_yard')).toMatchObject({ level: 'Yes', value: 6, threshold: 5 });
    expect(r.alternatives[0]).toMatchObject({ value: 'catch_up' });
    expect(r.alternatives[0].would_need).toMatch(/fewer than 5 commodity signals/);
    wellFormed(r);
  });
  it('not assessed: no call, and what a call would be with the missing input measured', () => {
    const r = reason('posture_cell', { iso3: 'CCC', tech: 'Solar', stage: 'Components' }, b);
    expect(r.conclusion.value).toBe('not_assessed');
    expect(r.rule).toMatch(/makes no call/);
    // high value, no pathway: need measured high gives Partner, low gives Watch
    expect(r.alternatives.map((a) => a.value).sort()).toEqual(['partner', 'watch']);
    wellFormed(r);
  });
});

describe('the class rule of an AIB node (R aib_classify_nodes, first match wins)', () => {
  const t = { low_exposure: 0.15, build_gap: 0.1, manage_largest_share: 0.4, manage_top3_share: 0.75, manage_coverage_loss: 0.05, adequate_coverage: 0.9, coordinate_dividend: 0.05 };
  const x = { ok: true, exposure: 0.5, residual: 0.05, largest: 0.2, top3: null, loss: 0, existing: 0.8, dividend: 0.08 };
  it('applies each branch in order', () => {
    expect(aibClassRule({ ...x, ok: false }, t).cls).toBe('INSUFFICIENT DATA');
    expect(aibClassRule({ ...x, exposure: 0.1 }, t)).toEqual({ cls: 'LOW INTERVENTION', branch: 'low_exposure' });
    expect(aibClassRule({ ...x, residual: 0.2 }, t).cls).toBe('BUILD');
    expect(aibClassRule({ ...x, largest: 0.45 }, t).cls).toBe('MANAGE SPECIALIZATION');
    expect(aibClassRule({ ...x, existing: 0.95 }, t)).toEqual({ cls: 'LOW INTERVENTION', branch: 'adequate_existing' });
    expect(aibClassRule(x, t).cls).toBe('COORDINATE');
    expect(aibClassRule({ ...x, dividend: 0.01 }, t)).toEqual({ cls: 'LOW INTERVENTION', branch: 'default_low' });
  });
});

describe.skipIf(!HAVE_REAL)('reasoning of the shipped campaign', () => {
  const { decoded } = loadReal();
  const post = (iso3: string, tech: string, stage: string) => reason('posture_cell', { iso3, tech, stage }, decoded);
  it('triage Preserve (KOR battery components): Catch Up on position 0.763 vs 0.75', () => {
    const r = post('KOR', 'Batteries', 'Components');
    expect(r.conclusion.value).toBe('preserve');
    expect(r.steps.map((s) => s.id)).toEqual(['secure', 'large_yard', 'need', 'prize', 'position']);
    expect(r.steps.find((s) => s.id === 'position')).toMatchObject({ value: 0.763, threshold: 0.75 });
    expect(r.rule).toContain(CHOICE_NAME.preserve);
    expect(r.alternatives[0]).toMatchObject({ value: 'catch_up', margin: 'Position 0.763 vs bar 0.75 (+0.013)' });
    wellFormed({ ...r, technical_path: '/x' });
  });
  it('Secure (JPN EV refining): the override, and the triage result with it off', () => {
    const r = post('JPN', 'Electric Vehicles', 'Refining');
    expect(r.conclusion.value).toBe('secure');
    expect(r.steps[0]).toMatchObject({ id: 'secure', level: 'Yes' });
    expect(r.rule).toMatch(/acute vulnerability/);
    expect(r.alternatives[0].value).toBe('catch_up');
    wellFormed(r);
  });
  it('Secure (BEL oil): without the override the step is a commodity, so Watch', () => {
    expect(post('BEL', 'Oil', 'Extraction').alternatives[0].value).toBe('watch');
  });
  it('triage Catch Up (USA battery refining): the pathway anchor on the 0.5% line is the nearest neighbour', () => {
    const r = post('USA', 'Batteries', 'Refining');
    expect(r.conclusion.value).toBe('catch_up');
    expect(r.alternatives[0]).toMatchObject({ value: 'watch' });
    expect(r.alternatives[0].margin).toMatch(/0\.5% anchor line/);
  });
  it('triage Partner (IND solar components) and the commodity neighbour (IND battery minerals)', () => {
    const r = post('IND', 'Solar', 'Components');
    expect(r.conclusion.value).toBe('partner');
    expect(r.alternatives.map((a) => a.value)).toEqual(expect.arrayContaining(['preserve', 'catch_up']));
    const m = post('IND', 'Batteries', 'Extraction');
    expect(m.alternatives[0]).toMatchObject({ value: 'watch' });
    expect(m.alternatives[0].margin).toMatch(/Commodity signals 4 of 5/);
  });
  it('triage Watch (DEU grid components) and Deployment Watch (USA nuclear, 0.597 vs 0.6)', () => {
    const w = post('DEU', 'Electric Grid', 'Components');
    expect(w.conclusion.value).toBe('watch');
    expect(w.rule).toMatch(/a strong position alone does not justify intervention/);
    wellFormed({ ...w, technical_path: '/x' });
    const d = post('USA', 'Nuclear', 'Deployment');
    expect(d.alternatives[0]).toMatchObject({ value: 'accelerate', margin: 'Deployment need 0.597 vs bar 0.6 (−0.003)' });
  });
  it('AIB nodes: the class bars and the shipped variants', () => {
    const mag = reason('aib_node', { node: 'rare-earth-permanent-magnets', scenario: 'decoupling' }, decoded);
    expect(mag.conclusion.value).toBe('BUILD');
    expect(mag.steps.map((s) => s.id)).toEqual(expect.arrayContaining(['exposure', 'gap', 'concentration', 'coverage', 'dividend', 'variants']));
    expect(mag.steps.find((s) => s.id === 'variants')!.level).toBe('Same');
    const gra = reason('aib_node', { node: 'graphite-electrodes', scenario: 'decoupling' }, decoded);
    expect(gra.alternatives.map((a) => a.value)).toEqual(expect.arrayContaining(['BUILD', 'LOW INTERVENTION']));
    const poly = reason('aib_node', { node: 'polysilicon', scenario: 'decoupling' }, decoded);
    expect(poly.alternatives[0]).toMatchObject({ value: 'MANAGE SPECIALIZATION' });
    expect(poly.steps.find((s) => s.id === 'physical')).toBeTruthy();
    wellFormed({ ...poly, technical_path: '/x' });
  });
  it('compacts: the materiality rule, the US$ arm margin, and the post-reveal chain', () => {
    const grid = reason('compact', { sid: 'SC-GRID', scenario: 'managed' }, decoded);
    expect(grid.conclusion.value).toBe('sectoral');
    expect(grid.alternatives[0]).toMatchObject({ value: 'narrower' });
    expect(grid.alternatives[0].would_need).toMatch(/US\$26m above the US\$1bn line/);
    expect(grid.compact_chain!.map((c) => c.id)).toEqual(['coordinated_trade', 'new_capacity', 'hosts', 'coalition', 'resilience_after']);
    expect(grid.compact_chain!.find((c) => c.id === 'resilience_after')!.text).toMatch(/arithmetic by construction/);
    const nuc = reason('compact', { sid: 'SC-NUCLEAR', scenario: 'managed' }, decoded);
    expect(nuc.compact_chain!.find((c) => c.id === 'coalition')!.text).toMatch(/empty although/);
  });
  it('sector chains: the Explorer stages, the dollar lead and the production-flagged upstream stage', () => {
    const solar = reason('sector_chain', { sid: 'SC-SOLAR', scenario: 'decoupling' }, decoded);
    expect(solar.conclusion.value).toBe('Assembly');
    expect(solar.sector!.explorer_stages).toEqual(['Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment']);
    expect(solar.sector!.lead_stage).toBe('Final Assembly & Equipment');
    expect(solar.sector!.chokepoint_note).toMatch(/Polysilicon/);
    expect(solar.alternatives.find((a) => a.value === 'no_stage')!.why_not).toBe('3 stages hold products existing trade cannot cover.');
    expect(solar.alternatives.map((a) => a.value)).toContain('Refining');
    expect(solar.alternatives.find((a) => a.value === 'Refining')!.why_not).toMatch(/97% of operational production capacity/);
    const grid = reason('sector_chain', { sid: 'SC-GRID', scenario: 'managed' }, decoded);
    expect(grid.conclusion.value).toBe('Refining');
    expect(grid.alternatives.find((a) => a.value === 'Assembly')!.why_not).toMatch(/Transformers: already as diversified/);
    expect(grid.alternatives.at(-1)!.value).toBe('no_stage');
  });
  it('walkthrough wording (2026-11-21): commodity counts, top ranks, possessives, deployment security line, one full stop', () => {
    // AUS battery minerals: six signals hold, but the two required ones are not counted, so the count is 4 of 5
    const aus = post('AUS', 'Batteries', 'Extraction');
    const ly = aus.steps.find((s) => s.id === 'large_yard')!;
    expect(ly.sentence).toMatch(/^4 commodity signals hold; the test needs 5/);
    expect(ly.drivers[0]).toBe('required signals that hold (all are needed; not counted): mature, passive');
    expect(ly.drivers[1]).toMatch(/^counted signals that hold \(4 of 5 needed\): /);
    expect(ly.drivers[1].split(', ')).toHaveLength(4);
    // AUS holds 16% of world supply: the self-supply guard zeroes the import-composition families, so only import
    // dependence can drive its need (supply_chain_need.R), and the composition ranks are marked as not scored
    const ausNeed = aus.steps.find((s) => s.id === 'need')!;
    expect(ausNeed.sentence).toMatch(/driven most by import dependence; its import mix is not scored/);
    expect(ausNeed.sentence).not.toMatch(/concentration of its own suppliers/);
    expect(ausNeed.drivers.find((d) => d.startsWith('concentration of its own suppliers'))).toMatch(/\(not scored: self-supply\)$/);
    expect(ausNeed.drivers.find((d) => d.startsWith('import dependence'))).not.toMatch(/not scored/);
    // need 0.998 never reads "above 100% of economies"
    const jpn = post('JPN', 'Electric Vehicles', 'Refining');
    expect(jpn.steps.find((s) => s.id === 'need')!.sentence).toMatch(/ranks above 99% of economies/);
    // a name ending in s takes a bare apostrophe; at Deployment the security line hands over to deployment need
    const usa = post('USA', 'Nuclear', 'Deployment');
    expect(usa.conclusion.summary).toMatch(/^United States' /);
    expect(usa.steps.find((s) => s.id === 'secure')!.sentence).toMatch(/so deployment need decides\.$/);
    const poly = reason('aib_node', { node: 'polysilicon', scenario: 'decoupling' }, decoded);
    expect(poly.conclusion.summary).not.toMatch(/\.\.$/);
    const bat = reason('compact', { sid: 'SC-BATTERIES', scenario: 'decoupling' }, decoded);
    expect(bat.conclusion.summary).toMatch(/, so the Explorer proposes: Potential Sectoral Compact\.$/);
    // the compact threat notes a reviewer reads after the reveal: plain words and country names, no field names
    const t = deriveCompactThreat(decoded.aib!.doc, 'SC-BATTERIES', 'decoupling')!;
    expect(t.notes.join(' ')).not.toMatch(/postInvestment|time_to_capacity_years|dependence_outcome|BUILD step/);
    // Slovenia is treated as allied since 2026-11-23 (config/aib_strategic_layer.yml treat_as_allied)
    expect(t.notes.join(' ')).toMatch(/suppliers outside the allied group: Mozambique\./);
    expect(t.intervention).toMatch(/new capacity in Japan; Argentina; /);
  });
  it('partnership: the four boxes, the coordination changes and the box-consistency check', () => {
    const r = reason('partnership', { pair: 'USA_JPN' }, decoded);
    expect(r.conclusion.label).toBe('Build together');
    expect(r.partnership!.memo_choices.map((m) => m.label)).toEqual(['Build together', 'Trade and standards', 'Institutions', 'Stay out']);
    expect(r.partnership!.coordination_changes.join(' ')).toMatch(/Rare-earth permanent magnets/);
    expect(r.steps.find((s) => s.id === 'consistency')!.sentence).toMatch(/Natural graphite/);
    wellFormed({ ...r, technical_path: '/x' });
  });
});
