// Tests of the TypeScript port of sp_choice_rule (qc/choiceRule.ts): every branch of the R rule on synthetic
// arguments, the inputs read from a shipped trace, the rule under moved thresholds (unknowns tried both ways),
// and the reproduction over the real shipped bundle (skipped when public/data is absent).

import { describe, expect, it } from 'vitest';
import {
  choiceRule, CONFIG_ONLY_DEFAULTS, cpHighUnder, extractChoiceInputs, largeUnder, reproduceShippedChoices, spChoiceRule, thresholdsFromCore,
  type ChoiceThresholds, type RuleArgs,
} from './choiceRule.ts';
import { HAVE_REAL, loadReal, mkDetail, mkPosture, triageTrace, TH } from './__tests__/fixtures.ts';

const th: ChoiceThresholds = { need: TH.need, prize: TH.prize, cp: TH.cp, deployment: TH.deployment, largeMin: 5, positionFirst: true, cpStrict: false };
const args = (o: Partial<RuleArgs>): RuleArgs => ({ stage: 'Refining', secure: false, need: 0.5, prize: 0.5, cpHigh: false, pathway: 'no', dn: null, large: false, capped: false, ...o });

describe('spChoiceRule: every branch of R sp_choice_rule (decision_engine.R:160-182)', () => {
  it('Secure first, at any stage and whatever the other inputs', () => {
    expect(spChoiceRule(args({ secure: true, need: null, prize: null }), th)).toBe('secure');
    expect(spChoiceRule(args({ secure: true, stage: 'Deployment', dn: 0.9 }), th)).toBe('secure');
  });
  it('Deployment: dn NA -> not_assessed, dn >= bar (inclusive) -> accelerate, else watch', () => {
    expect(spChoiceRule(args({ stage: 'Deployment', dn: null }), th)).toBe('not_assessed');
    expect(spChoiceRule(args({ stage: 'Deployment', dn: 0.6 }), th)).toBe('accelerate');
    expect(spChoiceRule(args({ stage: 'Deployment', dn: 0.59 }), th)).toBe('watch');
    // need / prize / position are not read at Deployment
    expect(spChoiceRule(args({ stage: 'Deployment', dn: 0.2, need: 0.99, prize: 0.99, cpHigh: true, large: true }), th)).toBe('watch');
  });
  it('a missing need or prize is not_assessed before the large-yard case', () => {
    expect(spChoiceRule(args({ need: null, large: true }), th)).toBe('not_assessed');
    expect(spChoiceRule(args({ prize: null, large: true }), th)).toBe('not_assessed');
  });
  it('the large-yard case gives watch unless position_first and a high position', () => {
    expect(spChoiceRule(args({ need: 0.9, prize: 0.9, large: true }), th)).toBe('watch');
    expect(spChoiceRule(args({ need: 0.9, prize: 0.9, large: true, cpHigh: true }), th)).toBe('preserve');
    expect(spChoiceRule(args({ need: 0.9, prize: 0.9, large: true, cpHigh: true }), { ...th, positionFirst: false })).toBe('watch');
  });
  it('triage: hp & ch -> preserve; hp & pathway -> catch_up; hp & hn -> partner; hp -> watch', () => {
    expect(spChoiceRule(args({ prize: 0.6, cpHigh: true }), th)).toBe('preserve');
    expect(spChoiceRule(args({ prize: 0.6, pathway: 'yes' }), th)).toBe('catch_up');
    expect(spChoiceRule(args({ prize: 0.6, need: 0.67 }), th)).toBe('partner');
    expect(spChoiceRule(args({ prize: 0.6, pathway: 'unknown' }), th)).toBe('watch');
  });
  it('triage: low prize: hn & ch -> preserve; hn -> partner; otherwise watch (a top-quarter position alone is watch)', () => {
    expect(spChoiceRule(args({ need: 0.8, cpHigh: true }), th)).toBe('preserve');
    expect(spChoiceRule(args({ need: 0.8, pathway: 'yes' }), th)).toBe('partner');
    expect(spChoiceRule(args({ need: 0.2, cpHigh: true, pathway: 'yes' }), th)).toBe('watch');
  });
  it('a prize capped as market-only is never high (whatever its value)', () => {
    expect(spChoiceRule(args({ prize: 0.95, capped: true, pathway: 'yes' }), th)).toBe('watch');
    expect(spChoiceRule(args({ prize: 0.95, capped: true, need: 0.9 }), th)).toBe('partner');
  });
});

describe('extractChoiceInputs: the rule inputs a shipped detail cell implies', () => {
  it('reads values, the trace words and the pathway', () => {
    const d = mkDetail({ choice: 'catch_up', need: 0.45, prize: 0.83, cp: 0.31, share: 0.01, pathway: 'yes', trace: triageTrace({ need: 0.45, prize: 0.83, cp: 0.31, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }) });
    const i = extractChoiceInputs(d);
    expect(i).toMatchObject({ need: 0.45, prize: 0.83, cp: 0.31, pathway: 'yes', large: false, largeN: 2, largeMin: 5, secure: false });
    expect(i.traced).toMatchObject({ need: 'low', prize: 'high', position: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' });
    expect(i.steps).toEqual(['secure_override', 'large_yard_case', 'supply_chain_need', 'strategic_industrial_value', 'cp_contender_pct', 'pathway_credible', 'choice']);
  });
  it('position words imply contender and capability-floor status', () => {
    const at = (pos: string, cp: number, borderline: string[] = [], share = 0.02) =>
      extractChoiceInputs(mkDetail({ choice: 'watch', need: 0.2, prize: 0.3, cp, share, borderline, trace: triageTrace({ need: 0.2, prize: 0.3, cp, pos, choice: 'watch' }) }));
    expect(at('high', 0.8)).toMatchObject({ contender: true, floorMet: true });
    expect(at('capability_floor_not_met', 0.8)).toMatchObject({ contender: true, floorMet: false });
    expect(at('not_a_contender', 0.8, [], 0.001)).toMatchObject({ contender: false });
    // within the band, tagged borderline on cp: R nb_cp requires a contender with the floor met
    expect(at('not_top_quarter', 0.72, ['cp_contender_pct'])).toMatchObject({ contender: true, floorMet: true });
    // within the band, a contender, not tagged: the floor must have failed
    expect(at('not_top_quarter', 0.72)).toMatchObject({ contender: true, floorMet: false });
    // outside the band: not implied
    expect(at('not_top_quarter', 0.4).floorMet).toBeNull();
  });
  it('Deployment reads deployment need, not need or prize', () => {
    const d = mkDetail({ stage: 'Deployment', choice: 'accelerate', need: 0.64, trace: [{ step: 'secure', test: 'secure_override', result: 'no' }, { step: 'need', test: 'deployment_need', value: 0.64, threshold: 0.6, result: 'high' }, { step: 'choice', test: 'choice', result: 'accelerate' }] });
    const i = extractChoiceInputs(d);
    expect(i).toMatchObject({ dn: 0.64, need: null, prize: null, large: undefined });
  });
});

describe('choiceRule on extracted inputs', () => {
  const cell = (cp: number, pos: string, share: number | null, borderline: string[] = []) =>
    extractChoiceInputs(mkDetail({ choice: 'partner', need: 0.8, prize: 0.3, cp, share, borderline, trace: triageTrace({ need: 0.8, prize: 0.3, cp, pos, choice: 'partner' }) }));
  it('reproduces the shipped choice from values', () => {
    expect(choiceRule(cell(0.5, 'not_top_quarter', 0.02), th).choice).toBe('partner');
  });
  it('a moved cp bar the contender / floor status cannot decide is unresolved, never guessed', () => {
    const i = cell(0.62, 'not_top_quarter', null);
    const out = choiceRule(i, { ...th, cp: 0.6 }, { mode: 'trace_at_base', base: th });
    expect(out.choice).toBeNull();
    expect(out.unresolved.join(' ')).toMatch(/cp_high/);
  });
  it('a value exactly on a moved threshold is unresolved (the bundle rounds to 3 digits)', () => {
    const i = extractChoiceInputs(mkDetail({ choice: 'watch', need: 0.62, prize: 0.3, cp: 0.1, share: 0.0, trace: triageTrace({ need: 0.62, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'watch' }) }));
    const out = choiceRule(i, { ...th, need: 0.62 }, { mode: 'trace_at_base', base: th });
    expect(out.choice).toBeNull();
    expect(out.unresolved[0]).toMatch(/supply_chain_need .*moved threshold/);
    // at the shipped threshold the trace word decides
    expect(choiceRule(i, th, { mode: 'trace_at_base', base: th }).choice).toBe('watch');
  });
  it('cpHighUnder: cp_strict at a shipped 0.750 drops a high position', () => {
    const i = cell(0.75, 'high', 0.05);
    expect(cpHighUnder(i, th, { mode: 'trace_at_base', base: th })).toBe(true);
    expect(cpHighUnder(i, { ...th, cpStrict: true }, { mode: 'trace_at_base', base: th })).toBe(false);
  });
});

describe('largeUnder: the large-yard case under another min_signals', () => {
  const ly = (result: string, n: number, borderline: string[] = []) =>
    extractChoiceInputs(mkDetail({ choice: 'watch', need: 0.2, prize: 0.3, cp: 0.1, borderline, trace: triageTrace({ need: 0.2, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', large: result, n, choice: 'watch' }) }));
  it('a case that holds keeps holding at a lower bar and needs the signals at a higher one', () => {
    expect(largeUnder(ly('yes', 5), 4)).toBe(true);
    expect(largeUnder(ly('yes', 5), 6)).toBe(false);
    expect(largeUnder(ly('yes', 6), 6)).toBe(true);
  });
  it('a failing case gains it at one signal fewer only when it failed for want of that signal (large_yard_near)', () => {
    expect(largeUnder(ly('no', 4, ['large_yard_case']), 4)).toBe(true);
    expect(largeUnder(ly('no', 4), 4)).toBe(false);
    expect(largeUnder(ly('no', 3), 4)).toBe(false);
    expect(largeUnder(ly('no', 6), 4)).toBe(false); // a required signal fails or a veto holds
    expect(largeUnder(ly('no', 4), 6)).toBe(false);
  });
  it('an undecided case never holds at a higher bar and is undecidable at a lower one with enough signals', () => {
    expect(largeUnder(ly('unknown', 4), 6)).toBe(false);
    expect(largeUnder(ly('unknown', 4), 4)).toBeNull();
    expect(largeUnder(ly('unknown', 2), 4)).toBe(false);
  });
});

describe('reproduceShippedChoices on synthetic cells', () => {
  it('lists every non-reproduced cell with a category and a reason', () => {
    const p = mkPosture([
      { iso3: 'AAA', tech: 'Batteries', stage: 'Refining', choice: 'catch_up', need: 0.45, prize: 0.83, cp: 0.31, share: 0.01, pathway: 'yes', trace: triageTrace({ need: 0.45, prize: 0.83, cp: 0.31, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }) },
      // the trace ends in partner but the core ships catch_up
      { iso3: 'AAA', tech: 'Solar', stage: 'Refining', choice: 'catch_up', need: 0.8, prize: 0.3, cp: 0.1, share: 0, pathway: 'no', trace: triageTrace({ need: 0.8, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'partner' }) },
      // the trace and the core say preserve but the values give partner (high need, low prize, low position)
      { iso3: 'AAA', tech: 'Wind', stage: 'Refining', choice: 'preserve', need: 0.8, prize: 0.3, cp: 0.1, share: 0.02, pathway: 'no', trace: triageTrace({ need: 0.8, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'preserve' }) },
      // a cell with no trace
      { iso3: 'AAA', tech: 'Coal', stage: 'Refining', choice: 'watch', need: 0.2, prize: 0.3, trace: [] },
    ]);
    const { rows, summary } = reproduceShippedChoices(p.core, p.details, thresholdsFromCore(p.core));
    expect(summary.cells).toBe(4);
    expect(summary.reproduced).toBe(1);
    const by = Object.fromEntries(rows.map((r) => [r.tech, r]));
    expect(by.Batteries.reproduced).toBe(true);
    expect(by.Solar.category).toBe('trace_mismatch');
    expect(by.Wind.category).toBe('rule_mismatch');
    expect(by.Wind.recomputed).toBe('partner');
    expect(by.Coal.category).toBe('no_detail');
    for (const r of rows.filter((x) => !x.reproduced)) expect(r.reason).toBeTruthy();
  });
  it('thresholdsFromCore carries the config-only defaults', () => {
    const p = mkPosture([{ choice: 'watch', need: 0.1, prize: 0.1 }]);
    expect(thresholdsFromCore(p.core)).toMatchObject({ need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, largeMin: CONFIG_ONLY_DEFAULTS.largeMin, positionFirst: true, cpStrict: false });
  });
});

describe.skipIf(!HAVE_REAL)('reproduction over the shipped bundle (214 economies x 66 cells)', () => {
  it('re-runs the rule on every cell; every exception carries a reason', () => {
    const { decoded } = loadReal();
    const p = decoded.posture!;
    const { rows, summary } = reproduceShippedChoices(p.core, p.details);
    expect(summary.cells).toBe(p.core.countries.length * 66);
    expect(rows.length).toBe(summary.cells);
    // main's bundle (0496e7ba) reproduces in full; a drop below 99.9% is a finding to explain, not noise
    expect(summary.rate).toBeGreaterThanOrEqual(0.999);
    for (const r of rows.filter((x) => !x.reproduced)) {
      expect(r.reason, `${r.iso3}|${r.tech}|${r.stage}`).toBeTruthy();
      expect(r.category).not.toBe('ok');
    }
    expect(Object.values(summary.by_path).reduce((a, b) => a + b.cells, 0)).toBe(summary.cells);
  });
});
