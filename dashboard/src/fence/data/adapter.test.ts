import { describe, expect, it } from 'vitest';
import { classify, UNKNOWN } from '../engine/index.ts';
import { SYN_BUNDLE, SYN_REFERENCE } from '../__fixtures__/synthetic.ts';
import { decodeState, encodeState, initialState, stateSearch } from '../state/assessment.ts';
import { buildAssessment, componentById, componentSubject, dataInputs, evidenceFor, stageSubject } from './adapter.ts';

const comp = (id: string) => componentSubject(componentById(SYN_REFERENCE, id)!);
const empty = { answers: {}, scenario: {}, strategies: [] };
const opts = { country: 'United States', withScenario: true };

describe('evidence adapter', () => {
  it('feeds the engine from the component’s own scope, on the measure the metric map prefers', () => {
    const s = comp('syn-cells');
    const ev = evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE);
    expect(ev.primary?.scopeId).toBe('testtech-midstream::Cells');
    // enginePriority.subcomponent is a bare string in the fixture: still read as a list
    expect(ev.engineMeasure).toBe('sub_exports');
    const { values } = dataInputs(ev);
    expect(values.nonAlliedTopShare?.value).toBe(0.6);
    expect(values.homeShare?.value).toBe(0.06);
    expect(values.nonAlliedTopShare?.measure).toBe('world exports of this sub-component (2024)');
    // TRL is the stage's global value, labelled as stage-level
    expect(values.trl?.note).toMatch(/stage-level/);
  });

  it('never borrows technology-stage shares for a component with no measured scope', () => {
    const s = comp('syn-control');
    const a = buildAssessment(s, empty, evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE), opts);
    expect(a.inputs.nonAlliedTopShare.value).toBe(UNKNOWN);
    expect(a.inputs.homeShare.value).toBe(UNKNOWN);
  });

  it('keeps provenance: data, curated assessment, your judgment, scenario, unknown', () => {
    const s = comp('syn-cells');
    const ev = evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE);
    const a = buildAssessment(s, { answers: { consequence: 'high', stockpile: 'unknown' }, scenario: { premium: 40 }, strategies: [] }, ev, opts);
    expect(a.inputs.hhi.provenance).toBe('data');
    expect(a.inputs.substitutability.provenance).toBe('assessment');
    expect(a.inputs.substitutability.assessmentConfidence).toBe('moderate');
    expect(a.inputs.consequence).toMatchObject({ value: 'high', provenance: 'judgment' });
    expect(a.inputs.stockpile).toMatchObject({ value: UNKNOWN, provenance: 'unknown' });
    expect(a.inputs.premium).toMatchObject({ value: 40, provenance: 'scenario' });
    // without the scenario the premium is simply unknown: the observed baseline is untouched
    const base = buildAssessment(s, { answers: {}, scenario: { premium: 40 }, strategies: [] }, ev, { ...opts, withScenario: false });
    expect(base.inputs.premium.value).toBe(UNKNOWN);
  });

  it('marks a curated share override as an assessment, not data', () => {
    const s = comp('syn-box');
    const a = buildAssessment(s, empty, evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE), opts);
    expect(a.inputs.nonAlliedTopShare).toMatchObject({ value: 0.9, provenance: 'assessment', subject: 'Curatoria' });
  });

  it('a reader’s value does not inherit the data’s supplier, measure or year', () => {
    const s = comp('syn-cells');
    const a = buildAssessment(s, { answers: { nonAlliedTopShare: 0.7 }, scenario: {}, strategies: [] }, evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE), opts);
    expect(a.inputs.nonAlliedTopShare).toEqual({ value: 0.7, provenance: 'judgment', note: undefined });
  });

  it('the home share falls back to the next measure when the preferred source does not list the country', () => {
    const s = stageSubject('Testtech', 'Midstream');
    const ev = evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE);
    expect(ev.engineMeasure).toBe('mfg_capacity');
    expect(ev.homeMeasure).toBe('exports');
    expect(dataInputs(ev).values.homeShare?.value).toBe(0.08);
    expect(dataInputs(ev).values.nonAlliedTopShare?.value).toBe(0.7);
  });

  it('whole-stage answers belong to their own technology-stage', () => {
    expect(stageSubject('Testtech', 'Midstream').key).not.toBe(stageSubject('Othertech', 'Midstream').key);
    expect(stageSubject('Testtech', 'Midstream').key).not.toBe(stageSubject('Testtech', 'Upstream').key);
  });

  it('a whole-stage assessment starts with every judgment unknown', () => {
    const s = stageSubject('Testtech', 'Midstream');
    const a = buildAssessment(s, empty, evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE), opts);
    expect(a.inputs.consequence.value).toBe(UNKNOWN);
    expect(a.inputs.homeShare.value).toBe(0.08);
    expect(classify(a).yard).toBe('indeterminate');
  });

  it('classifies the synthetic layers into different yards', () => {
    const y = (id: string) => {
      const s = comp(id);
      return classify(buildAssessment(s, { ...empty, answers: { premium: 15 } }, evidenceFor(s, SYN_BUNDLE, SYN_REFERENCE), opts)).yard;
    };
    expect(y('syn-control')).toBe('small');
    expect(y('syn-cells')).toBe('medium');
    expect(y('syn-magnet')).toBe('small');
  });
});

describe('assessment URL state', () => {
  it('round-trips an assessment through the `a` parameter', () => {
    const s = {
      ...initialState(),
      sel: { tech: 'Batteries', stage: 'Midstream' as const, stack: 'battery-storage' },
      focus: 'bess-cells',
      mode: 'quick' as const,
      step: 4,
      example: 'battery-storage',
      layers: {
        'bess-cells': {
          answers: { premium: 15, consequence: 'moderate', stockpile: 'unknown' },
          scenario: { nonAlliedTopShare: 0.45 },
          strategies: ['preserve' as const, 'leapAhead' as const],
          why: 'learning' as const,
          policy: { levels: { tariff: 2 as const, rnd: 3 as const }, scope: 'whole-product' as const },
        },
      },
    };
    expect(decodeState(encodeState(s))).toEqual(s);
    expect(stateSearch(s)).toMatch(/^\?a=[A-Za-z0-9_-]+$/);
  });

  it('drops anything malformed or unknown instead of guessing', () => {
    expect(decodeState('not base64 json')).toEqual(initialState());
    const bad = btoa(JSON.stringify({ v: 1, c: 'USA', t: 'X', s: 'Sideways', L: { k: { a: { consequence: 'apocalyptic', premium: 'lots', bogus: 1, timeHorizon: 'days' } } } }))
      .replace(/\+/g, '-')
      .replace(/\//g, '_')
      .replace(/=+$/, '');
    const s = decodeState(bad);
    expect(s.sel).toBeNull();
    expect(s.layers.k.answers).toEqual({ timeHorizon: 'days' });
  });

  it('drops numbers outside an input’s range', () => {
    const enc = (o: unknown) => btoa(JSON.stringify(o)).replace(/[+]/g, '-').replace(/[/]/g, '_').replace(/=+$/, '');
    const s = decodeState(enc({ v: 1, c: 'USA', L: { k: { a: { nonAlliedTopShare: 7, homeShare: 3, premium: -50, hhi: 0.2 } } } }));
    expect(s.layers.k.answers).toEqual({ hhi: 0.2 });
  });
});
