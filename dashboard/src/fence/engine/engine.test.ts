import { describe, expect, it } from 'vitest';
import {
  classify,
  emptyInputs,
  evaluatePolicyFit,
  evaluateSmallYard,
  evaluateStack,
  explainOf,
  explainTransition,
  INPUT_DEFS,
  stackOutcome,
  withValue,
} from './index.ts';
import type { Assessment, AssessmentContext, InputId, Inputs, Provenance, Strategy } from './index.ts';

type Vals = Partial<Record<InputId, string | number>>;

function make(vals: Vals, ctx: Partial<AssessmentContext> = {}, strategies: Strategy[] = [], prov: Provenance = 'judgment'): Assessment {
  const inputs = emptyInputs() as unknown as Record<InputId, { value: unknown; provenance: Provenance }>;
  for (const [k, v] of Object.entries(vals)) inputs[k as InputId] = { value: v, provenance: prov };
  return {
    context: { country: 'USA', technology: 'Test', stage: 'Midstream', componentLabel: 'Test component', layer: 'hardware', ...ctx },
    inputs: inputs as unknown as Inputs,
    strategies,
  };
}

// A complete, passive, non-acute manufactured good with a strong learning case.
const batteryCells: Vals = {
  consequence: 'moderate',
  timeHorizon: 'years',
  digitallyActive: 'no',
  substitutability: 'moderate',
  qualificationTime: 'm3to12',
  reconstitutionTime: 'y1to3',
  stockpile: 'lt3m',
  surgeCapacity: 'partial',
  nonAlliedTopShare: 0.6,
  hhi: 0.38,
  homeShare: 0.06,
  processKnowledge: 'yes',
  workforceSuppliers: 'yes',
  adjacentInnovation: 'yes',
  complementaryCapabilities: 'partly',
  premium: 20,
};

describe('Case A: concentration alone does not make a small yard', () => {
  it('very high supplier concentration with a moderate consequence is not Small Yard', () => {
    const a = make({ ...batteryCells, nonAlliedTopShare: 0.95, hhi: 0.9, substitutability: 'impossible', reconstitutionTime: 'gt3y' });
    const r = classify(a);
    expect(r.yard).not.toBe('small');
    expect(r.gates.small.acute.state).toBe('no');
    expect(r.reasons.some((x) => x.kind === 'not-by' && /concentration/i.test(x.text))).toBe(true);
  });
  it('concentration with easy substitution is not a physical chokepoint even when the consequence is severe', () => {
    const a = make({ ...batteryCells, consequence: 'severe', timeHorizon: 'weeks', substitutability: 'easy', nonAlliedTopShare: 0.9 });
    expect(evaluateSmallYard(a).physical.state).toBe('no');
    expect(classify(a).yard).not.toBe('small');
  });
});

describe('Case B: an acute digital control vulnerability fences the control layer', () => {
  const firmware: Vals = {
    consequence: 'severe',
    timeHorizon: 'days',
    digitallyActive: 'yes',
    remoteControl: 'yes',
    separability: 'separable',
  };
  it('produces Small Yard scoped to the control layer', () => {
    const r = classify(make(firmware, { layer: 'control' }));
    expect(r.yard).toBe('small');
    expect(r.scope).toBe('control-layer');
    expect(r.variant).toBe('small-control');
    expect(r.recommendedPosture).toMatch(/control layer/i);
    expect(r.path.find((n) => n.id === 'digital')?.answer).toBe('yes');
  });
  it('widens the fence when the control layer cannot be separated', () => {
    const r = classify(make({ ...firmware, separability: 'inseparable' }, { layer: 'control' }));
    expect(r.yard).toBe('small');
    expect(r.scope).toBe('whole-product');
  });
  it('flags an unknown boundary without hiding the vulnerability', () => {
    const r = classify(make({ ...firmware, separability: 'unknown' }, { layer: 'control' }));
    expect(r.yard).toBe('small');
    expect(r.scope).toBe('unclear');
    expect(r.missingEvidence.some((m) => m.input === 'separability')).toBe(true);
  });
});

describe('Case C: a physical chokepoint', () => {
  const magnets: Vals = {
    consequence: 'severe',
    timeHorizon: 'months',
    digitallyActive: 'no',
    nonAlliedTopShare: 0.84,
    substitutability: 'hard',
    qualificationTime: 'y1to3',
    reconstitutionTime: 'gt3y',
    stockpile: 'lt3m',
    surgeCapacity: 'none',
  };
  it('concentration + low substitutability + slow reconstitution + severe consequence is Small Yard', () => {
    const r = classify(make(magnets, { layer: 'material' }));
    expect(r.yard).toBe('small');
    expect(r.scope).toBe('input');
    const drivers = r.reasons.filter((x) => x.kind === 'driver').map((x) => x.text).join(' ');
    expect(drivers).toMatch(/84%/);
    expect(r.reasons.some((x) => x.kind === 'not-by' && /import share alone/i.test(x.text))).toBe(true);
  });
  it('a long stockpile and covering surge capacity remove the leverage', () => {
    const r = classify(make({ ...magnets, stockpile: 'gt12m', surgeCapacity: 'covers', qualificationTime: 'm3to12' }, { layer: 'material' }));
    expect(r.gates.small.physical.state).toBe('no');
  });
});

describe('Case D: medium yard', () => {
  it('no acute vulnerability + credible competitiveness + meaningful learning is Medium Yard', () => {
    const r = classify(make(batteryCells, {}, ['preserve', 'leapAhead']));
    expect(r.yard).toBe('medium');
    expect(r.gates.medium.learning.level).toBe('strong');
    expect(r.recommendedPosture).toMatch(/selectively open/i);
    expect(r.reasons.some((x) => x.kind === 'not-by' && /Economic Opportunity score/.test(x.text))).toBe(true);
  });
});

describe('Case E: large yard', () => {
  it('no security rationale + weak learning + high protection cost is Large Yard', () => {
    const r = classify(
      make({
        ...batteryCells,
        nonAlliedTopShare: 0.5,
        homeShare: 0.01,
        processKnowledge: 'no',
        workforceSuppliers: 'partly',
        adjacentInnovation: 'no',
        complementaryCapabilities: 'no',
        premium: 45,
      }),
    );
    expect(r.yard).toBe('large');
  });
  it('a moderate capability case with a high premium becomes a large yard with a capability floor', () => {
    const r = classify(make({ ...batteryCells, homeShare: 0.01, processKnowledge: 'partly', workforceSuppliers: 'partly', adjacentInnovation: 'no', premium: 50 }));
    expect(r.yard).toBe('large');
    expect(r.variant).toBe('large-floor');
  });
});

describe('Case F: unknown critical evidence lowers confidence; it is never filled', () => {
  it('an unknown consequence leaves the result indeterminate, with the gap marked pivotal', () => {
    const a = make({ ...batteryCells, consequence: 'unknown', substitutability: 'hard', reconstitutionTime: 'gt3y', surgeCapacity: 'none' });
    const r = classify(a);
    expect(r.yard).toBe('indeterminate');
    expect(r.confidence).toBe('low');
    expect(r.possible).toContain('small');
    const gap = r.missingEvidence.find((m) => m.input === 'consequence');
    expect(gap?.pivotal).toBe(true);
    // the input itself is still unknown: nothing was imputed
    expect(a.inputs.consequence.value).toBe('unknown');
  });
  it('unknown qualification time is pivotal when it decides whether a chokepoint is acute', () => {
    const r = classify(
      make({
        consequence: 'severe',
        timeHorizon: 'months',
        digitallyActive: 'no',
        nonAlliedTopShare: 0.8,
        substitutability: 'hard',
        qualificationTime: 'unknown',
        reconstitutionTime: 'gt3y',
        stockpile: 'm3to12',
        surgeCapacity: 'covers',
        ...{ processKnowledge: 'yes', workforceSuppliers: 'yes', adjacentInnovation: 'yes', homeShare: 0.05, complementaryCapabilities: 'yes', premium: 10 },
      }),
    );
    expect(r.yard).toBe('indeterminate');
    expect(r.missingEvidence[0].input).toBe('qualificationTime');
    expect(r.missingEvidence[0].pivotal).toBe(true);
  });
  it('the same assessment with every input known has higher confidence and completeness', () => {
    const known = classify(make(batteryCells));
    const partial = classify(make({ ...batteryCells, workforceSuppliers: 'unknown', stockpile: 'unknown' }));
    expect(partial.evidenceCompleteness).toBeLessThan(known.evidenceCompleteness);
    expect(partial.yard).toBe(known.yard);
  });
});

describe('Case G: a mixed system returns placements by component', () => {
  it('battery storage splits into control (small), cells (medium) and enclosure (large)', () => {
    const s = evaluateStack([
      {
        componentId: 'bms',
        label: 'BMS firmware',
        assessment: make({ consequence: 'high', timeHorizon: 'days', digitallyActive: 'yes', remoteControl: 'yes', separability: 'partial' }, { layer: 'control' }),
      },
      { componentId: 'cells', label: 'Cells', assessment: make(batteryCells, {}, ['preserve']) },
      {
        componentId: 'enclosure',
        label: 'Enclosure',
        assessment: make(
          {
            consequence: 'low',
            timeHorizon: 'years',
            digitallyActive: 'no',
            substitutability: 'easy',
            qualificationTime: 'lt3m',
            reconstitutionTime: 'lt6m',
            stockpile: 'lt3m',
            surgeCapacity: 'covers',
            nonAlliedTopShare: 0.3,
            homeShare: 0.02,
            processKnowledge: 'no',
            workforceSuppliers: 'no',
            adjacentInnovation: 'no',
            complementaryCapabilities: 'partly',
            premium: 25,
          },
          { layer: 'structure' },
        ),
      },
    ]);
    expect(s.outcome).toBe('mixed');
    expect(s.byYard.small.map((l) => l.componentId)).toEqual(['bms']);
    expect(s.byYard.medium.map((l) => l.componentId)).toEqual(['cells']);
    expect(s.byYard.large.map((l) => l.componentId)).toEqual(['enclosure']);
  });
});

describe('Case H: the policy sandbox flags over- and under-fencing', () => {
  const firmware = classify(
    make({ consequence: 'severe', timeHorizon: 'days', digitallyActive: 'yes', remoteControl: 'yes', separability: 'separable' }, { layer: 'control' }),
  );
  it('restricting the whole hardware stack for a control-layer vulnerability is over-fencing', () => {
    const fit = evaluatePolicyFit(firmware, { levels: { trustedSource: 3, tariff: 3 }, securityScope: 'whole-product' });
    expect(fit.findings.some((f) => f.kind === 'over-fencing')).toBe(true);
  });
  it('a narrow trusted-source and cyber package fits the control-layer diagnosis', () => {
    const fit = evaluatePolicyFit(firmware, { levels: { trustedSource: 3, cyberStandard: 3 }, securityScope: 'control-layer' });
    expect(fit.findings.map((f) => f.kind)).toEqual(['aligned']);
  });
  it('only R&D support for an acute chokepoint is under-fencing', () => {
    const chokepoint = classify(
      make(
        {
          consequence: 'severe',
          timeHorizon: 'months',
          digitallyActive: 'no',
          nonAlliedTopShare: 0.85,
          substitutability: 'impossible',
          qualificationTime: 'y1to3',
          reconstitutionTime: 'gt3y',
          stockpile: 'none',
          surgeCapacity: 'none',
        },
        { layer: 'material' },
      ),
    );
    expect(chokepoint.yard).toBe('small');
    const fit = evaluatePolicyFit(chokepoint, { levels: { rnd: 3 }, securityScope: 'control-layer' });
    const kinds = fit.findings.map((f) => f.kind);
    expect(kinds).toContain('under-fencing');
    expect(kinds).toContain('remediation-gap');
  });
  it('import restrictions without capability building are a mismatch for a medium yard', () => {
    const med = classify(make(batteryCells, {}, ['catchUp']));
    const fit = evaluatePolicyFit(med, { levels: { tariff: 3, procurement: 2, investmentRestriction: 2 }, securityScope: 'whole-product' });
    const kinds = fit.findings.map((f) => f.kind);
    expect(kinds).toContain('mismatch');
    expect(kinds).toContain('closed-gate');
    expect(kinds).toContain('misapplied');
  });
});

describe('review regressions', () => {
  const both: Vals = {
    consequence: 'severe', timeHorizon: 'weeks', digitallyActive: 'yes', remoteControl: 'yes', separability: 'separable',
    nonAlliedTopShare: 0.8, substitutability: 'hard', qualificationTime: 'y1to3', reconstitutionTime: 'gt3y', surgeCapacity: 'none', stockpile: 'none',
  };
  it('a control layer and a chokepoint together keep both problems in the posture and the fence fit', () => {
    const r = classify(make(both));
    expect(r.variant).toBe('small-both');
    expect(r.recommendedPosture).toMatch(/control layer and remediate the chokepoint/);
    const cyberOnly = evaluatePolicyFit(r, { levels: { trustedSource: 3, cyberStandard: 3 }, securityScope: 'control-layer' });
    expect(cyberOnly.findings.map((f) => f.kind)).toContain('under-fencing');
    const full = evaluatePolicyFit(r, { levels: { trustedSource: 3, cyberStandard: 3, stockpile: 3, publicFinance: 3 }, securityScope: 'control-layer' });
    expect(full.findings.map((f) => f.kind)).not.toContain('over-fencing');
  });
  it('completeness ignores questions that can no longer matter', () => {
    const settled = classify(make({ consequence: 'severe', timeHorizon: 'days', digitallyActive: 'yes', remoteControl: 'yes', separability: 'separable' }, { layer: 'control' }, ['leapAhead']));
    expect(settled.evidenceCompleteness).toBe(1);
    const noSmall = classify(make({ ...batteryCells, consequence: 'low', substitutability: 'unknown', qualificationTime: 'unknown', reconstitutionTime: 'unknown', surgeCapacity: 'unknown' }));
    expect(noSmall.yard).toBe('medium');
    expect(noSmall.missingEvidence.map((m) => m.input)).not.toContain('qualificationTime');
  });
  it('the fence fit flags a fence narrower than the diagnosis and tools it does not call for', () => {
    const whole = classify(make({ consequence: 'severe', timeHorizon: 'days', digitallyActive: 'yes', remoteControl: 'yes', separability: 'inseparable' }, { layer: 'control' }));
    const narrow = evaluatePolicyFit(whole, { levels: { trustedSource: 3, cyberStandard: 3 }, securityScope: 'control-layer' });
    expect(narrow.findings.some((f) => f.title === 'Fence too narrow')).toBe(true);
    const undecided = classify(make({ ...batteryCells, consequence: 'unknown', substitutability: 'hard', reconstitutionTime: 'gt3y', surgeCapacity: 'none' }));
    const tariff = evaluatePolicyFit(undecided, { levels: { tariff: 3, performanceSunset: 1 }, securityScope: 'control-layer' });
    expect(tariff.findings.some((f) => f.title === 'Not indicated by the diagnosis')).toBe(true);
    expect(tariff.findings.some((f) => f.kind === 'aligned')).toBe(false);
  });
  it('a system is mixed when no single yard is possible for every layer, even with unknowns', () => {
    const r = (possible: ('small' | 'medium' | 'large')[]) => ({ possible });
    expect(stackOutcome([r(['small']), r(['small']), r(['medium', 'large'])])).toBe('mixed');
    expect(stackOutcome([r(['small']), r(['small', 'medium'])])).toBe('indeterminate');
    expect(stackOutcome([r(['large']), r(['large'])])).toBe('large');
  });
  it('an assumed share is described as assumed, not as the data supplier’s observed share', () => {
    const a = make({ ...both, digitallyActive: 'no' }, { layer: 'material' });
    a.inputs.nonAlliedTopShare = { value: 0.62, provenance: 'scenario' };
    expect(classify(a).gates.small.physicalParts.coercion.detail).toMatch(/assumed to hold 62% of world supply/);
  });
  it('two indeterminate results are not reported as a change', () => {
    const before = make({ ...batteryCells, consequence: 'unknown', substitutability: 'hard', reconstitutionTime: 'gt3y', surgeCapacity: 'none', nonAlliedTopShare: 0.6 });
    const after = withValue(before, 'nonAlliedTopShare', 0.7);
    const t = explainTransition(before, classify(before), after, classify(after));
    expect(classify(before).yard).toBe('indeterminate');
    expect(t.changed).toBe(false);
  });
});

describe('scenario transitions and change conditions', () => {
  it('explains a Medium to Large move when learning is assumed low and the premium high', () => {
    const before = make({ ...batteryCells, homeShare: 0.01 });
    const after = withValue(withValue(withValue(withValue(before, 'processKnowledge', 'no'), 'workforceSuppliers', 'partly'), 'adjacentInnovation', 'no'), 'premium', 50);
    const rb = classify(before);
    const ra = classify(after);
    expect(rb.yard).toBe('medium');
    expect(ra.yard).toBe('large');
    const t = explainTransition(before, rb, after, ra);
    expect(t.changed).toBe(true);
    expect(t.headline).toBe('Medium Yard → Large Yard');
    expect(t.explanation).toMatch(/industrial-capability rationale weakens/);
    expect(t.changedInputs).toContain('premium');
  });
  it('lists what would move a medium yard, including the generic paper conditions', () => {
    const r = classify(make(batteryCells));
    expect(r.changeConditions.some((c) => c.toward === 'large')).toBe(true);
    expect(r.changeConditions.some((c) => c.toward === 'small')).toBe(true);
    expect(r.changeConditions.some((c) => c.generic)).toBe(true);
  });
  it('is deterministic', () => {
    expect(JSON.stringify(classify(make(batteryCells)))).toBe(JSON.stringify(classify(make(batteryCells))));
  });
});

describe('plain-language explanations', () => {
  it('every assumption has one, and none just restates its label', () => {
    for (const id of Object.keys(INPUT_DEFS) as InputId[]) {
      const text = explainOf(id);
      expect(text, id).toBeTruthy();
      expect(text.length, id).toBeGreaterThan(60);
      expect(text.toLowerCase().trim(), id).not.toBe(INPUT_DEFS[id].label.toLowerCase());
    }
  });
});
