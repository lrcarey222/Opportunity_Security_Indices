// whyChoice (posture/whyChoice.ts): "Why this result?" read from the shipped decision trace. The traces below
// mirror real detail cells (JPN Batteries|Refining catch_up, JPN Batteries|Extraction partner, an ABW-style
// Electric Vehicles|Assembly partner with a high prize and no pathway, JPN Coal|Extraction secure, JPN Gas|Deployment
// accelerate); every number in the words is the trace's value and bar.

import { describe, expect, it } from 'vitest';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { shippedChoice, whyChoice } from '../whyChoice.ts';
import type { NodeDetail, TraceStep } from '../detail.ts';

const META = { thresholds: { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, borderline_band: 0.05 } };

type D = Parameters<typeof whyChoice>[0];
function cell(stage: NodeDetail['stage'], trace: TraceStep[], extra: Partial<D> = {}): D {
  return { stage, trace, reason: null, choicePossible: null, pivotal: null, notAssessedReason: null, posture: 'compete', mode: null, deployMode: null, ...extra };
}
const s = (test: string, result: string, value: TraceStep['value'] = null, threshold: TraceStep['threshold'] = null): TraceStep => ({ step: test, test, value, threshold, result });

describe('whyChoice', () => {
  it('Catch Up (JPN Batteries|Refining): high value and a credible pathway, position below the top quarter; why not Preserve', () => {
    const d = cell('Refining', [
      s('secure_override', 'no'), s('large_yard_case', 'no', 0, 5), s('supply_chain_need', 'high', 0.939, 0.67), s('strategic_industrial_value', 'high', 0.777, 0.6),
      s('cp_contender_pct', 'not_top_quarter', 0.188, 0.75), s('pathway_credible', 'yes', 'direct_supply_share;feasibility'), s('choice', 'catch_up'),
    ], { mode: 'catch_up' });
    const w = whyChoice(d, META);
    expect(w.choice).toBe('catch_up');
    expect(w.because).toBe(
      'Strategic value is high (value 0.78, bar 0.60) and there is a credible domestic pathway (capability anchor), but the position is below the top quarter of producers (position 0.19, bar 0.75), so the default is to build from that base.',
    );
    expect(w.whyNot).toEqual({ alt: 'Preserve', text: 'The position is below the top quarter of producers (position 0.19, bar 0.75).' });
  });

  it('Partner with a high value and no pathway: why not Catch Up is the missing pathway', () => {
    const d = cell('Assembly', [
      s('secure_override', 'no'), s('large_yard_case', 'no', 1, 5), s('supply_chain_need', 'high', 0.81, 0.67), s('strategic_industrial_value', 'high', 0.66, 0.6),
      s('cp_contender_pct', 'not_top_quarter', 0.3, 0.75), s('pathway_credible', 'no'), s('choice', 'partner'),
    ], { mode: 'partner' });
    const w = whyChoice(d, META);
    expect(w.because).toBe(
      'Supply risk is high (need 0.81, bar 0.67) but the position is below the top quarter of producers (position 0.30, bar 0.75) and there is no credible domestic pathway, so building at home is not the default.',
    );
    expect(w.whyNot).toEqual({ alt: 'Catch Up', text: 'No credible domestic pathway (capability anchor) was found.' });
  });

  it('Partner with a low value (JPN Batteries|Extraction): the close call the evidence allows comes first', () => {
    const trace = [
      s('secure_override', 'no'), s('large_yard_case', 'no', 4, 5), s('supply_chain_need', 'high', 0.993, 0.67), s('strategic_industrial_value', 'low', 0.571, 0.6),
      s('cp_contender_pct', 'not_top_quarter', 0, 0.75), s('choice', 'partner'),
    ];
    const w = whyChoice(cell('Extraction', trace, { mode: 'partner', choicePossible: 'partner|watch', pivotal: 'large_yard_case' }), META);
    expect(w.because).toBe('Supply risk is high (need 0.99, bar 0.67) but strategic value is below the bar (value 0.57, bar 0.60), so reliable partners come before building at home.');
    expect(w.whyNot).toEqual({ alt: 'Watch', text: 'Close call: the open-market test (4 signals, bar 5) is near the bar.' });
    // without a close call, the rule's own alternative
    expect(whyChoice(cell('Extraction', trace, { mode: 'partner' }), META).whyNot).toEqual({ alt: 'Catch Up', text: 'Building at home needs strategic value at or above the bar (value 0.57, bar 0.60).' });
  });

  it('Secure and Accelerate read their own steps; Deployment uses the deployment bar', () => {
    const sec = whyChoice(cell('Extraction', [s('secure_override', 'yes', 'systemic_resilience'), s('choice', 'secure')], { posture: 'secure' }), META);
    expect(sec).toEqual({ choice: 'secure', because: 'An acute vulnerability was found (system resilience), so Secure applies whatever the economic prize.' });
    const acc = whyChoice(cell('Deployment', [s('secure_override', 'no'), s('deployment_need', 'high', 0.627, 0.6), s('choice', 'accelerate')], { posture: 'deploy', deployMode: 'accelerate' }), META);
    expect(acc.because).toBe('Deployment need is at or above the bar (need 0.63, bar 0.60): the energy system needs more of it. This says nothing about where the equipment is made.');
    expect(acc.whyNot).toBeUndefined();
  });

  it('not assessed uses the shipped reason; a trace that does not reproduce the choice falls back to the engine sentence, with no why-not', () => {
    const na = whyChoice(
      cell('Recovery', [s('secure_override', 'no'), s('large_yard_case', 'unknown', 0, 5), s('supply_chain_need', 'not_assessed', null, 0.67), s('strategic_industrial_value', 'low', 0.009, 0.6), s('choice', 'not_assessed')], {
        posture: 'not_assessed', notAssessedReason: 'Supply-chain need is not measured (too few comparable importers or no trade data): not assessed.',
      }),
      META,
    );
    expect(na.because).toMatch(/^Supply-chain need is not measured/);
    const odd = whyChoice(
      cell('Assembly', [s('secure_override', 'no'), s('supply_chain_need', 'low', 0.2, 0.67), s('strategic_industrial_value', 'low', 0.2, 0.6), s('cp_contender_pct', 'not_top_quarter', 0.1, 0.75), s('choice', 'preserve')], {
        mode: 'preserve', reason: 'a reason the engine wrote.',
      }),
      META,
    );
    expect(odd).toEqual({ choice: 'preserve', because: 'A reason the engine wrote.' });
    expect(shippedChoice(cell('Assembly', [], { mode: 'partner' }))).toBe('partner');
  });

  it('the words avoid the words the copy guide rules out', () => {
    const d = cell('Refining', [s('secure_override', 'no'), s('supply_chain_need', 'low', 0.3, 0.67), s('strategic_industrial_value', 'low', 0.2, 0.6), s('cp_contender_pct', 'unknown'), s('choice', 'watch')]);
    const w = whyChoice(d, META);
    expect(w.because).toBe('Both supply risk (need 0.30, bar 0.67) and strategic value (value 0.20, bar 0.60) are below their bars, so it is left open.');
    expect(`${w.because} ${w.whyNot?.text}`).not.toMatch(AVOID_RE);
  });
});
