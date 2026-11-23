// The blind-first case review state machine (review/flow.ts; design 5 step 4): no reveal before a stored
// blind answer by any action, the blind answer locked once stored, verdict 3-6 needs the issue, the
// don't-know path, extras only for flagged cases, the payload and draft restore.

import { describe, expect, it } from 'vitest';
import { DONT_KNOW } from './model.ts';
import {
  FLOW_STEPS,
  canEnter,
  canReveal,
  flowReducer,
  initialFlowState,
  restoreFlowState,
  stepsFor,
  toDecisionUsePayload,
  toSubmissionPayload,
  toThreatPayload,
  type CaseFlowState,
  type FlowAction,
} from './flow.ts';

const CASE = { case_id: 'CASE_001', snapshot_sha256: 'a'.repeat(64) };
const run = (s: CaseFlowState, ...as: FlowAction[]) => as.reduce(flowReducer, s);
const T0 = '2027-01-20T09:00:00.000Z';
const T1 = '2027-01-20T09:04:00.000Z';
const T2 = '2027-01-20T09:10:30.000Z';

/** A state at the blind step with a complete answer, not yet submitted. */
function answered(choice = 'secure', flags: { decision_use?: boolean; threat?: boolean } = {}) {
  return run(initialFlowState({ case_id: 'CASE_001', ...flags }), { type: 'start', at: T0 }, { type: 'next' }, { type: 'set_blind', patch: { choice, confidence: 4, factors: ['import_dependence'] } });
}

describe('blind-first sequence', () => {
  it('starts at context and moves to blind', () => {
    const s0 = initialFlowState({ case_id: 'CASE_001' });
    expect(s0.step).toBe('context');
    expect(canReveal(s0)).toBe(false);
    expect(run(s0, { type: 'next' }).step).toBe('blind');
  });

  it('no action reaches the reveal (or any later step) before the blind answer is stored', () => {
    const atBlind = answered();
    const attempts: FlowAction[] = [
      { type: 'next' },
      { type: 'goto', step: 'reveal' },
      { type: 'goto', step: 'verdict' },
      { type: 'goto', step: 'done' },
      { type: 'set_verdict', verdict: 1 },
      { type: 'set_issue', patch: { issue_text: 'x', issue_type: 'data' } },
    ];
    for (const a of attempts) {
      const s = flowReducer(atBlind, a);
      expect(['context', 'blind']).toContain(s.step);
      expect(s.reveal.verdict).toBeNull();
      expect(s.reveal.revealed_at).toBeNull();
    }
    for (const step of FLOW_STEPS.slice(2)) expect(canEnter(atBlind, step)).toBe(false);
    // a fresh state cannot jump either
    const s0 = initialFlowState({ case_id: 'CASE_001' });
    expect(flowReducer(s0, { type: 'goto', step: 'reveal' }).step).toBe('context');
  });

  it('cannot submit an empty blind answer, or one without a driving factor', () => {
    const s = run(initialFlowState({ case_id: 'CASE_001' }), { type: 'next' });
    expect(flowReducer(s, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const noFactor = flowReducer(s, { type: 'set_blind', patch: { choice: 'watch' } });
    expect(flowReducer(noFactor, { type: 'submit_blind', at: T1 }).step).toBe('blind');
  });

  it('cannot submit the blind answer from another step', () => {
    const s = run(initialFlowState({ case_id: 'CASE_001' }), { type: 'set_blind', patch: { choice: 'watch', factors: ['market'] } });
    expect(s.step).toBe('context');
    expect(flowReducer(s, { type: 'submit_blind', at: T1 })).toBe(s);
  });

  it('reveals once the blind answer is stored, and locks it', () => {
    const s = run(answered('secure'), { type: 'submit_blind', at: T1 });
    expect(s.step).toBe('reveal');
    expect(canReveal(s)).toBe(true);
    expect(s.blind.answered_at).toBe(T1);
    expect(s.reveal.revealed_at).toBe(T1);
    const edited = flowReducer(s, { type: 'set_blind', patch: { choice: 'watch' } });
    expect(edited.blind.choice).toBe('secure');
    // going back to the blind step shows the stored answer; it still cannot change
    const back = run(s, { type: 'back' });
    expect(back.step).toBe('blind');
    expect(flowReducer(back, { type: 'set_blind', patch: { choice: 'watch' } }).blind.choice).toBe('secure');
    expect(flowReducer(back, { type: 'next' }).step).toBe('reveal');
  });

  it('caps the driving factors at three', () => {
    const s = flowReducer(answered(), { type: 'set_blind', patch: { factors: ['a', 'b', 'c', 'd'] } });
    expect(s.blind.factors).toEqual(['a', 'b', 'c']);
  });
});

describe('branching after the reveal', () => {
  const revealed = (flags = {}) => run(answered('secure', flags), { type: 'submit_blind', at: T1 }, { type: 'next' });

  it('needs a verdict to leave the verdict step', () => {
    const s = revealed();
    expect(s.step).toBe('verdict');
    expect(flowReducer(s, { type: 'next' }).step).toBe('verdict');
  });

  it('verdicts 3-6 require the main issue text and type', () => {
    for (const v of [3, 4, 5, 6] as const) {
      const s = run(revealed(), { type: 'set_verdict', verdict: v }, { type: 'next' });
      expect(s.step).toBe('issue');
      expect(flowReducer(s, { type: 'next', at: T2 }).step).toBe('issue');
      const textOnly = run(s, { type: 'set_issue', patch: { issue_text: 'Re-exports counted as production' } }, { type: 'next', at: T2 });
      expect(textOnly.step).toBe('issue');
      const done = run(s, { type: 'set_issue', patch: { issue_text: 'Re-exports counted as production', issue_type: 'data' } }, { type: 'next', at: T2 });
      expect(done.step).toBe('done');
      expect(() => toSubmissionPayload(s, CASE)).toThrow();
      expect(toSubmissionPayload(done, CASE).reveal.main_issue).toEqual({ text: 'Re-exports counted as production', type: 'data' });
    }
  });

  it('verdicts 1, 2 and 7 pass the issue step without text', () => {
    for (const v of [1, 2, 7] as const) {
      const s = run(revealed(), { type: 'set_verdict', verdict: v }, { type: 'next' }, { type: 'next', at: T2 });
      expect(s.step).toBe('done');
      expect(s.finished_at).toBe(T2);
    }
  });

  it('a DONT_KNOW blind answer still reveals and pre-suggests verdict 7', () => {
    const s = run(initialFlowState({ case_id: 'CASE_001' }), { type: 'next' }, { type: 'set_blind', patch: { choice: DONT_KNOW } }, { type: 'submit_blind', at: T1 });
    expect(s.step).toBe('reveal');
    expect(s.suggested_verdict).toBe(7);
    const done = run(s, { type: 'next' }, { type: 'set_verdict', verdict: 7 }, { type: 'next' }, { type: 'next', at: T2 });
    const p = toSubmissionPayload(done, CASE);
    expect(p.blind.choice).toBe(DONT_KNOW);
    expect(p.blind.confidence).toBeNull();
    expect(p.outside_expertise).toBe(true);
  });

  it('has an extras step only when the case has decision-use or threat questions', () => {
    expect(stepsFor({ has_decision_use: false, has_threat: false })).not.toContain('extras');
    expect(stepsFor({ has_decision_use: true, has_threat: false })).toContain('extras');
    const plain = run(revealed(), { type: 'set_verdict', verdict: 1 }, { type: 'next' }, { type: 'next', at: T2 });
    expect(plain.step).toBe('done');
    expect(flowReducer(plain, { type: 'set_decision_use', patch: { actionable: 3 } }).decision_use).toBeNull();
    const flagged = run(revealed({ decision_use: true, threat: true }), { type: 'set_verdict', verdict: 1 }, { type: 'next' }, { type: 'next' });
    expect(flagged.step).toBe('extras');
    const filled = run(
      flagged,
      { type: 'set_decision_use', patch: { would_do_differently: ' prioritise offtake ', actionable: 2 } },
      { type: 'set_threat', patch: { reduces_threat: 3, missing_constraints: ['financing'] } },
      { type: 'next', at: T2 },
    );
    expect(filled.step).toBe('done');
    expect(toDecisionUsePayload(filled)).toMatchObject({ case_id: 'CASE_001', would_do_differently: 'prioritise offtake', actionable: 2, identifies_problem: null });
    expect(toThreatPayload(filled, 'b'.repeat(64))).toMatchObject({ case_id: 'CASE_001', threat_snapshot_sha256: 'b'.repeat(64), reduces_threat: 3, missing_constraints: ['financing'] });
    expect(toDecisionUsePayload(plain)).toBeNull();
  });
});

describe('payload and drafts', () => {
  it('builds the case_review payload with the timing and the assignment', () => {
    const s = run(
      initialFlowState({ case_id: 'CASE_001' }, { reason: 'anchor', match: 'adjacent' }),
      { type: 'start', at: T0 },
      { type: 'next' },
      { type: 'set_blind', patch: { choice: 'partner', confidence: 3, factors: ['import_hhi', 'china_share'], rationale: '  ' } },
      { type: 'submit_blind', at: T1 },
      { type: 'next' },
      { type: 'set_verdict', verdict: 2 },
      { type: 'next' },
      { type: 'set_issue', patch: { suggested_choice: 'secure' } },
      { type: 'next', at: T2 },
    );
    const p = toSubmissionPayload(s, CASE);
    expect(p).toEqual({
      case_id: 'CASE_001',
      case_snapshot_sha256: CASE.snapshot_sha256,
      assignment_reason: 'anchor',
      expertise_match: 'adjacent',
      blind: { choice: 'partner', confidence: 3, factors: ['import_hhi', 'china_share'], rationale: null, answered_at: T1 },
      reveal: { revealed_at: T1, verdict: 2, main_issue: null, suggested_choice: 'secure', evidence_wanted: null },
      outside_expertise: false,
      time_spent_s: 630,
    });
    expect(() => toSubmissionPayload(s, { ...CASE, case_id: 'CASE_002' })).toThrow();
  });

  it('round-trips through JSON (drafts) and refuses a hand-edited draft that skips the blind step', () => {
    const s = run(answered(), { type: 'submit_blind', at: T1 });
    const restored = restoreFlowState(JSON.parse(JSON.stringify(s)), 'CASE_001');
    expect(restored).toEqual(s);
    expect(restoreFlowState(s, 'CASE_002')).toBeNull();
    expect(restoreFlowState({ version: 2 }, 'CASE_001')).toBeNull();
    expect(restoreFlowState(null, 'CASE_001')).toBeNull();
    const forged = { ...answered(), step: 'verdict', reached: 'verdict' };
    const r = restoreFlowState(JSON.parse(JSON.stringify(forged)), 'CASE_001');
    expect(r?.step).toBe('blind');
  });
});
