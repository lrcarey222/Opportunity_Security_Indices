// The UX v2 case review machine (review/flow.ts second half): blind-first with no route to the reveal before a
// valid stored blind answer (incl. tampered drafts), the reviewer states and the insufficient-evidence branch,
// red team only for disagreement or low confidence, the product step per product, notes and their conversion,
// the v2 payload, and v1 drafts that still restore.

import { describe, expect, it } from 'vitest';
import {
  CONFIDENCE_LEVEL_SCORE,
  DONT_KNOW,
  INSUFFICIENT_EVIDENCE,
  NONE_OF_THESE,
  OUTSIDE_EXPERTISE,
  REVIEW_VERDICT_CODES,
  REVIEW_VERDICT_LEGACY,
  type AnswerOption,
  type CaseKind,
} from './model.ts';
import {
  FLOW_STEPS_V2,
  canEnterV2,
  canRevealV2,
  flowReducer,
  flowReducerV2,
  initialFlowState,
  initialFlowStateV2,
  migrateV1Draft,
  needsRedTeam,
  notePhase,
  explorerComparisonValue,
  explorerStageSet,
  priorDiffers,
  restoreFlowStateV2,
  stageJaccard,
  stepsForV2,
  toSubmissionPayloadV2,
  verdictCodeConsistency,
  type CaseFlowState,
  type CaseFlowStateV2,
  type FlowAction,
  type FlowActionV2,
} from './flow.ts';

const OPTS: AnswerOption[] = ['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch'].map((value) => ({ value, label: value }));
const STAGE_OPTS: AnswerOption[] = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'].map((value) => ({ value, label: value }));
const caseOf = (kind: CaseKind = 'posture_cell', flags: { decision_use?: boolean; threat?: boolean } = {}) => ({ case_id: 'CASE_001', kind, answer_options: kind === 'sector_chain' ? STAGE_OPTS : OPTS, ...flags });
const run = (s: CaseFlowStateV2, ...as: FlowActionV2[]) => as.reduce(flowReducerV2, s);
const T0 = '2027-01-20T09:00:00.000Z';
const T1 = '2027-01-20T09:04:00.000Z';
const T2 = '2027-01-20T09:10:30.000Z';
const SNAP = { conclusion: { value: 'preserve' } };
const PAYCASE = { case_id: 'CASE_001', snapshot_sha256: 'a'.repeat(64), snapshot: SNAP };

/** At the blind step with a complete, unsubmitted answer. */
function answered(choice = 'partner', kind: CaseKind = 'posture_cell', extra: FlowActionV2[] = [], flags = {}) {
  return run(
    initialFlowStateV2(caseOf(kind, flags)),
    { type: 'start', at: T0 },
    { type: 'next' },
    { type: 'set_blind', patch: { choice, confidence_level: 'medium', driver: 'allied_alternatives' } },
    ...extra,
  );
}
const revealed = (choice = 'partner', kind: CaseKind = 'posture_cell', extra: FlowActionV2[] = [], flags = {}) => run(answered(choice, kind, extra, flags), { type: 'submit_blind', at: T1 });

describe('v2 blind-first sequence', () => {
  it('starts at the brief and moves to the blind question', () => {
    const s0 = initialFlowStateV2(caseOf());
    expect(s0.version).toBe(2);
    expect(s0.step).toBe('brief');
    expect(s0.product).toBe('country');
    expect(run(s0, { type: 'next' }).step).toBe('blind');
  });

  it('no action reaches the reveal or any later step before the blind answer is stored', () => {
    const atBlind = answered();
    const attempts: FlowActionV2[] = [
      { type: 'next' },
      ...FLOW_STEPS_V2.slice(2).map((step) => ({ type: 'goto', step }) as FlowActionV2),
      { type: 'set_verdict_code', code: 'same_same' },
      { type: 'set_reveal', patch: { validity: 'supported', issue_text: 'x' } },
      { type: 'set_product_answer', id: 'bottleneck_located', value: 'yes' },
      { type: 'convert_note', id: 'n1', to: 'data_issue' },
    ];
    for (const a of attempts) {
      const s = flowReducerV2(atBlind, a);
      expect(['brief', 'blind']).toContain(s.step);
      expect(s.reveal.verdict_code).toBeNull();
      expect(s.reveal.revealed_at).toBeNull();
      expect(s.reveal.validity).toBeNull();
    }
    for (const step of FLOW_STEPS_V2.slice(2)) expect(canEnterV2(atBlind, step)).toBe(false);
    expect(flowReducerV2(initialFlowStateV2(caseOf()), { type: 'goto', step: 'reveal' }).step).toBe('brief');
  });

  it('needs a choice, a confidence level and a driver for a canonical answer', () => {
    const s = run(initialFlowStateV2(caseOf()), { type: 'next' });
    expect(flowReducerV2(s, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const noDriver = run(s, { type: 'set_blind', patch: { choice: 'watch', confidence_level: 'high' } });
    expect(flowReducerV2(noDriver, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const noConf = run(s, { type: 'set_blind', patch: { choice: 'watch', driver: 'market_opportunity' } });
    expect(flowReducerV2(noConf, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const notAnOption = run(s, { type: 'set_blind', patch: { choice: 'build', confidence_level: 'high', driver: 'other' } });
    expect(flowReducerV2(notAnOption, { type: 'submit_blind', at: T1 }).step).toBe('blind');
  });

  it('reveals once stored and locks the blind answer', () => {
    const s = revealed('partner');
    expect(s.step).toBe('reveal');
    expect(canRevealV2(s)).toBe(true);
    const tried = flowReducerV2(s, { type: 'set_blind', patch: { choice: 'secure', confidence_level: 'high' } });
    expect(tried).toBe(s);
    expect(run(s, { type: 'back' }, { type: 'back' }).step).toBe('brief');
    expect(run(s, { type: 'back' }, { type: 'next' }).step).toBe('reveal'); // back to blind, then forward without resubmitting
    // the blind step's lock button, pressed again after coming back, moves forward and keeps the stored answer and times
    const again = run(s, { type: 'back' }, { type: 'submit_blind', at: T2 });
    expect(again.step).toBe('reveal');
    expect(again.blind).toEqual(s.blind);
    expect(again.reveal.revealed_at).toBe(s.reveal.revealed_at);
  });
});

describe('reviewer states', () => {
  it('insufficient evidence requires at least one missing-evidence category', () => {
    const s = run(initialFlowStateV2(caseOf()), { type: 'next' }, { type: 'set_blind', patch: { choice: INSUFFICIENT_EVIDENCE } });
    expect(flowReducerV2(s, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const withGap = run(s, { type: 'set_blind', patch: { missing_evidence: ['production_capacity', 'not_a_gap' as never, 'production_capacity'], missing_evidence_text: 'plant list' } });
    expect(withGap.blind.missing_evidence).toEqual(['production_capacity']);
    const r = run(withGap, { type: 'submit_blind', at: T1 });
    expect(r.step).toBe('reveal');
    // confidence and driver are optional on this branch
    const p = toSubmissionPayloadV2(run(r, { type: 'next' }, { type: 'set_verdict_code', code: 'plausible_insufficient' }, { type: 'set_reveal', patch: { validity: 'unsure', persuasion: 'unchanged', issue_text: 'no capacity data' } }, { type: 'next' }, { type: 'next', at: T2 }), PAYCASE);
    expect(p.blind.choice).toBe(INSUFFICIENT_EVIDENCE);
    expect(p.blind.missing_evidence).toEqual(['production_capacity']);
    expect(p.blind.missing_evidence_text).toBe('plant list');
    expect(p.reveal.prior_differs).toBeNull();
    expect(p.blind.ux_version).toBe(2);
  });

  it('outside expertise stores without confidence and pre-selects the outside-expertise statement', () => {
    const s = run(initialFlowStateV2(caseOf()), { type: 'next' }, { type: 'set_blind', patch: { choice: OUTSIDE_EXPERTISE } }, { type: 'submit_blind', at: T1 });
    expect(s.step).toBe('reveal');
    expect(s.reveal.verdict_code).toBe('outside_expertise');
    const done = run(s, { type: 'next' }, { type: 'next', at: T2 });
    expect(done.step).toBe('done');
    const p = toSubmissionPayloadV2(done, PAYCASE);
    expect(p.outside_expertise).toBe(true);
    expect(p.reveal.verdict).toBe(7);
    expect(p.blind.confidence).toBeNull();
  });

  it('none of these needs confidence and a driver, and keeps the optional own recommendation; v1 DONT_KNOW is read as outside expertise', () => {
    const s = run(initialFlowStateV2(caseOf()), { type: 'next' }, { type: 'set_blind', patch: { choice: NONE_OF_THESE, own_recommendation: 'a regional stockpile' } });
    expect(flowReducerV2(s, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    const ok = run(s, { type: 'set_blind', patch: { confidence_level: 'medium', driver: 'other', driver_text: 'stockpiles' } }, { type: 'submit_blind', at: T1 });
    expect(ok.step).toBe('reveal');
    expect(ok.blind.own_recommendation).toBe('a regional stockpile');
    expect(run(initialFlowStateV2(caseOf()), { type: 'set_blind', patch: { choice: DONT_KNOW } }).blind.choice).toBe(OUTSIDE_EXPERTISE);
  });

  it('branch text not asked by the final choice is dropped when the answer is stored', () => {
    const s = answered('partner', 'posture_cell', [{ type: 'set_blind', patch: { missing_evidence: ['ownership'], own_recommendation: 'x' } }]);
    const r = run(s, { type: 'submit_blind', at: T1 });
    expect(r.blind.missing_evidence).toEqual([]);
    expect(r.blind.own_recommendation).toBeNull();
  });
});

describe('verdict, red team and product steps', () => {
  it('agreeing with medium confidence skips the red team; country has no product step', () => {
    const s = run(revealed('preserve'), { type: 'next' }, { type: 'set_verdict_code', code: 'same_same' }, { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } });
    expect(needsRedTeam(s)).toBe(false);
    expect(stepsForV2(s)).toEqual(['brief', 'blind', 'reveal', 'verdict', 'done']);
    const d = run(s, { type: 'next', at: T2 });
    expect(d.step).toBe('done');
    const p = toSubmissionPayloadV2(d, PAYCASE);
    expect(p.reveal.verdict).toBe(1);
    expect(p.reveal.prior_differs).toBe(false);
    expect(p.reveal.red_team).toBeNull();
    expect(p.reveal.product_answers).toBeUndefined();
    expect(p.review_mode).toBe('decision');
  });

  it('a disagreeing statement needs the main issue with its fixed type, then shows the red team', () => {
    let s = run(revealed('partner'), { type: 'next' }, { type: 'set_verdict_code', code: 'wrong_fact' }, { type: 'set_reveal', patch: { validity: 'not_supported', persuasion: 'unchanged' } });
    expect(s.reveal.issue_type).toBe('data');
    expect(flowReducerV2(s, { type: 'next' }).step).toBe('verdict'); // no issue text yet
    // the type is fixed by the statement
    expect(flowReducerV2(s, { type: 'set_reveal', patch: { issue_type: 'method' } }).reveal.issue_type).toBe('data');
    s = run(s, { type: 'set_reveal', patch: { issue_text: 'the share counts re-exports' } }, { type: 'next' });
    expect(s.step).toBe('red_team');
    s = run(s, { type: 'set_reveal', patch: { must_be_true: 'domestic plants qualify', resolving_evidence: 'production_capacity', resolving_evidence_text: 'plant census' } }, { type: 'next', at: T2 });
    expect(s.step).toBe('done');
    const p = toSubmissionPayloadV2(s, PAYCASE);
    expect(p.reveal.verdict_code).toBe('wrong_fact');
    expect(p.reveal.verdict).toBe(REVIEW_VERDICT_LEGACY.wrong_fact);
    expect(p.reveal.main_issue).toEqual({ text: 'the share counts re-exports', type: 'data' });
    expect(p.reveal.red_team).toEqual({ must_be_true: 'domestic plants qualify', resolving_evidence: 'production_capacity', resolving_evidence_text: 'plant census' });
    expect(p.reveal.prior_differs).toBe(true);
  });

  it('same conclusion for different reasons lets the reviewer pick the issue type', () => {
    const s = run(revealed('preserve'), { type: 'next' }, { type: 'set_verdict_code', code: 'same_different_reasons' }, { type: 'set_reveal', patch: { validity: 'partly', persuasion: 'unchanged', issue_text: 'demand, not position' } });
    expect(flowReducerV2(s, { type: 'next' }).step).toBe('verdict');
    const t = run(s, { type: 'set_reveal', patch: { issue_type: 'method' } }, { type: 'next', at: T2 });
    expect(t.step).toBe('done');
    expect(toSubmissionPayloadV2(t, PAYCASE).reveal.main_issue?.type).toBe('method');
  });

  it('low confidence shows the red team even when agreeing', () => {
    const s = run(
      initialFlowStateV2(caseOf()),
      { type: 'next' },
      { type: 'set_blind', patch: { choice: 'preserve', confidence_level: 'low', driver: 'domestic_capability' } },
      { type: 'submit_blind', at: T1 },
      { type: 'next' },
      { type: 'set_verdict_code', code: 'same_same' },
      { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } },
      { type: 'next' },
    );
    expect(s.step).toBe('red_team');
    // both red-team answers are optional
    expect(run(s, { type: 'next', at: T2 }).step).toBe('done');
  });

  it('every verdict code maps to its legacy verdict', () => {
    for (const code of REVIEW_VERDICT_CODES) {
      const s = run(revealed('partner'), { type: 'next' }, { type: 'set_verdict_code', code }, { type: 'set_reveal', patch: { validity: 'partly', persuasion: 'somewhat', issue_text: 'x', issue_type: 'context' } });
      const d = run(s, { type: 'next' }, { type: 'next' }, { type: 'next', at: T2 });
      expect(d.step, code).toBe('done');
      expect(toSubmissionPayloadV2(d, PAYCASE).reveal.verdict).toBe(REVIEW_VERDICT_LEGACY[code]);
    }
  });

  it('the product step follows the product: sector asks the bottleneck question, partnership five, compact one', () => {
    const sec = run(revealed('Components', 'sector_chain', [{ type: 'set_blind', patch: { choices: ['Components', 'Refining'] } }]), { type: 'next' }, { type: 'set_verdict_code', code: 'same_same' }, { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } }, { type: 'next' });
    expect(sec.product).toBe('sector');
    expect(sec.step).toBe('product');
    expect(flowReducerV2(sec, { type: 'next' }).step).toBe('product'); // the headline question is required
    expect(flowReducerV2(sec, { type: 'set_product_answer', id: 'bottleneck_located', value: 'maybe' })).toBe(sec);
    expect(flowReducerV2(sec, { type: 'set_product_answer', id: 'complementarity', value: 'industrial' })).toBe(sec);
    const done = run(sec, { type: 'set_product_answer', id: 'bottleneck_located', value: 'partly' }, { type: 'next', at: T2 });
    expect(done.step).toBe('done');
    const p = toSubmissionPayloadV2(done, { ...PAYCASE, kind: 'sector_chain', snapshot: { conclusion: { value: 'x' }, reasoning: { sector: { explorer_stages: ['Components'], lead_stage: 'Components' } } as never } });
    expect(p.reveal.product_answers).toEqual({ bottleneck_located: 'partly' });
    expect(p.blind.choices).toEqual(['Components', 'Refining']);
    expect(p.reveal.prior_differs).toBe(false); // the lead stage matches

    const part = initialFlowStateV2({ ...caseOf('partnership') });
    expect(part.product).toBe('partnership');
    expect(stepsForV2(part)).toContain('product');
    expect(stepsForV2(initialFlowStateV2(caseOf('compact')))).toContain('product');
    expect(stepsForV2(initialFlowStateV2(caseOf('posture_cell')))).not.toContain('product');
  });

  it('a sector chain lead stage must be one of the selected stages', () => {
    const opts = ['Extraction', 'Refining', 'Components'].map((value) => ({ value, label: value }));
    const s = run(
      initialFlowStateV2({ case_id: 'CASE_030', kind: 'sector_chain', answer_options: opts }),
      { type: 'next' },
      { type: 'set_blind', patch: { choice: 'Components', choices: ['Refining'], confidence_level: 'high', driver: 'concentration_security' } },
    );
    expect(flowReducerV2(s, { type: 'submit_blind', at: T1 }).step).toBe('blind');
    expect(run(s, { type: 'set_blind', patch: { choices: ['Refining', 'Components'] } }, { type: 'submit_blind', at: T1 }).step).toBe('reveal');
    // reviewer states clear the stage list
    expect(run(s, { type: 'set_blind', patch: { choice: OUTSIDE_EXPERTISE } }, { type: 'submit_blind', at: T1 }).blind.choices).toEqual([]);
  });

  it('extras appear only for flagged cases', () => {
    const s = revealed('preserve', 'compact', [], { decision_use: true, threat: true });
    expect(stepsForV2(s)).toContain('extras');
    expect(stepsForV2(revealed('preserve', 'compact'))).not.toContain('extras');
  });

  it('prior and statement consistency', () => {
    expect(priorDiffers('partner', 'preserve')).toBe(true);
    expect(priorDiffers(NONE_OF_THESE, 'preserve')).toBeNull();
    expect(priorDiffers('partner', null)).toBeNull();
    expect(verdictCodeConsistency('persuaded', false)).toBe('inconsistent');
    expect(verdictCodeConsistency('same_same', true)).toBe('inconsistent');
    expect(verdictCodeConsistency('wrong_rule', true)).toBe('consistent');
    expect(verdictCodeConsistency('persuaded', null)).toBe('unknown');
    expect(stageJaccard(['a', 'b'], ['b', 'c'])).toBeCloseTo(1 / 3);
    expect(stageJaccard([], [])).toBe(1);
  });

  it('sector chains compare stage keys, although the reasoning carries display labels (walkthrough 2026-11-21)', () => {
    const snap = {
      conclusion: { value: 'Assembly' },
      reasoning: { sector: { explorer_stages: ['Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment'], lead_stage: 'Final Assembly & Equipment', chokepoint_note: null } },
    } as never;
    expect(explorerComparisonValue('sector_chain', snap)).toBe('Assembly');
    expect(priorDiffers('Assembly', explorerComparisonValue('sector_chain', snap))).toBe(false);
    expect(explorerStageSet('sector_chain', snap)).toEqual(['Refining', 'Components', 'Assembly']);
    expect(stageJaccard(['Refining', 'Components'], explorerStageSet('sector_chain', snap)!)).toBeCloseTo(2 / 3);
  });

  it('the payload is never half-built', () => {
    const s = revealed('partner');
    expect(() => toSubmissionPayloadV2(answered(), PAYCASE)).toThrow(/blind/);
    expect(() => toSubmissionPayloadV2(s, PAYCASE)).toThrow(/verdict/);
    const noValidity = run(s, { type: 'next' }, { type: 'set_verdict_code', code: 'persuaded' });
    expect(() => toSubmissionPayloadV2(noValidity, PAYCASE)).toThrow(/supports/);
    expect(() => toSubmissionPayloadV2(s, { ...PAYCASE, case_id: 'CASE_002' })).toThrow(/CASE_002/);
  });
});

describe('notes', () => {
  it('add, edit, delete at any step before done; conversion only after the reveal; wrap only with notes', () => {
    let s = run(initialFlowStateV2(caseOf()), { type: 'add_note', id: 'n1', text: 'check the trade year', at: T0 }, { type: 'add_note', id: 'n1', text: 'dup', at: T0 }, { type: 'add_note', id: 'n2', text: '   ', at: T0 });
    expect(s.notes.map((n) => n.id)).toEqual(['n1']);
    expect(flowReducerV2(s, { type: 'convert_note', id: 'n1', to: 'data_issue' })).toBe(s); // not before the reveal
    s = run(s, { type: 'edit_note', id: 'n1', text: 'check the 2024 trade year', at: T0 }, { type: 'next' }, { type: 'set_blind', patch: { choice: 'preserve', confidence_level: 'high', driver: 'domestic_capability' } }, { type: 'submit_blind', at: T1 });
    s = run(s, { type: 'add_note', id: 'n3', text: 'reveal reason is odd', at: T2 }, { type: 'convert_note', id: 'n1', to: 'data_issue' }, { type: 'convert_note', id: 'n3', to: 'bogus' as never });
    expect(s.notes.find((n) => n.id === 'n1')?.converted_to).toBe('data_issue');
    expect(s.notes.find((n) => n.id === 'n3')?.converted_to).toBeNull();
    expect(notePhase(s.notes[0], s)).toBe('before');
    expect(notePhase(s.notes[1], s)).toBe('after');
    s = run(s, { type: 'next' }, { type: 'set_verdict_code', code: 'same_same' }, { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } }, { type: 'next' });
    expect(s.step).toBe('wrap');
    s = run(s, { type: 'convert_note', id: 'n3', to: 'method_concern' }, { type: 'next', at: T2 });
    expect(s.step).toBe('done');
    const p = toSubmissionPayloadV2(s, PAYCASE);
    expect(p.notes?.map((n) => [n.id, n.converted_to])).toEqual([['n1', 'data_issue'], ['n3', 'method_concern']]);
    // done freezes notes
    expect(flowReducerV2(s, { type: 'add_note', id: 'n4', text: 'late', at: T2 })).toBe(s);
  });

  it('without notes the wrap step is skipped, and deleting the last note at wrap still leads to done', () => {
    let s = run(revealed('preserve'), { type: 'add_note', id: 'n1', text: 'x', at: T1 }, { type: 'next' }, { type: 'set_verdict_code', code: 'same_same' }, { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } }, { type: 'next' });
    expect(s.step).toBe('wrap');
    s = run(s, { type: 'delete_note', id: 'n1' });
    expect(stepsForV2(s)).not.toContain('wrap');
    expect(run(s, { type: 'next', at: T2 }).step).toBe('done');
    expect(run(s, { type: 'back' }).step).toBe('verdict');
  });

  it('replace_notes sanitises notes kept outside the draft', () => {
    const s = flowReducerV2(initialFlowStateV2(caseOf()), { type: 'replace_notes', notes: [{ id: 'a', text: 'ok', created_at: T0 }, { id: 'b' }, 'junk'] });
    expect(s.notes).toEqual([{ id: 'a', text: 'ok', created_at: T0, updated_at: T0, converted_to: null }]);
  });
});

describe('restore', () => {
  it('a v2 draft round-trips through JSON', () => {
    const s = run(revealed('partner'), { type: 'add_note', id: 'n1', text: 'x', at: T1 }, { type: 'next' }, { type: 'set_verdict_code', code: 'missing_factor' });
    const back = restoreFlowStateV2(JSON.parse(JSON.stringify(s)), caseOf());
    expect(back).toEqual(s);
  });

  it('a tampered draft cannot skip the blind step: invalid stored answer, post-blind step, reveal fields', () => {
    const atBlind = answered();
    const forged = { ...JSON.parse(JSON.stringify(atBlind)), step: 'verdict', reached: 'done', reveal: { ...atBlind.reveal, revealed_at: T1, verdict_code: 'same_same', validity: 'supported' } };
    const r = restoreFlowStateV2(forged, caseOf()) as CaseFlowStateV2;
    expect(r.step).toBe('blind');
    expect(r.reached).toBe('blind');
    expect(r.reveal.verdict_code).toBeNull();
    expect(r.reveal.revealed_at).toBeNull();
    expect(canRevealV2(r)).toBe(false);
    // answered_at set but the stored answer is invalid (insufficient evidence without a category)
    const bad = { ...JSON.parse(JSON.stringify(revealed(INSUFFICIENT_EVIDENCE, 'posture_cell', [{ type: 'set_blind', patch: { missing_evidence: ['ownership'] } }]))) };
    bad.blind.missing_evidence = [];
    const r2 = restoreFlowStateV2(bad, caseOf()) as CaseFlowStateV2;
    expect(r2.step).toBe('blind');
    expect(r2.blind.answered_at).toBeNull();
    // a choice that is not an option of the case
    const off = JSON.parse(JSON.stringify(revealed('partner')));
    off.blind.choice = 'build_everything';
    expect((restoreFlowStateV2(off, caseOf()) as CaseFlowStateV2).step).toBe('blind');
    // another case, junk, unknown version
    expect(restoreFlowStateV2(JSON.parse(JSON.stringify(atBlind)), { ...caseOf(), case_id: 'CASE_002' })).toBeNull();
    expect(restoreFlowStateV2('x', caseOf())).toBeNull();
    expect(restoreFlowStateV2({ ...atBlind, version: 3 }, caseOf())).toBeNull();
  });

  it('a v1 draft before its blind answer becomes a fresh v2 state (DONT_KNOW read as outside expertise)', () => {
    const acts: FlowAction[] = [{ type: 'start', at: T0 }, { type: 'next' }, { type: 'set_blind', patch: { choice: DONT_KNOW, rationale: 'no idea' } }];
    const v1 = acts.reduce(flowReducer, initialFlowState({ case_id: 'CASE_001' }));
    const r = restoreFlowStateV2(JSON.parse(JSON.stringify(v1)), caseOf()) as CaseFlowStateV2;
    expect(r.version).toBe(2);
    expect(r.migrated_from).toBe(1);
    expect(r.step).toBe('blind');
    expect(r.blind.choice).toBe(OUTSIDE_EXPERTISE);
    expect(r.blind.answered_at).toBeNull();
    expect(r.started_at).toBe(T0);
  });

  it('a v1 draft after its blind answer keeps that answer locked and asks the v2 statement; its payload carries the v1 blind', () => {
    const acts: FlowAction[] = [
      { type: 'start', at: T0 },
      { type: 'next' },
      { type: 'set_blind', patch: { choice: 'partner', confidence: 2, factors: ['import_dependence'] } },
      { type: 'submit_blind', at: T1 },
      { type: 'next' },
      { type: 'set_verdict', verdict: 5 },
      { type: 'set_issue', patch: { issue_text: 'threshold knife-edge' } },
    ];
    const v1 = acts.reduce(flowReducer, initialFlowState({ case_id: 'CASE_001' }));
    expect(v1.step).toBe('verdict');
    const r = restoreFlowStateV2(JSON.parse(JSON.stringify(v1)), caseOf()) as CaseFlowStateV2;
    expect(r.step).toBe('verdict');
    expect(canRevealV2(r)).toBe(true);
    expect(r.blind.legacy).toEqual({ confidence: 2, factors: ['import_dependence'] });
    expect(r.reveal.legacy_verdict).toBe(5);
    expect(r.reveal.issue_text).toBe('threshold knife-edge');
    expect(flowReducerV2(r, { type: 'set_blind', patch: { choice: 'secure' } })).toBe(r);
    const d = run(r, { type: 'set_verdict_code', code: 'wrong_rule' }, { type: 'set_reveal', patch: { validity: 'partly', persuasion: 'unchanged' } }, { type: 'next' });
    expect(d.step).toBe('red_team'); // disagreeing (and v1 confidence 2 reads as low)
    const p = toSubmissionPayloadV2(run(d, { type: 'next', at: T2 }), PAYCASE);
    expect(p.blind).toEqual({ choice: 'partner', confidence: 2, factors: ['import_dependence'], rationale: null, answered_at: T1 });
    expect(p.blind.ux_version).toBeUndefined();
    expect(p.reveal.verdict_code).toBe('wrong_rule');
    expect(p.reveal.main_issue?.type).toBe('decision_rule');
  });

  it('a v1 draft still restores with the v1 machine, and a finished one migrates as done', () => {
    const v1 = [{ type: 'next' }, { type: 'set_blind', patch: { choice: 'watch', confidence: 4, factors: ['market'] } }, { type: 'submit_blind', at: T1 }, { type: 'next' }, { type: 'set_verdict', verdict: 1 }, { type: 'next' }, { type: 'next', at: T2 }].reduce(
      (s: CaseFlowState, a) => flowReducer(s, a as FlowAction),
      initialFlowState({ case_id: 'CASE_001' }),
    );
    expect(v1.step).toBe('done');
    const m = migrateV1Draft(v1, caseOf());
    expect(m.step).toBe('done');
    expect(m.finished_at).toBe(T2);
    expect(canEnterV2(m, 'done')).toBe(true);
  });

  it('the review mode and confidence score travel with the payload', () => {
    const s = run(initialFlowStateV2(caseOf(), { review_mode: 'technical' }), { type: 'start', at: T0 }, { type: 'next' }, { type: 'set_blind', patch: { choice: 'preserve', confidence_level: 'high', driver: 'domestic_capability' } }, { type: 'submit_blind', at: T1 }, { type: 'next' }, { type: 'set_verdict_code', code: 'same_same' }, { type: 'set_reveal', patch: { validity: 'supported', persuasion: 'unchanged' } }, { type: 'next', at: T2 });
    const p = toSubmissionPayloadV2(s, PAYCASE);
    expect(p.review_mode).toBe('technical');
    expect(p.blind.confidence).toBe(CONFIDENCE_LEVEL_SCORE.high);
    expect(p.blind.factors).toEqual(['domestic_capability']);
    expect(p.time_spent_s).toBe(630);
  });
});

describe('reviewer-facing messages', () => {
  it('use no banned words and no bare acronyms', async () => {
    const { AVOID_RE } = await import('../content/vocabulary.ts');
    const { bareAcronymsInText } = await import('../components/testing/acronyms.ts');
    const { blindErrorsV2, verdictErrorsV2, productErrorsV2 } = await import('./flow.ts');
    const s0 = run(initialFlowStateV2(caseOf('sector_chain')), { type: 'next' });
    const msgs = [
      ...blindErrorsV2(s0),
      ...blindErrorsV2(run(s0, { type: 'set_blind', patch: { choice: 'Components', choices: ['Refining', 'Mars'] } })),
      ...blindErrorsV2(run(s0, { type: 'set_blind', patch: { choice: INSUFFICIENT_EVIDENCE } })),
      ...blindErrorsV2(run(s0, { type: 'set_blind', patch: { choice: 'bogus' } })),
    ];
    const r = revealed('Components', 'sector_chain');
    msgs.push(...verdictErrorsV2(run(r, { type: 'next' })), ...verdictErrorsV2(run(r, { type: 'next' }, { type: 'set_verdict_code', code: 'same_different_reasons' })), ...productErrorsV2(r));
    expect(msgs.length).toBeGreaterThan(8);
    for (const m of msgs) {
      expect(AVOID_RE.test(m), m).toBe(false);
      expect(bareAcronymsInText(m), m).toEqual([]);
    }
  });
});
