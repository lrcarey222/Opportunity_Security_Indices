// The blind-first case review as a pure state machine (docs/QC_SYSTEM_DESIGN.md 5 step 4):
// context -> blind -> reveal -> verdict -> issue -> extras -> done. The reveal step cannot be entered by
// any action until a blind answer (a canonical choice or DONT_KNOW) is stored, and once stored the blind
// answer is locked: later edits are ignored, so the prior a reviewer gave cannot be revised after seeing
// the Explorer's conclusion. Verdicts 3-6 need the "single most important miss" (text + type); a DONT_KNOW
// blind answer pre-suggests verdict 7 but still reveals. The extras step exists only for cases flagged
// decision_use or threat. State is plain JSON (drafts autosave it); timestamps come in with the actions.

import {
  DONT_KNOW,
  VERDICTS_NEEDING_EXPLANATION,
  type AssignmentReason,
  type BlindAnswer,
  type CaseDefinition,
  type CaseReviewPayload,
  type DecisionUsePayload,
  type ExpertiseMatch,
  type IssueType,
  type Likert4,
  type MissingConstraint,
  type ThreatReviewPayload,
  type Verdict,
} from './model.ts';

/** The label of the "I don't know" answer, wherever a choice is asked. */
export const DONT_KNOW_LABEL = "I don't know / outside my expertise";

export const FLOW_STEPS = ['context', 'blind', 'reveal', 'verdict', 'issue', 'extras', 'done'] as const;
export type FlowStep = (typeof FLOW_STEPS)[number];

/** Steps that show or depend on the Explorer's conclusion. */
const POST_BLIND: readonly FlowStep[] = ['reveal', 'verdict', 'issue', 'extras', 'done'];

export const MAX_FACTORS = 3;

export interface BlindDraft {
  choice: string | null;
  confidence: 1 | 2 | 3 | 4 | 5 | null;
  factors: string[];
  rationale: string | null;
  answered_at: string | null;
}

export interface RevealDraft {
  revealed_at: string | null;
  verdict: Verdict | null;
  issue_text: string;
  issue_type: IssueType | null;
  metric_ids: string[];
  suggested_choice: string | null;
  evidence_wanted: string | null;
}

export type DecisionUseDraft = Omit<DecisionUsePayload, 'case_id'>;
export type ThreatDraft = Omit<ThreatReviewPayload, 'case_id' | 'threat_snapshot_sha256'>;

export interface CaseFlowState {
  version: 1;
  case_id: string;
  step: FlowStep;
  /** The furthest step reached (navigation back and forth stays within it). */
  reached: FlowStep;
  assignment_reason: AssignmentReason;
  expertise_match: ExpertiseMatch;
  has_decision_use: boolean;
  has_threat: boolean;
  blind: BlindDraft;
  reveal: RevealDraft;
  /** Verdict the form pre-selects (7 after a DONT_KNOW blind answer); the reviewer can change it. */
  suggested_verdict: Verdict | null;
  decision_use: DecisionUseDraft | null;
  threat: ThreatDraft | null;
  started_at: string | null;
  finished_at: string | null;
}

export type FlowAction =
  | { type: 'start'; at: string }
  | { type: 'next'; at?: string }
  | { type: 'back' }
  | { type: 'goto'; step: FlowStep }
  | { type: 'set_blind'; patch: Partial<Omit<BlindDraft, 'answered_at'>> }
  | { type: 'submit_blind'; at: string }
  | { type: 'set_verdict'; verdict: Verdict }
  | { type: 'set_issue'; patch: Partial<Omit<RevealDraft, 'revealed_at' | 'verdict'>> }
  | { type: 'set_decision_use'; patch: Partial<DecisionUseDraft> }
  | { type: 'set_threat'; patch: Partial<ThreatDraft> };

export function initialFlowState(
  c: Pick<CaseDefinition, 'case_id' | 'decision_use' | 'threat'>,
  a: { reason: AssignmentReason; match: ExpertiseMatch } = { reason: 'assigned', match: 'in' },
): CaseFlowState {
  return {
    version: 1,
    case_id: c.case_id,
    step: 'context',
    reached: 'context',
    assignment_reason: a.reason,
    expertise_match: a.match,
    has_decision_use: c.decision_use === true,
    has_threat: c.threat === true,
    blind: { choice: null, confidence: null, factors: [], rationale: null, answered_at: null },
    reveal: { revealed_at: null, verdict: null, issue_text: '', issue_type: null, metric_ids: [], suggested_choice: null, evidence_wanted: null },
    suggested_verdict: null,
    decision_use: null,
    threat: null,
    started_at: null,
    finished_at: null,
  };
}

const idx = (s: FlowStep) => FLOW_STEPS.indexOf(s);
const later = (a: FlowStep, b: FlowStep): FlowStep => (idx(a) >= idx(b) ? a : b);

/** Whether the blind answer is stored (and the reveal may render). */
export function canReveal(s: CaseFlowState): boolean {
  return s.blind.answered_at !== null && typeof s.blind.choice === 'string' && s.blind.choice.length > 0;
}

/** The steps this case walks through (extras only when the case has decision-use or threat questions). */
export function stepsFor(s: Pick<CaseFlowState, 'has_decision_use' | 'has_threat'>): FlowStep[] {
  return FLOW_STEPS.filter((x) => x !== 'extras' || s.has_decision_use || s.has_threat);
}

/** Problems with the blind answer (empty when it can be submitted). */
export function blindErrors(s: CaseFlowState): string[] {
  const e: string[] = [];
  const b = s.blind;
  if (!b.choice) e.push('Choose an answer, or say you do not know.');
  if (b.factors.length > MAX_FACTORS) e.push(`Pick at most ${MAX_FACTORS} driving factors.`);
  if (b.choice && b.choice !== DONT_KNOW && b.factors.length === 0) e.push('Pick at least one factor that drives your answer.');
  return e;
}

/** Whether the verdict needs the single most important miss. */
export const verdictNeedsIssue = (v: Verdict | null): boolean => v !== null && VERDICTS_NEEDING_EXPLANATION.includes(v);

/** Problems with the issue step (empty when it can be left). */
export function issueErrors(s: CaseFlowState): string[] {
  const e: string[] = [];
  if (verdictNeedsIssue(s.reveal.verdict)) {
    if (!s.reveal.issue_text.trim()) e.push('Describe the single most important thing the Explorer misses.');
    if (!s.reveal.issue_type) e.push('Say what kind of problem it is.');
  }
  return e;
}

/** Problems that keep the current step from advancing. */
export function stepErrors(s: CaseFlowState): string[] {
  switch (s.step) {
    case 'blind':
      return canReveal(s) ? [] : blindErrors(s);
    case 'verdict':
      return s.reveal.verdict === null ? ['Choose a verdict.'] : [];
    case 'issue':
      return issueErrors(s);
    default:
      return [];
  }
}

function nextStep(s: CaseFlowState): FlowStep {
  const steps = stepsFor(s);
  return steps[Math.min(steps.length - 1, steps.indexOf(s.step) + 1)];
}

/** Whether a step may be shown now: never a post-blind step before the blind answer is stored. */
export function canEnter(s: CaseFlowState, step: FlowStep): boolean {
  if (!stepsFor(s).includes(step)) return false;
  if (POST_BLIND.includes(step) && !canReveal(s)) return false;
  if (idx(step) > idx(s.reached)) return false;
  if (step === 'done') return s.finished_at !== null;
  return true;
}

/** The reducer. Invalid actions return the state unchanged (the same object). */
export function flowReducer(s: CaseFlowState, a: FlowAction): CaseFlowState {
  switch (a.type) {
    case 'start':
      return s.started_at ? s : { ...s, started_at: a.at };
    case 'set_blind': {
      if (s.blind.answered_at !== null) return s; // locked once stored
      const patch = { ...a.patch };
      if (patch.factors) patch.factors = patch.factors.slice(0, MAX_FACTORS);
      return { ...s, blind: { ...s.blind, ...patch } };
    }
    case 'submit_blind': {
      if (s.step !== 'blind' || canReveal(s) || blindErrors(s).length) return s;
      const dk = s.blind.choice === DONT_KNOW;
      return {
        ...s,
        blind: { ...s.blind, answered_at: a.at },
        reveal: { ...s.reveal, revealed_at: a.at },
        suggested_verdict: dk ? 7 : s.suggested_verdict,
        step: 'reveal',
        reached: later(s.reached, 'reveal'),
      };
    }
    case 'next': {
      if (s.step === 'done') return s;
      // the blind step advances only by submit_blind (or, after it, back to where the reviewer was)
      if (s.step === 'blind') return canReveal(s) ? { ...s, step: 'reveal' } : s;
      if (stepErrors(s).length) return s;
      const to = nextStep(s);
      if (POST_BLIND.includes(to) && !canReveal(s)) return s;
      if (to === 'done') return { ...s, step: 'done', reached: 'done', finished_at: s.finished_at ?? a.at ?? null };
      return { ...s, step: to, reached: later(s.reached, to) };
    }
    case 'back': {
      const steps = stepsFor(s);
      const i = steps.indexOf(s.step);
      return i > 0 && s.step !== 'done' ? { ...s, step: steps[i - 1] } : s;
    }
    case 'goto':
      return canEnter(s, a.step) && a.step !== s.step ? { ...s, step: a.step } : s;
    case 'set_verdict': {
      if (!canReveal(s) || a.verdict < 1 || a.verdict > 7) return s;
      return { ...s, reveal: { ...s.reveal, verdict: a.verdict } };
    }
    case 'set_issue':
      if (!canReveal(s)) return s;
      return { ...s, reveal: { ...s.reveal, ...a.patch } };
    case 'set_decision_use':
      if (!canReveal(s) || !s.has_decision_use) return s;
      return { ...s, decision_use: { ...emptyDecisionUse(), ...s.decision_use, ...a.patch } };
    case 'set_threat':
      if (!canReveal(s) || !s.has_threat) return s;
      return { ...s, threat: { ...emptyThreat(), ...s.threat, ...a.patch } };
  }
}

export function emptyDecisionUse(): DecisionUseDraft {
  return { would_do_differently: '', actionable: null, identifies_problem: null, separates_diagnosis_from_prescription: null, right_geography: null, evidence_required: null, improvement: null, decision_use: null };
}

export function emptyThreat(): ThreatDraft {
  return { reduces_threat: null, scale_sufficient: null, timing_plausible: null, commercially_plausible: null, missing_constraints: [] as MissingConstraint[], success_looks_like: null, stop_point: null };
}

const nn = (x: string | null | undefined): string | null => (typeof x === 'string' && x.trim() ? x.trim() : null);

/**
 * The case_review payload of a finished (or at least verdicted) review. Throws when the blind answer is not
 * stored or no verdict is chosen, or when a verdict 3-6 lacks its issue: a payload is never half-built.
 */
export function toSubmissionPayload(s: CaseFlowState, c: Pick<CaseDefinition, 'case_id'> & { snapshot_sha256: string }): CaseReviewPayload {
  if (s.case_id !== c.case_id) throw new Error(`flow state is for ${s.case_id}, not ${c.case_id}`);
  if (!canReveal(s)) throw new Error('the blind answer is not stored');
  if (s.reveal.verdict === null || s.reveal.revealed_at === null) throw new Error('no verdict');
  if (issueErrors(s).length) throw new Error(issueErrors(s).join(' '));
  const blind: BlindAnswer = {
    choice: s.blind.choice as string,
    confidence: s.blind.choice === DONT_KNOW ? null : s.blind.confidence,
    factors: s.blind.factors.slice(0, MAX_FACTORS),
    rationale: nn(s.blind.rationale),
    answered_at: s.blind.answered_at as string,
  };
  const issue = nn(s.reveal.issue_text) && s.reveal.issue_type ? { text: s.reveal.issue_text.trim(), type: s.reveal.issue_type, ...(s.reveal.metric_ids.length ? { metric_ids: [...s.reveal.metric_ids] } : {}) } : null;
  const spent = s.started_at && s.finished_at ? Math.round((Date.parse(s.finished_at) - Date.parse(s.started_at)) / 1000) : null;
  return {
    case_id: c.case_id,
    case_snapshot_sha256: c.snapshot_sha256,
    assignment_reason: s.assignment_reason,
    expertise_match: s.expertise_match,
    blind,
    reveal: {
      revealed_at: s.reveal.revealed_at,
      verdict: s.reveal.verdict,
      main_issue: issue,
      suggested_choice: nn(s.reveal.suggested_choice),
      evidence_wanted: nn(s.reveal.evidence_wanted),
    },
    outside_expertise: s.blind.choice === DONT_KNOW || s.reveal.verdict === 7,
    time_spent_s: spent !== null && Number.isFinite(spent) && spent >= 0 ? spent : null,
  };
}

const likertOrNull = (x: Likert4 | null | undefined): Likert4 | null => (x === 1 || x === 2 || x === 3 || x === 4 ? x : null);

/** The decision_use payload of the extras step (null when the case has none or nothing was answered). */
export function toDecisionUsePayload(s: CaseFlowState): DecisionUsePayload | null {
  const d = s.decision_use;
  if (!s.has_decision_use || !d) return null;
  return {
    case_id: s.case_id,
    would_do_differently: d.would_do_differently.trim(),
    actionable: likertOrNull(d.actionable),
    identifies_problem: likertOrNull(d.identifies_problem),
    separates_diagnosis_from_prescription: likertOrNull(d.separates_diagnosis_from_prescription),
    right_geography: likertOrNull(d.right_geography),
    evidence_required: nn(d.evidence_required),
    improvement: nn(d.improvement),
    decision_use: nn(d.decision_use),
  };
}

/** The threat_review payload of the extras step (null when the case has none or nothing was answered). */
export function toThreatPayload(s: CaseFlowState, threatSnapshotSha256: string): ThreatReviewPayload | null {
  const t = s.threat;
  if (!s.has_threat || !t) return null;
  return {
    case_id: s.case_id,
    threat_snapshot_sha256: threatSnapshotSha256,
    reduces_threat: likertOrNull(t.reduces_threat),
    scale_sufficient: likertOrNull(t.scale_sufficient),
    timing_plausible: likertOrNull(t.timing_plausible),
    commercially_plausible: likertOrNull(t.commercially_plausible),
    missing_constraints: [...t.missing_constraints],
    success_looks_like: nn(t.success_looks_like),
    stop_point: nn(t.stop_point),
  };
}

/** A stored draft, checked: the state when it is a flow state for this case, else null (start over). */
export function restoreFlowState(x: unknown, caseId: string): CaseFlowState | null {
  if (typeof x !== 'object' || x === null) return null;
  const s = x as Partial<CaseFlowState>;
  if (s.version !== 1 || s.case_id !== caseId || !FLOW_STEPS.includes(s.step as FlowStep) || typeof s.blind !== 'object' || typeof s.reveal !== 'object' || !s.blind || !s.reveal) return null;
  const st = s as CaseFlowState;
  // a draft edited by hand cannot skip the blind step
  if (POST_BLIND.includes(st.step) && !canReveal(st)) return { ...st, step: 'blind', reached: 'blind' };
  return st;
}
