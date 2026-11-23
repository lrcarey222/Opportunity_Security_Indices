// The blind-first case review as a pure state machine (docs/QC_SYSTEM_DESIGN.md 5 step 4):
// context -> blind -> reveal -> verdict -> issue -> extras -> done. The reveal step cannot be entered by
// any action until a blind answer (a canonical choice or DONT_KNOW) is stored, and once stored the blind
// answer is locked: later edits are ignored, so the prior a reviewer gave cannot be revised after seeing
// the Explorer's conclusion. Verdicts 3-6 need the "single most important miss" (text + type); a DONT_KNOW
// blind answer pre-suggests verdict 7 but still reveals. The extras step exists only for cases flagged
// decision_use or threat. State is plain JSON (drafts autosave it); timestamps come in with the actions.

//
// UX v2 (docs/QC_UX_REVIEW.md; the second half of this file) is a separate machine with its own state type
// (CaseFlowStateV2, version 2): brief -> blind -> reveal -> verdict -> red_team -> product -> extras -> wrap ->
// done. The v1 machine is kept unchanged so v1 drafts and the v1 view keep working; restoreFlowStateV2
// migrates a v1 draft safely (see migrateV1Draft).

import { STAGE_DISPLAY, SUPPLY_STAGES } from '../lib/stages.ts';
import {
  CONFIDENCE_LEVEL_LABEL,
  CONFIDENCE_LEVEL_SCORE,
  DISAGREEING_VERDICT_CODES,
  DONT_KNOW,
  DRIVER_FACTOR_LABEL,
  EVIDENCE_GAP_LABEL,
  INSUFFICIENT_EVIDENCE,
  ISSUE_TYPE_LABEL,
  NONE_OF_THESE,
  NOTE_CONVERSION_LABEL,
  OUTSIDE_EXPERTISE,
  PRODUCT_QUESTIONS,
  REVIEWER_STATES,
  REVIEW_PRODUCT_OF_KIND,
  REVIEW_VERDICT_CODES,
  REVIEW_VERDICT_ISSUE_TYPE,
  REVIEW_VERDICT_LEGACY,
  VERDICTS_NEEDING_EXPLANATION,
  type AnswerOption,
  type AssignmentReason,
  type BlindAnswer,
  type CaseDefinition,
  type CaseKind,
  type CaseReasoning,
  type CaseReviewPayload,
  type ConfidenceLevel,
  type DecisionUsePayload,
  type DriverFactor,
  type EvidenceGap,
  type ExpertiseMatch,
  type IssueType,
  type Likert4,
  type MissingConstraint,
  type NoteConversion,
  type Persuasion,
  type ReviewerNote,
  type ReviewMode,
  type ReviewProduct,
  type ReviewVerdictCode,
  type RevealResponse,
  type ThreatReviewPayload,
  type Validity,
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
export function toDecisionUsePayload(s: Pick<CaseFlowState, 'case_id' | 'has_decision_use' | 'decision_use'>): DecisionUsePayload | null {
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
export function toThreatPayload(s: Pick<CaseFlowState, 'case_id' | 'has_threat' | 'threat'>, threatSnapshotSha256: string): ThreatReviewPayload | null {
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

// =============================================================================================================
// UX v2: the case brief, the blind answer with confidence level and one driver, the reviewer states, the
// reveal, the verdict taxonomy with validity and persuasion, the red-team prompts, the product questions, the
// decision-use / threat extras and the notes wrap-up. Same guarantees as v1: no action reaches the reveal or any
// later step before a valid blind answer is stored, and once stored it is locked.

export const FLOW_STEPS_V2 = ['brief', 'blind', 'reveal', 'verdict', 'red_team', 'product', 'extras', 'wrap', 'done'] as const;
export type FlowStepV2 = (typeof FLOW_STEPS_V2)[number];

/** Steps that show or depend on the Explorer's conclusion. */
const POST_BLIND_V2: readonly FlowStepV2[] = ['reveal', 'verdict', 'red_team', 'product', 'extras', 'wrap', 'done'];

export const MAX_NOTES = 30;
export const MAX_NOTE_LENGTH = 4000;
/** Free-text answers are cut to this length in payloads (a pasted document is not a review answer). */
export const MAX_TEXT_LENGTH = 8000;

export const VALIDITY_VALUES: readonly Validity[] = ['supported', 'partly', 'not_supported', 'unsure'];
export const PERSUASION_VALUES: readonly Persuasion[] = ['changed', 'somewhat', 'unchanged'];
const CONFIDENCE_LEVELS = Object.keys(CONFIDENCE_LEVEL_LABEL) as ConfidenceLevel[];
const DRIVERS = Object.keys(DRIVER_FACTOR_LABEL) as DriverFactor[];
const GAPS = Object.keys(EVIDENCE_GAP_LABEL) as EvidenceGap[];
const CONVERSIONS = Object.keys(NOTE_CONVERSION_LABEL) as NoteConversion[];
const ISSUE_TYPES = Object.keys(ISSUE_TYPE_LABEL) as IssueType[];

/**
 * The blind answer as a v2 review stores it. `own_recommendation` (the reviewer's own recommendation when they
 * answer NONE_OF_THESE) extends model.ts BlindAnswer; readers treat it as optional reviewer free text.
 */
export interface BlindAnswerV2 extends BlindAnswer {
  own_recommendation?: string | null;
}

export interface BlindDraftV2 {
  /** A canonical option value or a REVIEWER_STATES value. Sector chain: the stage to act on first. */
  choice: string | null;
  /** Sector chain only: every stage the reviewer would intervene at (choice must be one of them when any are set). */
  choices: string[];
  confidence_level: ConfidenceLevel | null;
  driver: DriverFactor | null;
  driver_text: string | null;
  rationale: string | null;
  /** Insufficient-evidence branch. */
  missing_evidence: EvidenceGap[];
  missing_evidence_text: string | null;
  /** None-of-these branch: what the reviewer would recommend instead (optional). */
  own_recommendation: string | null;
  answered_at: string | null;
  /** Set only when migrated from a v1 draft whose blind answer was already stored: the v1 answer, read-only. */
  legacy: { confidence: 1 | 2 | 3 | 4 | 5 | null; factors: string[] } | null;
}

export interface RevealDraftV2 {
  revealed_at: string | null;
  verdict_code: ReviewVerdictCode | null;
  validity: Validity | null;
  persuasion: Persuasion | null;
  /** The main issue (required for the codes verdictCodeNeedsIssue names). */
  issue_text: string;
  issue_type: IssueType | null;
  metric_ids: string[];
  suggested_choice: string | null;
  evidence_wanted: string | null;
  /** Red team: "What would have to be true for the Explorer's conclusion to be correct?" */
  must_be_true: string | null;
  /** Red team: "What single piece of additional evidence would most help resolve this case?" */
  resolving_evidence: EvidenceGap | null;
  resolving_evidence_text: string | null;
  /** PRODUCT_QUESTIONS answers keyed by question id. */
  product_answers: Record<string, string>;
  /** The v1 verdict of a migrated v1 draft (shown for reference; the v2 statement is asked again). */
  legacy_verdict: Verdict | null;
}

export interface CaseFlowStateV2 {
  version: 2;
  case_id: string;
  kind: CaseKind;
  product: ReviewProduct;
  step: FlowStepV2;
  /** The furthest step reached (navigation back and forth stays within it). */
  reached: FlowStepV2;
  assignment_reason: AssignmentReason;
  expertise_match: ExpertiseMatch;
  has_decision_use: boolean;
  has_threat: boolean;
  review_mode: ReviewMode;
  /** The case's canonical option values (empty = not checked). Reviewer states are always allowed. */
  options: string[];
  blind: BlindDraftV2;
  reveal: RevealDraftV2;
  notes: ReviewerNote[];
  decision_use: DecisionUseDraft | null;
  threat: ThreatDraft | null;
  started_at: string | null;
  finished_at: string | null;
  /** 1 when this state was migrated from a v1 draft. */
  migrated_from: 1 | null;
}

export type FlowActionV2 =
  | { type: 'start'; at: string }
  | { type: 'next'; at?: string }
  | { type: 'back' }
  | { type: 'goto'; step: FlowStepV2 }
  | { type: 'set_blind'; patch: Partial<Omit<BlindDraftV2, 'answered_at' | 'legacy'>> }
  | { type: 'submit_blind'; at: string }
  | { type: 'set_verdict_code'; code: ReviewVerdictCode }
  | {
      type: 'set_reveal';
      patch: Partial<Omit<RevealDraftV2, 'revealed_at' | 'verdict_code' | 'product_answers' | 'legacy_verdict'>>;
    }
  | { type: 'set_product_answer'; id: string; value: string | null }
  | { type: 'set_decision_use'; patch: Partial<DecisionUseDraft> }
  | { type: 'set_threat'; patch: Partial<ThreatDraft> }
  | { type: 'add_note'; id: string; text: string; at: string }
  | { type: 'edit_note'; id: string; text: string; at: string }
  | { type: 'delete_note'; id: string }
  | { type: 'convert_note'; id: string; to: NoteConversion | null }
  | { type: 'replace_notes'; notes: unknown };

export type CaseForFlowV2 = Pick<CaseDefinition, 'case_id' | 'kind' | 'decision_use' | 'threat'> & { answer_options?: readonly AnswerOption[] };

export interface InitV2Options {
  reason?: AssignmentReason;
  match?: ExpertiseMatch;
  review_mode?: ReviewMode | null;
  /** Notes kept outside the draft (storage.ts loadNotes), e.g. after a draft was discarded. */
  notes?: unknown;
}

const isReviewerState = (c: string | null | undefined): boolean => typeof c === 'string' && (REVIEWER_STATES.includes(c) || c === DONT_KNOW);
/** Whether a blind choice is one of the reviewer states (none of these / insufficient evidence / outside expertise, or v1 don't know). */
export const isReviewerStateChoice = isReviewerState;

/** The canonical option values of a case (reviewer states and DONT_KNOW left out). */
export function canonicalOptionValues(options: readonly AnswerOption[] | undefined): string[] {
  return [...new Set((options ?? []).map((o) => o.value).filter((v) => typeof v === 'string' && v && !isReviewerState(v)))];
}

/** The product a case kind reviews (sector when the kind is unknown). */
export const productOf = (kind: CaseKind | string | null | undefined): ReviewProduct => REVIEW_PRODUCT_OF_KIND[kind as CaseKind] ?? 'sector';

/** The tailored post-reveal questions of a product. */
export const productQuestions = (product: ReviewProduct) => PRODUCT_QUESTIONS[product] ?? [];

function emptyBlindV2(): BlindDraftV2 {
  return {
    choice: null,
    choices: [],
    confidence_level: null,
    driver: null,
    driver_text: null,
    rationale: null,
    missing_evidence: [],
    missing_evidence_text: null,
    own_recommendation: null,
    answered_at: null,
    legacy: null,
  };
}

function emptyRevealV2(): RevealDraftV2 {
  return {
    revealed_at: null,
    verdict_code: null,
    validity: null,
    persuasion: null,
    issue_text: '',
    issue_type: null,
    metric_ids: [],
    suggested_choice: null,
    evidence_wanted: null,
    must_be_true: null,
    resolving_evidence: null,
    resolving_evidence_text: null,
    product_answers: {},
    legacy_verdict: null,
  };
}

const isStr = (x: unknown): x is string => typeof x === 'string';
const strOrNull = (x: unknown): string | null => (typeof x === 'string' ? x : null);
const oneOf = <T extends string>(x: unknown, values: readonly T[]): T | null => (typeof x === 'string' && (values as readonly string[]).includes(x) ? (x as T) : null);
const uniq = <T>(xs: readonly T[]): T[] => [...new Set(xs)];

/** Valid notes only (id + string text + timestamps), at most MAX_NOTES, texts cut to MAX_NOTE_LENGTH, ids unique. */
export function sanitizeNotes(x: unknown): ReviewerNote[] {
  if (!Array.isArray(x)) return [];
  const seen = new Set<string>();
  const out: ReviewerNote[] = [];
  for (const n of x) {
    if (!n || typeof n !== 'object') continue;
    const r = n as Partial<ReviewerNote>;
    if (!isStr(r.id) || !r.id || seen.has(r.id) || !isStr(r.text) || !isStr(r.created_at)) continue;
    seen.add(r.id);
    out.push({
      id: r.id,
      text: r.text.slice(0, MAX_NOTE_LENGTH),
      created_at: r.created_at,
      updated_at: isStr(r.updated_at) ? r.updated_at : r.created_at,
      converted_to: oneOf(r.converted_to, CONVERSIONS),
    });
    if (out.length >= MAX_NOTES) break;
  }
  return out;
}

export function initialFlowStateV2(c: CaseForFlowV2, o: InitV2Options = {}): CaseFlowStateV2 {
  return {
    version: 2,
    case_id: c.case_id,
    kind: c.kind,
    product: productOf(c.kind),
    step: 'brief',
    reached: 'brief',
    assignment_reason: o.reason ?? 'assigned',
    expertise_match: o.match ?? 'in',
    has_decision_use: c.decision_use === true,
    has_threat: c.threat === true,
    review_mode: o.review_mode === 'technical' ? 'technical' : 'decision',
    options: canonicalOptionValues(c.answer_options),
    blind: emptyBlindV2(),
    reveal: emptyRevealV2(),
    notes: sanitizeNotes(o.notes),
    decision_use: null,
    threat: null,
    started_at: null,
    finished_at: null,
    migrated_from: null,
  };
}

const idxV2 = (s: FlowStepV2) => FLOW_STEPS_V2.indexOf(s);
const laterV2 = (a: FlowStepV2, b: FlowStepV2): FlowStepV2 => (idxV2(a) >= idxV2(b) ? a : b);

/** Problems with the blind answer (empty when it can be stored). A migrated v1 answer is checked as v1 was. */
export function blindErrorsV2(s: Pick<CaseFlowStateV2, 'kind' | 'options' | 'blind'>): string[] {
  const b = s.blind;
  if (b.legacy) return isStr(b.choice) && b.choice ? [] : ['Choose an answer.'];
  const e: string[] = [];
  if (!b.choice) return ['Choose an answer, or one of the last three options.'];
  const state = isReviewerState(b.choice);
  if (!state && s.options.length && !s.options.includes(b.choice)) e.push('Choose one of the options shown.');
  if (b.choice === DONT_KNOW) e.push('Choose one of the options shown.');
  if (!state || b.choice === NONE_OF_THESE) {
    if (!b.confidence_level) e.push('Say how confident you are.');
    if (!b.driver) e.push('Pick the most important factor driving your judgment.');
  }
  if (b.choice === INSUFFICIENT_EVIDENCE && b.missing_evidence.length === 0) e.push('Say what information is missing (pick at least one).');
  if (s.kind === 'sector_chain' && !state) {
    if (b.choices.length && !b.choices.includes(b.choice)) e.push('The stage you would act on first must be one of the stages you selected.');
    if (s.options.length && b.choices.some((x) => !s.options.includes(x))) e.push('Select stages from the chain shown.');
  }
  return e;
}

/** Whether a valid blind answer is stored (and the reveal may render). */
export function canRevealV2(s: Pick<CaseFlowStateV2, 'kind' | 'options' | 'blind'>): boolean {
  return s.blind.answered_at !== null && isStr(s.blind.choice) && s.blind.choice.length > 0 && blindErrorsV2(s).length === 0;
}

/** Whether the blind answer was given with low confidence (a v1 answer: confidence 1-2). */
export function blindConfidenceLow(s: Pick<CaseFlowStateV2, 'blind'>): boolean {
  const b = s.blind;
  if (b.legacy) return b.legacy.confidence !== null && b.legacy.confidence <= 2;
  return b.confidence_level === 'low';
}

/** Whether the verdict statement needs the main issue (every disagreeing code, and "same conclusion, different reasons"). */
export const verdictCodeNeedsIssue = (c: ReviewVerdictCode | null | undefined): boolean =>
  !!c && (DISAGREEING_VERDICT_CODES.includes(c) || c === 'same_different_reasons');

/** The issue type a code fixes (null when the reviewer picks it: same_different_reasons). */
export const verdictCodeIssueType = (c: ReviewVerdictCode | null | undefined): IssueType | null => (c ? (REVIEW_VERDICT_ISSUE_TYPE[c] ?? null) : null);

/** Whether the red-team questions apply: a disagreeing statement or a low-confidence blind answer. */
export function needsRedTeam(s: Pick<CaseFlowStateV2, 'blind' | 'reveal'>): boolean {
  const c = s.reveal.verdict_code;
  return (!!c && DISAGREEING_VERDICT_CODES.includes(c)) || blindConfidenceLow(s);
}

const hasNotes = (s: Pick<CaseFlowStateV2, 'notes'>) => s.notes.some((n) => n.text.trim().length > 0);

/** The steps this case walks through now (red team, product, extras and wrap appear only when they apply). */
export function stepsForV2(s: CaseFlowStateV2): FlowStepV2[] {
  return FLOW_STEPS_V2.filter((x) => {
    if (x === 'red_team') return needsRedTeam(s);
    if (x === 'product') return productQuestions(s.product).length > 0;
    if (x === 'extras') return s.has_decision_use || s.has_threat;
    if (x === 'wrap') return hasNotes(s);
    return true;
  });
}

/** Problems with the verdict step. */
export function verdictErrorsV2(s: CaseFlowStateV2): string[] {
  const r = s.reveal;
  if (!r.verdict_code) return ['Choose the statement that best describes your view.'];
  const e: string[] = [];
  if (r.verdict_code !== 'outside_expertise') {
    if (!r.validity) e.push("Say whether the evidence supports the Explorer's conclusion.");
    if (!r.persuasion) e.push('Say whether the evidence changed your view.');
  }
  if (verdictCodeNeedsIssue(r.verdict_code)) {
    if (!r.issue_text.trim()) e.push('Say in a sentence or two where it fails, or which reasons differ.');
    if (!r.issue_type) e.push('Say what kind of problem it is.');
  }
  return e;
}

/** Problems with the product step: the first (headline) question needs an answer, unless the review is outside expertise. */
export function productErrorsV2(s: CaseFlowStateV2): string[] {
  const qs = productQuestions(s.product);
  if (!qs.length || s.reveal.verdict_code === 'outside_expertise') return [];
  return s.reveal.product_answers[qs[0].id] ? [] : ['Answer the first question (or choose "Not sure").'];
}

/** Problems that keep the current step from advancing. */
export function stepErrorsV2(s: CaseFlowStateV2): string[] {
  switch (s.step) {
    case 'blind':
      return canRevealV2(s) ? [] : blindErrorsV2(s);
    case 'verdict':
      return verdictErrorsV2(s);
    case 'product':
      return productErrorsV2(s);
    default:
      return [];
  }
}

function neighbourV2(s: CaseFlowStateV2, dir: 1 | -1): FlowStepV2 | null {
  const steps = stepsForV2(s);
  const i = steps.indexOf(s.step);
  if (i >= 0) return steps[i + dir] ?? null;
  // the current step no longer applies (e.g. wrap after the last note was deleted): the nearest one that does
  const at = idxV2(s.step);
  const cand = dir === 1 ? steps.filter((x) => idxV2(x) > at) : steps.filter((x) => idxV2(x) < at).reverse();
  return cand[0] ?? null;
}

/** Whether a step may be shown now: never a post-blind step before a valid blind answer is stored. */
export function canEnterV2(s: CaseFlowStateV2, step: FlowStepV2): boolean {
  if (!stepsForV2(s).includes(step)) return false;
  if (POST_BLIND_V2.includes(step) && !canRevealV2(s)) return false;
  if (idxV2(step) > idxV2(s.reached)) return false;
  if (step === 'done') return s.finished_at !== null;
  return true;
}

function sanitizeBlindPatch(s: CaseFlowStateV2, p: Partial<Omit<BlindDraftV2, 'answered_at' | 'legacy'>>): Partial<BlindDraftV2> {
  const out: Partial<BlindDraftV2> = {};
  if ('choice' in p) out.choice = p.choice === DONT_KNOW ? OUTSIDE_EXPERTISE : isStr(p.choice) && p.choice ? p.choice : null;
  if ('choices' in p) out.choices = s.kind === 'sector_chain' && Array.isArray(p.choices) ? uniq(p.choices.filter((x) => isStr(x) && x && !isReviewerState(x))) : [];
  if ('confidence_level' in p) out.confidence_level = oneOf(p.confidence_level, CONFIDENCE_LEVELS);
  if ('driver' in p) out.driver = oneOf(p.driver, DRIVERS);
  if ('missing_evidence' in p) out.missing_evidence = Array.isArray(p.missing_evidence) ? uniq(p.missing_evidence.filter((g) => GAPS.includes(g))) : [];
  for (const k of ['driver_text', 'rationale', 'missing_evidence_text', 'own_recommendation'] as const) if (k in p) out[k] = strOrNull(p[k]);
  return out;
}

function sanitizeRevealPatch(s: CaseFlowStateV2, p: FlowActionV2 & { type: 'set_reveal' }): Partial<RevealDraftV2> {
  const x = p.patch;
  const out: Partial<RevealDraftV2> = {};
  if ('validity' in x) out.validity = oneOf(x.validity, VALIDITY_VALUES);
  if ('persuasion' in x) out.persuasion = oneOf(x.persuasion, PERSUASION_VALUES);
  if ('issue_text' in x) out.issue_text = isStr(x.issue_text) ? x.issue_text : '';
  // the issue type is fixed by the statement, except for "same conclusion, different reasons"
  if ('issue_type' in x && !verdictCodeIssueType(s.reveal.verdict_code)) out.issue_type = oneOf(x.issue_type, ISSUE_TYPES);
  if ('metric_ids' in x) out.metric_ids = Array.isArray(x.metric_ids) ? uniq(x.metric_ids.filter((m) => isStr(m) && m)) : [];
  if ('resolving_evidence' in x) out.resolving_evidence = oneOf(x.resolving_evidence, GAPS);
  for (const k of ['suggested_choice', 'evidence_wanted', 'must_be_true', 'resolving_evidence_text'] as const) if (k in x) out[k] = strOrNull(x[k]);
  return out;
}

const noteText = (t: unknown): string => (isStr(t) ? t.slice(0, MAX_NOTE_LENGTH) : '');

/** The v2 reducer. Invalid actions return the state unchanged (the same object). */
export function flowReducerV2(s: CaseFlowStateV2, a: FlowActionV2): CaseFlowStateV2 {
  switch (a.type) {
    case 'start':
      return s.started_at ? s : { ...s, started_at: a.at };
    case 'set_blind': {
      if (s.blind.answered_at !== null) return s; // locked once stored
      return { ...s, blind: { ...s.blind, ...sanitizeBlindPatch(s, a.patch) } };
    }
    case 'submit_blind': {
      if (s.step !== 'blind') return s;
      // already stored (the reviewer came back to the blind step): the lock button only moves forward, nothing changes
      if (canRevealV2(s)) return { ...s, step: 'reveal' };
      if (blindErrorsV2(s).length) return s;
      const b = s.blind;
      const state = isReviewerState(b.choice);
      const blind: BlindDraftV2 = {
        ...b,
        // keep only what the chosen branch asks for
        choices: s.kind === 'sector_chain' && !state ? b.choices : [],
        missing_evidence: b.choice === INSUFFICIENT_EVIDENCE ? b.missing_evidence : [],
        missing_evidence_text: b.choice === INSUFFICIENT_EVIDENCE ? b.missing_evidence_text : null,
        own_recommendation: b.choice === NONE_OF_THESE ? b.own_recommendation : null,
        answered_at: a.at,
      };
      const outside = b.choice === OUTSIDE_EXPERTISE;
      return {
        ...s,
        blind,
        reveal: { ...s.reveal, revealed_at: a.at, verdict_code: outside && !s.reveal.verdict_code ? 'outside_expertise' : s.reveal.verdict_code },
        step: 'reveal',
        reached: laterV2(s.reached, 'reveal'),
      };
    }
    case 'next': {
      if (s.step === 'done') return s;
      if (s.step === 'blind') return canRevealV2(s) ? { ...s, step: 'reveal' } : s;
      if (stepErrorsV2(s).length) return s;
      const to = neighbourV2(s, 1) ?? 'done';
      if (POST_BLIND_V2.includes(to) && !canRevealV2(s)) return s;
      if (to === 'done') return { ...s, step: 'done', reached: 'done', finished_at: s.finished_at ?? a.at ?? null };
      return { ...s, step: to, reached: laterV2(s.reached, to) };
    }
    case 'back': {
      if (s.step === 'done') return s;
      const to = neighbourV2(s, -1);
      return to ? { ...s, step: to } : s;
    }
    case 'goto':
      return canEnterV2(s, a.step) && a.step !== s.step ? { ...s, step: a.step } : s;
    case 'set_verdict_code': {
      if (!canRevealV2(s) || s.step === 'done' || !REVIEW_VERDICT_CODES.includes(a.code)) return s;
      const fixed = verdictCodeIssueType(a.code);
      const issue_type = fixed ?? (a.code === 'same_different_reasons' ? s.reveal.issue_type : null);
      return { ...s, reveal: { ...s.reveal, verdict_code: a.code, issue_type } };
    }
    case 'set_reveal':
      if (!canRevealV2(s) || s.step === 'done') return s;
      return { ...s, reveal: { ...s.reveal, ...sanitizeRevealPatch(s, a) } };
    case 'set_product_answer': {
      if (!canRevealV2(s) || s.step === 'done') return s;
      const q = productQuestions(s.product).find((x) => x.id === a.id);
      if (!q) return s;
      const answers = { ...s.reveal.product_answers };
      if (a.value === null) delete answers[a.id];
      else if (q.options.some((o) => o.value === a.value)) answers[a.id] = a.value;
      else return s;
      return { ...s, reveal: { ...s.reveal, product_answers: answers } };
    }
    case 'set_decision_use':
      if (!canRevealV2(s) || !s.has_decision_use || s.step === 'done') return s;
      return { ...s, decision_use: { ...emptyDecisionUse(), ...s.decision_use, ...a.patch } };
    case 'set_threat':
      if (!canRevealV2(s) || !s.has_threat || s.step === 'done') return s;
      return { ...s, threat: { ...emptyThreat(), ...s.threat, ...a.patch } };
    case 'add_note': {
      if (s.step === 'done' || !isStr(a.id) || !a.id || s.notes.some((n) => n.id === a.id) || s.notes.length >= MAX_NOTES) return s;
      const text = noteText(a.text);
      if (!text.trim()) return s;
      return { ...s, notes: [...s.notes, { id: a.id, text, created_at: a.at, updated_at: a.at, converted_to: null }] };
    }
    case 'edit_note': {
      if (s.step === 'done' || !s.notes.some((n) => n.id === a.id)) return s;
      return { ...s, notes: s.notes.map((n) => (n.id === a.id ? { ...n, text: noteText(a.text), updated_at: a.at } : n)) };
    }
    case 'delete_note':
      if (s.step === 'done' || !s.notes.some((n) => n.id === a.id)) return s;
      return { ...s, notes: s.notes.filter((n) => n.id !== a.id) };
    case 'convert_note': {
      // converting a note into a typed report happens after the reveal (at the wrap-up)
      if (s.step === 'done' || !canRevealV2(s) || !s.notes.some((n) => n.id === a.id)) return s;
      const to = a.to === null ? null : oneOf(a.to, CONVERSIONS);
      if (a.to !== null && !to) return s;
      return { ...s, notes: s.notes.map((n) => (n.id === a.id ? { ...n, converted_to: to } : n)) };
    }
    case 'replace_notes':
      if (s.step === 'done') return s;
      return { ...s, notes: sanitizeNotes(a.notes) };
  }
}

/** Whether a note was written before the blind answer was stored ('before') or after the reveal ('after'). */
export function notePhase(n: Pick<ReviewerNote, 'created_at'>, s: Pick<CaseFlowStateV2, 'blind'>): 'before' | 'after' {
  return s.blind.answered_at !== null && n.created_at >= s.blind.answered_at ? 'after' : 'before';
}

// ---- the Explorer's conclusion as the prior is compared with it --------------------------------------------------

/** What a blind answer is compared with: the conclusion value; for a sector chain, the Explorer's lead stage when it has one. */
export function explorerComparisonValue(
  kind: CaseKind | string | null | undefined,
  snapshot: { conclusion?: { value?: string | null } | null; reasoning?: Pick<CaseReasoning, 'sector'> | null } | null | undefined,
): string | null {
  if (!snapshot) return null;
  if (kind === 'sector_chain') {
    // reasoning.sector carries display labels ("Final Assembly & Equipment"); blind answers are stage keys
    const lead = snapshot.reasoning?.sector?.lead_stage;
    if (isStr(lead) && lead) return stageKeyOf(lead);
  }
  const v = snapshot.conclusion?.value;
  return isStr(v) && v ? v : null;
}

/** The Explorer's stage set for a sector chain (null when the case is not one or carries none). */
export function explorerStageSet(kind: CaseKind | string | null | undefined, snapshot: { reasoning?: Pick<CaseReasoning, 'sector'> | null } | null | undefined): string[] | null {
  if (kind !== 'sector_chain') return null;
  const st = snapshot?.reasoning?.sector?.explorer_stages;
  return Array.isArray(st) ? st.filter(isStr).map(stageKeyOf) : null;
}

/** A stage key from a six-stage display label (STAGE_DISPLAY); a key or any other value passes through. */
export function stageKeyOf(x: string): string {
  return SUPPLY_STAGES.find((k) => STAGE_DISPLAY[k] === x) ?? x;
}

/** Prior: did the Explorer's conclusion differ from the blind answer? null for reviewer states or when unknown. */
export function priorDiffers(choice: string | null | undefined, explorer: string | null | undefined): boolean | null {
  if (!isStr(choice) || !choice || isReviewerState(choice) || !isStr(explorer) || !explorer) return null;
  return choice !== explorer;
}

/** Jaccard overlap of two stage sets (1 when both are empty). */
export function stageJaccard(a: readonly string[], b: readonly string[]): number {
  const A = new Set(a);
  const B = new Set(b);
  const inter = [...A].filter((x) => B.has(x)).length;
  const union = new Set([...A, ...B]).size;
  return union === 0 ? 1 : inter / union;
}

/**
 * Whether a statement agrees with the stored prior: "same conclusion ..." needs the blind answer to have matched
 * the Explorer, "different initial conclusion, but persuaded" needs it to have differed. Not enforced (the
 * reviewer's statement is stored as given); the admin flags inconsistent pairs.
 */
export function verdictCodeConsistency(code: ReviewVerdictCode | null | undefined, prior: boolean | null | undefined): 'consistent' | 'inconsistent' | 'unknown' {
  if (!code || prior === null || prior === undefined) return 'unknown';
  if ((code === 'same_same' || code === 'same_different_reasons') && prior) return 'inconsistent';
  if (code === 'persuaded' && !prior) return 'inconsistent';
  return 'consistent';
}

// ---- payload -------------------------------------------------------------------------------------------------

const cut = (x: string | null | undefined): string | null => {
  const t = nn(x);
  return t === null ? null : t.slice(0, MAX_TEXT_LENGTH);
};

export interface CaseForPayloadV2 {
  case_id: string;
  snapshot_sha256: string;
  kind?: CaseKind;
  /** The snapshot (its conclusion and reasoning give prior_differs); without it prior_differs is null. */
  snapshot?: { conclusion?: { value?: string | null } | null; reasoning?: CaseReasoning | null } | null;
}

/** The blind answer as stored in the payload. */
export function blindAnswerOfV2(s: CaseFlowStateV2): BlindAnswerV2 {
  const b = s.blind;
  if (b.legacy) {
    return { choice: b.choice as string, confidence: b.legacy.confidence, factors: [...b.legacy.factors], rationale: cut(b.rationale), answered_at: b.answered_at as string };
  }
  const state = isReviewerState(b.choice);
  const out: BlindAnswerV2 = {
    choice: b.choice as string,
    confidence: b.confidence_level ? CONFIDENCE_LEVEL_SCORE[b.confidence_level] : null,
    factors: b.driver ? [b.driver] : [],
    rationale: cut(b.rationale),
    answered_at: b.answered_at as string,
    confidence_level: b.confidence_level,
    driver: b.driver,
    driver_text: cut(b.driver_text),
    ux_version: 2,
  };
  if (s.kind === 'sector_chain' && !state) out.choices = b.choices.length ? [...b.choices] : [b.choice as string];
  if (b.choice === INSUFFICIENT_EVIDENCE) {
    out.missing_evidence = [...b.missing_evidence];
    out.missing_evidence_text = cut(b.missing_evidence_text);
  }
  if (b.choice === NONE_OF_THESE) out.own_recommendation = cut(b.own_recommendation);
  return out;
}

/**
 * The case_review payload of a v2 review. Throws when the blind answer is not stored, no statement is chosen, or
 * the verdict step is incomplete: a payload is never half-built. `verdict` is derived (REVIEW_VERDICT_LEGACY) so
 * v1 aggregation and exports keep working; prior_differs is computed from the snapshot when given.
 */
export function toSubmissionPayloadV2(s: CaseFlowStateV2, c: CaseForPayloadV2): CaseReviewPayload {
  if (s.case_id !== c.case_id) throw new Error(`flow state is for ${s.case_id}, not ${c.case_id}`);
  if (!canRevealV2(s)) throw new Error('the blind answer is not stored');
  const r = s.reveal;
  if (!r.verdict_code || r.revealed_at === null) throw new Error('no verdict');
  const ve = verdictErrorsV2(s);
  if (ve.length) throw new Error(ve.join(' '));
  const blind = blindAnswerOfV2(s);
  const code = r.verdict_code;
  const issue = verdictCodeNeedsIssue(code) && r.issue_type ? { text: (cut(r.issue_text) as string), type: r.issue_type, ...(r.metric_ids.length ? { metric_ids: [...r.metric_ids] } : {}) } : null;
  const rt = { must_be_true: cut(r.must_be_true), resolving_evidence: r.resolving_evidence, resolving_evidence_text: cut(r.resolving_evidence_text) };
  const qs = productQuestions(s.product);
  const answers: Record<string, string> = {};
  for (const q of qs) {
    const v = r.product_answers[q.id];
    if (v && q.options.some((o) => o.value === v)) answers[q.id] = v;
  }
  const kind = c.kind ?? s.kind;
  const reveal: RevealResponse = {
    revealed_at: r.revealed_at,
    verdict: REVIEW_VERDICT_LEGACY[code],
    main_issue: issue,
    suggested_choice: cut(r.suggested_choice),
    evidence_wanted: cut(r.evidence_wanted),
    verdict_code: code,
    prior_differs: priorDiffers(blind.choice, explorerComparisonValue(kind, c.snapshot)),
    validity: r.validity,
    persuasion: r.persuasion,
    red_team: rt.must_be_true || rt.resolving_evidence || rt.resolving_evidence_text ? rt : null,
    ...(qs.length ? { product_answers: answers } : {}),
  };
  const spent = s.started_at && s.finished_at ? Math.round((Date.parse(s.finished_at) - Date.parse(s.started_at)) / 1000) : null;
  const notes = s.notes
    .filter((n) => n.text.trim())
    .map((n) => ({ id: n.id, text: n.text.trim(), created_at: n.created_at, updated_at: n.updated_at, converted_to: n.converted_to ?? null }));
  return {
    case_id: c.case_id,
    case_snapshot_sha256: c.snapshot_sha256,
    assignment_reason: s.assignment_reason,
    expertise_match: s.expertise_match,
    blind,
    reveal,
    outside_expertise: blind.choice === OUTSIDE_EXPERTISE || blind.choice === DONT_KNOW || code === 'outside_expertise',
    time_spent_s: spent !== null && Number.isFinite(spent) && spent >= 0 ? spent : null,
    notes,
    review_mode: s.review_mode,
  };
}

// ---- restore and migration ---------------------------------------------------------------------------------

const VERDICTS: readonly Verdict[] = [1, 2, 3, 4, 5, 6, 7];
const ASSIGNMENT_REASONS: readonly AssignmentReason[] = ['assigned', 'anchor', 'outside', 'self_selected'];
const MATCHES: readonly ExpertiseMatch[] = ['in', 'adjacent', 'outside'];
const conf15 = (x: unknown): 1 | 2 | 3 | 4 | 5 | null => (x === 1 || x === 2 || x === 3 || x === 4 || x === 5 ? x : null);

function sanitizeDecisionUse(x: unknown): DecisionUseDraft | null {
  return x && typeof x === 'object' ? { ...emptyDecisionUse(), ...(x as Partial<DecisionUseDraft>) } : null;
}
function sanitizeThreat(x: unknown): ThreatDraft | null {
  if (!x || typeof x !== 'object') return null;
  const t = { ...emptyThreat(), ...(x as Partial<ThreatDraft>) };
  return { ...t, missing_constraints: Array.isArray(t.missing_constraints) ? [...t.missing_constraints] : [] };
}

/** The lock check every restore ends with: a post-blind step without a valid stored blind answer goes back to blind, unlocked, with nothing revealed. */
function enforceBlindFirst(st: CaseFlowStateV2): CaseFlowStateV2 {
  if (canRevealV2(st)) return st;
  const tampered = POST_BLIND_V2.includes(st.step) || POST_BLIND_V2.includes(st.reached) || st.blind.answered_at !== null || st.reveal.revealed_at !== null;
  if (!tampered) return st;
  return {
    ...st,
    step: st.step === 'brief' ? 'brief' : 'blind',
    reached: 'blind',
    blind: { ...st.blind, answered_at: null, legacy: null },
    reveal: emptyRevealV2(),
    decision_use: null,
    threat: null,
    finished_at: null,
  };
}

function restoreV2Draft(x: Record<string, unknown>, c: CaseForFlowV2, o: InitV2Options): CaseFlowStateV2 | null {
  if (!FLOW_STEPS_V2.includes(x.step as FlowStepV2) || !x.blind || typeof x.blind !== 'object' || !x.reveal || typeof x.reveal !== 'object') return null;
  const base = initialFlowStateV2(c, { ...o, review_mode: oneOf(x.review_mode, ['decision', 'technical'] as const) ?? o.review_mode, notes: x.notes ?? o.notes });
  const b = x.blind as Record<string, unknown>;
  const r = x.reveal as Record<string, unknown>;
  const legacyRaw = b.legacy as { confidence?: unknown; factors?: unknown } | null | undefined;
  const legacy = legacyRaw && typeof legacyRaw === 'object' ? { confidence: conf15(legacyRaw.confidence), factors: Array.isArray(legacyRaw.factors) ? legacyRaw.factors.filter(isStr) : [] } : null;
  const blindPatch = sanitizeBlindPatch(base, b as Partial<BlindDraftV2>);
  const st: CaseFlowStateV2 = {
    ...base,
    step: x.step as FlowStepV2,
    reached: FLOW_STEPS_V2.includes(x.reached as FlowStepV2) ? (x.reached as FlowStepV2) : (x.step as FlowStepV2),
    assignment_reason: oneOf(x.assignment_reason, ASSIGNMENT_REASONS) ?? base.assignment_reason,
    expertise_match: oneOf(x.expertise_match, MATCHES) ?? base.expertise_match,
    blind: {
      ...base.blind,
      ...blindPatch,
      // a stored legacy choice (DONT_KNOW) is kept verbatim
      choice: legacy && isStr(b.choice) ? b.choice : (blindPatch.choice ?? null),
      answered_at: strOrNull(b.answered_at),
      legacy,
    },
    reveal: {
      ...base.reveal,
      revealed_at: strOrNull(r.revealed_at),
      verdict_code: oneOf(r.verdict_code, REVIEW_VERDICT_CODES),
      issue_type: oneOf(r.issue_type, ISSUE_TYPES),
      product_answers: {},
      legacy_verdict: VERDICTS.includes(r.legacy_verdict as Verdict) ? (r.legacy_verdict as Verdict) : null,
    },
    decision_use: base.has_decision_use ? sanitizeDecisionUse(x.decision_use) : null,
    threat: base.has_threat ? sanitizeThreat(x.threat) : null,
    started_at: strOrNull(x.started_at),
    finished_at: strOrNull(x.finished_at),
    migrated_from: x.migrated_from === 1 ? 1 : null,
  };
  st.reveal = { ...st.reveal, ...sanitizeRevealPatch({ ...st, reveal: { ...st.reveal, verdict_code: null } }, { type: 'set_reveal', patch: r as Partial<RevealDraftV2> }) };
  const pa = r.product_answers && typeof r.product_answers === 'object' ? (r.product_answers as Record<string, unknown>) : {};
  for (const q of productQuestions(st.product)) {
    const v = pa[q.id];
    if (isStr(v) && q.options.some((op) => op.value === v)) st.reveal.product_answers[q.id] = v;
  }
  // the statement fixes the issue type (a hand-edited type does not survive)
  const fixed = verdictCodeIssueType(st.reveal.verdict_code);
  if (fixed) st.reveal.issue_type = fixed;
  else if (st.reveal.verdict_code !== 'same_different_reasons') st.reveal.issue_type = null;
  if (st.step === 'done' && st.finished_at === null) st.step = 'wrap';
  return enforceBlindFirst(st);
}

/**
 * A v1 draft as a v2 state. Before its blind answer was stored: a fresh v2 state (assignment, start time and the
 * answer typed so far are kept; DONT_KNOW becomes OUTSIDE_EXPERTISE), so the reviewer answers the v2 blind
 * question. After: the stored v1 answer is carried verbatim and locked (blind.legacy holds its 1-5 confidence and
 * factors; no confidence level or driver is invented), and the reviewer gives the v2 statement at the verdict
 * step (the v1 verdict is kept as legacy_verdict for reference). A finished v1 draft (already submitted) stays done.
 */
export function migrateV1Draft(v1: CaseFlowState, c: CaseForFlowV2, o: InitV2Options = {}): CaseFlowStateV2 {
  const base = initialFlowStateV2(c, { ...o, reason: oneOf(v1.assignment_reason, ASSIGNMENT_REASONS) ?? o.reason, match: oneOf(v1.expertise_match, MATCHES) ?? o.match });
  const started_at = strOrNull(v1.started_at);
  if (!canReveal(v1)) {
    const choice = isStr(v1.blind?.choice) && v1.blind.choice ? (v1.blind.choice === DONT_KNOW ? OUTSIDE_EXPERTISE : v1.blind.choice) : null;
    const st: CaseFlowStateV2 = {
      ...base,
      step: v1.step === 'blind' ? 'blind' : 'brief',
      reached: v1.step === 'blind' || v1.reached === 'blind' ? 'blind' : 'brief',
      blind: { ...base.blind, choice: choice && (isReviewerState(choice) || !base.options.length || base.options.includes(choice)) ? choice : null, rationale: strOrNull(v1.blind?.rationale) },
      started_at,
      migrated_from: 1,
    };
    return st;
  }
  const r = v1.reveal;
  const stepMap: Partial<Record<FlowStep, FlowStepV2>> = { reveal: 'reveal', verdict: 'verdict', issue: 'verdict', extras: 'verdict', done: 'done' };
  const finished = v1.step === 'done' && isStr(v1.finished_at);
  const step: FlowStepV2 = finished ? 'done' : (stepMap[v1.step] ?? 'reveal');
  return {
    ...base,
    step,
    reached: step,
    blind: {
      ...base.blind,
      choice: v1.blind.choice,
      rationale: strOrNull(v1.blind.rationale),
      answered_at: v1.blind.answered_at,
      legacy: { confidence: conf15(v1.blind.confidence), factors: Array.isArray(v1.blind.factors) ? v1.blind.factors.filter(isStr) : [] },
    },
    reveal: {
      ...base.reveal,
      revealed_at: strOrNull(r?.revealed_at) ?? v1.blind.answered_at,
      verdict_code: v1.blind.choice === DONT_KNOW ? 'outside_expertise' : null,
      issue_text: isStr(r?.issue_text) ? r.issue_text : '',
      metric_ids: Array.isArray(r?.metric_ids) ? r.metric_ids.filter(isStr) : [],
      suggested_choice: strOrNull(r?.suggested_choice),
      evidence_wanted: strOrNull(r?.evidence_wanted),
      legacy_verdict: VERDICTS.includes(r?.verdict as Verdict) ? (r.verdict as Verdict) : null,
    },
    decision_use: base.has_decision_use ? sanitizeDecisionUse(v1.decision_use) : null,
    threat: base.has_threat ? sanitizeThreat(v1.threat) : null,
    started_at,
    finished_at: finished ? (v1.finished_at as string) : null,
    migrated_from: 1,
  };
}

/**
 * A stored draft (v2, or v1 to migrate), checked: a v2 state for this case, else null (start over). Every field is
 * re-validated, and a draft whose step is past the blind answer without a valid stored answer (edited by hand) is
 * sent back to the blind step with nothing revealed.
 */
export function restoreFlowStateV2(x: unknown, c: CaseForFlowV2, o: InitV2Options = {}): CaseFlowStateV2 | null {
  if (typeof x !== 'object' || x === null) return null;
  const d = x as Record<string, unknown>;
  if (d.case_id !== c.case_id) return null;
  if (d.version === 2) return restoreV2Draft(d, c, o);
  if (d.version === 1) {
    const v1 = restoreFlowState(x, c.case_id);
    return v1 ? enforceBlindFirst(migrateV1Draft(v1, c, o)) : null;
  }
  return null;
}
