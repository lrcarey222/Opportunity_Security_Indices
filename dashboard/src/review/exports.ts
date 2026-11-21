// The review exports (docs/QC_SYSTEM_DESIGN.md 6.5): pure row builders with fixed column orders and stable
// IDs for qc_review_responses, qc_case_results, qc_method_feedback, qc_data_issues, qc_framework_answers,
// qc_decision_use, qc_threat_reviews and qc_priority_fixes, and csvFiles() that writes them as CSV text
// (lib/csv.ts rowsToCsv; no BOM, so R reads the first column name cleanly). Reviewers appear as their
// pseudonymous code; names only with includeNames, and never an anonymous reviewer's. Rows join on
// submission_id, case_id, card_id, scenario_id and reviewer_code. Pure: no IO.

import {
  DONT_KNOW,
  ISSUE_TYPE_LABEL,
  VERDICT_LABEL,
  type Adjudication,
  type CaseReviewPayload,
  type DataIssuePayload,
  type FrameworkScenario,
  type MethodCard,
  type ReviewCase,
  type Submission,
  type SubmissionKind,
} from './model.ts';
import { rowsToCsv } from '../lib/csv.ts';
import { caseAgreement, dedupeSubmissions, isFrameworkMismatch, isLowActionability, isOutsideAnswer, latestAdjudications, priorityFixes, reportMatch } from './aggregate.ts';
import { exportableName, pseudonymReviewerCode } from './privacy.ts';
import { DEFAULT_MIN_CONFIDENCE, isExpertIn, normaliseCountryTag, normaliseSectorTag, normaliseStageTag } from './tags.ts';

export type Row = Record<string, string | number | boolean | null>;

export interface ExportOptions {
  includeNames?: boolean;
  cards?: readonly MethodCard[];
  scenarios?: readonly FrameworkScenario[];
}

const list = (xs: readonly string[] | null | undefined): string => (xs ?? []).join(';');
const dist = (d: Record<string, number>): string =>
  Object.entries(d)
    .sort((a, b) => b[1] - a[1] || (a[0] < b[0] ? -1 : 1))
    .map(([k, v]) => `${k}:${v}`)
    .join(';');
const nameCols = (o: ExportOptions): string[] => (o.includeNames ? ['reviewer_name'] : []);
const reviewer = (s: Submission, o: ExportOptions): Row => ({ reviewer_code: pseudonymReviewerCode(s.reviewer.reviewer_id), ...(o.includeNames ? { reviewer_name: exportableName(s, o) } : {}) });
const ofKind = <K extends SubmissionKind>(subs: readonly Submission[], k: K) => subs.filter((s): s is Submission<K> => s.kind === k && !!s.payload);

// ---- qc_review_responses ------------------------------------------------------------------------------------

export const REVIEW_RESPONSE_COLUMNS = [
  'submission_id', 'campaign_id', 'case_id', 'case_kind', 'case_title', 'anchor', 'reviewer_code', 'expertise_match', 'assignment_reason', 'outside_expertise',
  'blind_choice', 'blind_dont_know', 'blind_confidence', 'blind_factors', 'blind_rationale', 'model_choice', 'model_label', 'blind_matches_model', 'verdict',
  'verdict_label', 'persuaded', 'rejecting', 'issue_type', 'issue_type_label', 'main_issue', 'issue_metric_ids', 'suggested_choice', 'evidence_wanted',
  'snapshot_sha256_reviewed', 'snapshot_sha256_current', 'snapshot_matches', 'data_release', 'app_commit', 'answered_at', 'revealed_at', 'created_at', 'submitted_at',
  'time_spent_s',
] as const;

/** One row per case review (every submission, resubmissions included; qc_case_results counts the latest). */
export function reviewResponseRows(subs: readonly Submission[], cases: readonly ReviewCase[], o: ExportOptions = {}): Row[] {
  const byId = new Map(cases.map((c) => [c.case_id, c]));
  return ofKind(dedupeSubmissions(subs), 'case_review').map((s) => {
    const p: CaseReviewPayload = s.payload;
    const c = byId.get(p.case_id) ?? null;
    const model = c?.snapshot?.conclusion?.value ?? null;
    const dk = p.blind?.choice === DONT_KNOW;
    const v = p.reveal?.verdict ?? null;
    const it = p.reveal?.main_issue?.type ?? null;
    return {
      submission_id: s.submission_id,
      campaign_id: s.campaign_id,
      case_id: p.case_id,
      case_kind: c?.kind ?? null,
      case_title: c?.title ?? null,
      anchor: c ? c.anchor : null,
      ...reviewer(s, o),
      expertise_match: p.expertise_match ?? null,
      assignment_reason: p.assignment_reason ?? null,
      outside_expertise: p.outside_expertise === true || isOutsideAnswer(p),
      blind_choice: p.blind?.choice ?? null,
      blind_dont_know: dk,
      blind_confidence: p.blind?.confidence ?? null,
      blind_factors: list(p.blind?.factors),
      blind_rationale: p.blind?.rationale ?? null,
      model_choice: model,
      model_label: c?.snapshot?.conclusion?.label ?? null,
      blind_matches_model: model === null || dk ? null : p.blind?.choice === model,
      verdict: v,
      verdict_label: v ? VERDICT_LABEL[v] : null,
      persuaded: v === 2,
      rejecting: v === 4 || v === 5 || v === 6,
      issue_type: it,
      issue_type_label: it ? ISSUE_TYPE_LABEL[it] : null,
      main_issue: p.reveal?.main_issue?.text ?? null,
      issue_metric_ids: list(p.reveal?.main_issue?.metric_ids),
      suggested_choice: p.reveal?.suggested_choice ?? null,
      evidence_wanted: p.reveal?.evidence_wanted ?? null,
      snapshot_sha256_reviewed: p.case_snapshot_sha256 ?? null,
      snapshot_sha256_current: c?.snapshot_sha256 ?? null,
      snapshot_matches: c ? p.case_snapshot_sha256 === c.snapshot_sha256 : null,
      data_release: s.data_release,
      app_commit: s.app_commit,
      answered_at: p.blind?.answered_at ?? null,
      revealed_at: p.reveal?.revealed_at ?? null,
      created_at: s.created_at,
      submitted_at: s.submitted_at,
      time_spent_s: p.time_spent_s ?? null,
    };
  });
}

// ---- qc_case_results ----------------------------------------------------------------------------------------

export const CASE_RESULT_COLUMNS = [
  'case_id', 'case_kind', 'case_title', 'anchor', 'model_choice', 'model_label', 'n_reviews', 'n_interpretable', 'n_dont_know', 'n_verdict7', 'n_outside',
  'blind_distribution', 'modal_blind', 'modal_share', 'n_matching_model', 'share_matching_model', 'n_persuaded', 'share_persuaded', 'n_rejecting', 'share_rejecting',
  'verdict_counts', 'issue_types', 'suggested_choices', 'n_snapshot_mismatch', 'too_few_reviews', 'summary_label',
] as const;

/** One row per case (latest review per reviewer); shares are empty when there are too few interpretable reviews. */
export function caseResultRows(subs: readonly Submission[], cases: readonly ReviewCase[]): Row[] {
  return caseAgreement(subs, cases).map((a) => ({
    case_id: a.case_id,
    case_kind: a.kind,
    case_title: a.title,
    anchor: a.anchor,
    model_choice: a.model_choice,
    model_label: a.model_label,
    n_reviews: a.n_reviews,
    n_interpretable: a.n_interpretable,
    n_dont_know: a.n_dont_know,
    n_verdict7: a.n_verdict7,
    n_outside: a.n_outside,
    blind_distribution: dist(a.blind_distribution),
    modal_blind: a.modal.join('|') || null,
    modal_share: a.modal_share,
    n_matching_model: a.n_matching_model,
    share_matching_model: a.share_matching_model,
    n_persuaded: a.n_persuaded,
    share_persuaded: a.share_persuaded,
    n_rejecting: a.n_rejecting,
    share_rejecting: a.share_rejecting,
    verdict_counts: Object.entries(a.verdict_counts)
      .filter(([, n]) => n > 0)
      .map(([v, n]) => `${v}:${n}`)
      .join(';'),
    issue_types: dist(a.issue_types as Record<string, number>),
    suggested_choices: dist(a.suggested_choices),
    n_snapshot_mismatch: a.n_snapshot_mismatch,
    too_few_reviews: a.too_few,
    summary_label: a.summary_label,
  }));
}

// ---- qc_method_feedback -------------------------------------------------------------------------------------

export const METHOD_FEEDBACK_COLUMNS = [
  'submission_id', 'campaign_id', 'card_id', 'card_title', 'card_area', 'reviewer_code', 'card_relevance', 'outside_expertise', 'stance', 'materiality',
  'bias_direction', 'alternative', 'affected_outputs', 'comment', 'data_release', 'app_commit', 'created_at', 'submitted_at',
] as const;

export function methodFeedbackRows(subs: readonly Submission[], o: ExportOptions = {}): Row[] {
  const cards = new Map((o.cards ?? []).map((c) => [c.card_id, c]));
  return ofKind(dedupeSubmissions(subs), 'method_feedback').map((s) => {
    const p = s.payload;
    const card = cards.get(p.card_id) ?? null;
    const ex = s.reviewer.expertise;
    return {
      submission_id: s.submission_id,
      campaign_id: s.campaign_id,
      card_id: p.card_id,
      card_title: card?.title ?? null,
      card_area: card?.area ?? null,
      ...reviewer(s, o),
      card_relevance: card && ex ? (card.method_tags.some((t) => isExpertIn(ex, 'method', t, DEFAULT_MIN_CONFIDENCE)) ? 'in' : 'outside') : null,
      outside_expertise: p.outside_expertise === true,
      stance: p.stance,
      materiality: p.materiality ?? null,
      bias_direction: p.bias_direction ?? null,
      alternative: p.alternative ?? null,
      affected_outputs: p.affected_outputs ?? null,
      comment: p.comment ?? null,
      data_release: s.data_release,
      app_commit: s.app_commit,
      created_at: s.created_at,
      submitted_at: s.submitted_at,
    };
  });
}

// ---- qc_data_issues -----------------------------------------------------------------------------------------

export const DATA_ISSUE_COLUMNS = [
  'submission_id', 'kind', 'campaign_id', 'reviewer_code', 'expertise_match', 'severity', 'break_type', 'what_is_wrong', 'corrected_value', 'citation', 'metric_id',
  'metric_label', 'metric_value', 'metric_display', 'metric_country', 'metric_tech', 'metric_stage', 'metric_source', 'metric_source_year', 'metric_transform',
  'page_url', 'page_route', 'page_view', 'page_country', 'page_tech', 'page_stage', 'page_node', 'page_layer', 'page_pair', 'page_sid', 'page_scenario',
  'displayed_result', 'page_metric_ids', 'data_release', 'release_note', 'app_commit', 'captured_at', 'adjudication_status', 'adjudication_issue_type', 'adjudication_confidence_real',
  'adjudication_affected_outputs', 'adjudication_note', 'adjudicated_by', 'adjudicated_at', 'resolution_commit', 'duplicate_of', 'priority_group', 'priority_score',
  'created_at', 'submitted_at',
] as const;

/** data_issue and break_model submissions with their page context, adjudication status and priority group. */
export function dataIssueRows(subs: readonly Submission[], cases: readonly ReviewCase[], adjudications: readonly Adjudication[] = [], o: ExportOptions = {}): Row[] {
  const adj = latestAdjudications(adjudications);
  const pri = priorityFixes(subs, cases, adjudications, { cards: o.cards });
  const groupOf = new Map<string, { id: string; score: number }>();
  for (const p of pri) for (const id of p.submission_ids) groupOf.set(id, { id: p.group_id, score: p.score });
  return dedupeSubmissions(subs)
    .filter((s): s is Submission<'data_issue'> | Submission<'break_model'> => (s.kind === 'data_issue' || s.kind === 'break_model') && !!s.payload)
    .map((s) => {
      const p: DataIssuePayload = s.payload;
      const pc = s.page_context ?? null;
      const a = adj.get(s.submission_id) ?? null;
      const m = p.metric ?? null;
      const at = {
        country: normaliseCountryTag(m?.country ?? pc?.country ?? null),
        tech: normaliseSectorTag(m?.tech ?? pc?.tech ?? null),
        stage: normaliseStageTag(m?.stage ?? pc?.stage ?? null),
      };
      const g = groupOf.get(s.submission_id);
      return {
        submission_id: s.submission_id,
        kind: s.kind,
        campaign_id: s.campaign_id,
        ...reviewer(s, o),
        expertise_match: reportMatch(s, at).match,
        severity: p.severity,
        break_type: p.break_type ?? null,
        what_is_wrong: p.what_is_wrong,
        corrected_value: p.corrected_value ?? null,
        citation: p.citation ?? null,
        metric_id: m?.id ?? null,
        metric_label: m?.label ?? null,
        metric_value: m?.value ?? null,
        metric_display: m?.display ?? null,
        metric_country: m?.country ?? null,
        metric_tech: m?.tech ?? null,
        metric_stage: m?.stage ?? null,
        metric_source: m?.source ?? null,
        metric_source_year: m?.source_year ?? null,
        metric_transform: m?.transform ?? null,
        page_url: pc?.url ?? null,
        page_route: pc?.route ?? null,
        page_view: pc?.view ?? null,
        page_country: pc?.country ?? null,
        page_tech: pc?.tech ?? null,
        page_stage: pc?.stage ?? null,
        page_node: pc?.node ?? null,
        page_layer: pc?.layer ?? null,
        page_pair: pc?.pair ?? null,
        page_sid: pc?.sid ?? null,
        page_scenario: pc?.scenario ?? null,
        displayed_result: pc?.displayed_result ?? null,
        page_metric_ids: list(pc?.metric_ids),
        data_release: s.data_release ?? pc?.data_release ?? null,
        release_note: pc?.release_note ?? null,
        app_commit: s.app_commit,
        captured_at: pc?.captured_at ?? null,
        adjudication_status: a?.status ?? 'open',
        adjudication_issue_type: a?.issue_type ?? null,
        adjudication_confidence_real: a?.confidence_real ?? null,
        adjudication_affected_outputs: a ? (Array.isArray(a.affected_outputs) ? a.affected_outputs.join(';') : (a.affected_outputs ?? null)) : null,
        adjudication_note: a?.note ?? null,
        adjudicated_by: a?.decided_by ?? null,
        adjudicated_at: a?.decided_at ?? null,
        resolution_commit: a?.resolution_commit ?? null,
        duplicate_of: a?.duplicate_of ?? null,
        priority_group: g?.id ?? null,
        priority_score: g ? Number(g.score.toFixed(6)) : null,
        created_at: s.created_at,
        submitted_at: s.submitted_at,
      };
    });
}

// ---- qc_framework_answers -----------------------------------------------------------------------------------

export const FRAMEWORK_ANSWER_COLUMNS = [
  'submission_id', 'campaign_id', 'scenario_id', 'concept', 'reviewer_code', 'answer', 'intended_answer', 'intended_answer_current', 'correct', 'mismatch',
  'correct_reported', 'confidence', 'confusing_terms', 'comment', 'data_release', 'app_commit', 'created_at', 'submitted_at',
] as const;

/** Correctness is recomputed against the scenario's current intended answer and the copy in the payload. */
export function frameworkAnswerRows(subs: readonly Submission[], o: ExportOptions = {}): Row[] {
  const sc = new Map((o.scenarios ?? []).map((x) => [x.scenario_id, x]));
  return ofKind(dedupeSubmissions(subs), 'framework_answer').map((s) => {
    const p = s.payload;
    const cur = sc.get(p.scenario_id) ?? null;
    const dk = p.answer === DONT_KNOW;
    return {
      submission_id: s.submission_id,
      campaign_id: s.campaign_id,
      scenario_id: p.scenario_id,
      concept: cur?.concept ?? null,
      ...reviewer(s, o),
      answer: p.answer,
      intended_answer: p.intended_answer,
      intended_answer_current: cur?.intended_answer ?? null,
      correct: dk ? null : p.answer === p.intended_answer,
      mismatch: isFrameworkMismatch(p),
      correct_reported: typeof p.correct === 'boolean' ? p.correct : null,
      confidence: p.confidence ?? null,
      confusing_terms: list(p.confusing_terms),
      comment: p.comment ?? null,
      data_release: s.data_release,
      app_commit: s.app_commit,
      created_at: s.created_at,
      submitted_at: s.submitted_at,
    };
  });
}

// ---- qc_decision_use and qc_threat_reviews ------------------------------------------------------------------

export const DECISION_USE_COLUMNS = [
  'submission_id', 'campaign_id', 'case_id', 'case_title', 'reviewer_code', 'actionable', 'identifies_problem', 'separates_diagnosis_from_prescription',
  'right_geography', 'low_actionability', 'would_do_differently', 'evidence_required', 'improvement', 'decision_use', 'data_release', 'app_commit', 'created_at',
  'submitted_at',
] as const;

export function decisionUseRows(subs: readonly Submission[], cases: readonly ReviewCase[], o: ExportOptions = {}): Row[] {
  const titles = new Map(cases.map((c) => [c.case_id, c.title]));
  return ofKind(dedupeSubmissions(subs), 'decision_use').map((s) => {
    const p = s.payload;
    return {
      submission_id: s.submission_id,
      campaign_id: s.campaign_id,
      case_id: p.case_id,
      case_title: titles.get(p.case_id) ?? null,
      ...reviewer(s, o),
      actionable: p.actionable ?? null,
      identifies_problem: p.identifies_problem ?? null,
      separates_diagnosis_from_prescription: p.separates_diagnosis_from_prescription ?? null,
      right_geography: p.right_geography ?? null,
      low_actionability: isLowActionability(p),
      would_do_differently: p.would_do_differently ?? null,
      evidence_required: p.evidence_required ?? null,
      improvement: p.improvement ?? null,
      decision_use: p.decision_use ?? null,
      data_release: s.data_release,
      app_commit: s.app_commit,
      created_at: s.created_at,
      submitted_at: s.submitted_at,
    };
  });
}

export const THREAT_REVIEW_COLUMNS = [
  'submission_id', 'campaign_id', 'case_id', 'case_title', 'reviewer_code', 'threat_snapshot_sha256', 'reduces_threat', 'scale_sufficient', 'timing_plausible',
  'commercially_plausible', 'missing_constraints', 'success_looks_like', 'stop_point', 'data_release', 'app_commit', 'created_at', 'submitted_at',
] as const;

export function threatReviewRows(subs: readonly Submission[], cases: readonly ReviewCase[], o: ExportOptions = {}): Row[] {
  const titles = new Map(cases.map((c) => [c.case_id, c.title]));
  return ofKind(dedupeSubmissions(subs), 'threat_review').map((s) => {
    const p = s.payload;
    return {
      submission_id: s.submission_id,
      campaign_id: s.campaign_id,
      case_id: p.case_id,
      case_title: titles.get(p.case_id) ?? null,
      ...reviewer(s, o),
      threat_snapshot_sha256: p.threat_snapshot_sha256 ?? null,
      reduces_threat: p.reduces_threat ?? null,
      scale_sufficient: p.scale_sufficient ?? null,
      timing_plausible: p.timing_plausible ?? null,
      commercially_plausible: p.commercially_plausible ?? null,
      missing_constraints: list(p.missing_constraints),
      success_looks_like: p.success_looks_like ?? null,
      stop_point: p.stop_point ?? null,
      data_release: s.data_release,
      app_commit: s.app_commit,
      created_at: s.created_at,
      submitted_at: s.submitted_at,
    };
  });
}

// ---- qc_priority_fixes --------------------------------------------------------------------------------------

export const PRIORITY_FIX_COLUMNS = [
  'rank', 'group_id', 'source', 'title', 'issue_type', 'severity', 'materiality', 'confidence_real', 'confidence_basis', 'affected_outputs', 'breadth', 'score',
  'n_reports', 'n_reviewers', 'adjudication_status', 'unresolved', 'case_id', 'metric_id', 'country', 'tech', 'stage', 'submission_ids',
] as const;

export function priorityFixRows(subs: readonly Submission[], cases: readonly ReviewCase[], adjudications: readonly Adjudication[] = [], o: ExportOptions = {}): Row[] {
  return priorityFixes(subs, cases, adjudications, { cards: o.cards }).map((p, i) => ({
    rank: i + 1,
    group_id: p.group_id,
    source: p.source,
    title: p.title,
    issue_type: p.issue_type,
    severity: p.severity,
    materiality: p.materiality,
    confidence_real: Number(p.confidence_real.toFixed(6)),
    confidence_basis: p.confidence_basis,
    affected_outputs: p.affected_outputs,
    breadth: Number(p.breadth.toFixed(6)),
    score: Number(p.score.toFixed(6)),
    n_reports: p.n_reports,
    n_reviewers: p.n_reviewers,
    adjudication_status: p.adjudication_status,
    unresolved: p.unresolved,
    case_id: p.case_id,
    metric_id: p.metric_id,
    country: p.country,
    tech: p.tech,
    stage: p.stage,
    submission_ids: p.submission_ids.join(';'),
  }));
}

// ---- files --------------------------------------------------------------------------------------------------

export interface CsvFile {
  name: string;
  text: string;
  rows: number;
}

const withNames = (cols: readonly string[], o: ExportOptions): string[] => {
  const out = [...cols];
  const i = out.indexOf('reviewer_code');
  if (i >= 0) out.splice(i + 1, 0, ...nameCols(o));
  return out;
};

/**
 * Spreadsheet formula neutralisation: reviewer free text arrives from outside NEIS (a packet, the endpoint),
 * so a text cell starting with =, +, @, a tab / carriage return, or '-' not followed by a digit is prefixed
 * with an apostrophe before it can run as a formula when the CSV is opened in a spreadsheet. Numbers, IDs and
 * negative numbers written as text ("-0.3") are untouched.
 */
export function neutraliseFormula(v: string): string {
  return /^[=+@\t\r]/.test(v) || /^-(?![\d.])/.test(v) ? `'${v}` : v;
}
const safeRow = (r: Row): Row => {
  const out: Row = {};
  for (const [k, v] of Object.entries(r)) out[k] = typeof v === 'string' ? neutraliseFormula(v) : v;
  return out;
};

/** Every export as CSV text, in a fixed order with fixed columns (a file with no rows still has its header). */
export function csvFiles(subs: readonly Submission[], cases: readonly ReviewCase[], adjudications: readonly Adjudication[] = [], o: ExportOptions = {}): CsvFile[] {
  const files: [string, readonly string[], Row[]][] = [
    ['qc_review_responses.csv', withNames(REVIEW_RESPONSE_COLUMNS, o), reviewResponseRows(subs, cases, o)],
    ['qc_case_results.csv', CASE_RESULT_COLUMNS, caseResultRows(subs, cases)],
    ['qc_method_feedback.csv', withNames(METHOD_FEEDBACK_COLUMNS, o), methodFeedbackRows(subs, o)],
    ['qc_data_issues.csv', withNames(DATA_ISSUE_COLUMNS, o), dataIssueRows(subs, cases, adjudications, o)],
    ['qc_framework_answers.csv', withNames(FRAMEWORK_ANSWER_COLUMNS, o), frameworkAnswerRows(subs, o)],
    ['qc_decision_use.csv', withNames(DECISION_USE_COLUMNS, o), decisionUseRows(subs, cases, o)],
    ['qc_threat_reviews.csv', withNames(THREAT_REVIEW_COLUMNS, o), threatReviewRows(subs, cases, o)],
    ['qc_priority_fixes.csv', PRIORITY_FIX_COLUMNS, priorityFixRows(subs, cases, adjudications, o)],
  ];
  return files.map(([name, cols, rows]) => ({ name, text: rowsToCsv(rows.map(safeRow), [...cols]), rows: rows.length }));
}
