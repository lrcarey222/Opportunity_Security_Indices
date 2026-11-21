// Everything the internal QC dashboard (/review/admin) and the CSV exports summarise from submissions
// (docs/QC_SYSTEM_DESIGN.md 6.6): overview, coverage, anchor coverage against the campaign target,
// model-vs-expert and expert-vs-expert agreement per case, blind-vs-reveal transitions, the disagreement
// taxonomy, priority fixes (materiality x confidence the issue is real x breadth; adjudication overrides;
// corroboration raises confidence, capped at 1; frequency shown, never used alone), unresolved critical
// issues, and method / framework / decision-use / threat summaries.
// Rules: "I don't know" blind answers and verdict 7 are counted but excluded from every agreement
// denominator; a case with fewer than 3 interpretable reviews is "too few reviews to summarise" and its
// shares are null. No kappa, no significance. Expert disagreement is evidence to investigate, not a vote.
// Pure: no IO.

import {
  DONT_KNOW,
  ISSUE_TYPE_LABEL,
  MATCH_WEIGHT,
  SEVERITY_WEIGHT,
  VERDICT_ISSUE_TYPE,
  type Adjudication,
  type AdjudicationStatus,
  type Campaign,
  type CaseReviewPayload,
  type DataIssuePayload,
  type DecisionUsePayload,
  type ExpertiseMatch,
  type FrameworkAnswerPayload,
  type FrameworkScenario,
  type IssueType,
  type MethodCard,
  type MethodFeedbackPayload,
  type ReviewCase,
  type RobustnessClass,
  type Severity,
  type Submission,
  type SubmissionKind,
  type ThreatReviewPayload,
  type Verdict,
} from './model.ts';
import { caseTags, DEFAULT_MIN_CONFIDENCE, isExpertIn, normaliseCountryTag, normaliseSectorTag, normaliseStageTag, profileMatchesCase, reporterConfidence, tagConfidence } from './tags.ts';

export const MIN_REVIEWS_TO_SUMMARISE = 3;
export const TOO_FEW_LABEL = 'Too few reviews to summarise';
/** Reporter confidence assumed when a report carries none (no rating on the matching tags). */
export const DEFAULT_REPORTER_CONFIDENCE = 0.5;

const ISSUE_TYPES = Object.keys(ISSUE_TYPE_LABEL) as IssueType[];
const SEVERITIES: Severity[] = ['cosmetic', 'minor', 'material', 'critical'];
const CLOSED: readonly AdjudicationStatus[] = ['resolved', 'rejected', 'duplicate', 'wont_fix'];

type Sub<K extends SubmissionKind> = Submission<K>;
const ofKind = <K extends SubmissionKind>(subs: readonly Submission[], k: K): Sub<K>[] => subs.filter((s): s is Sub<K> => s.kind === k);
const when = (s: Submission) => s.submitted_at ?? s.created_at ?? '';

// ---- de-duplication -----------------------------------------------------------------------------------------

/** One copy of each submission_id (the latest submitted), in a stable order. */
export function dedupeSubmissions(subs: readonly Submission[]): Submission[] {
  const by = new Map<string, Submission>();
  for (const s of subs) {
    if (!s || typeof s.submission_id !== 'string') continue;
    const prev = by.get(s.submission_id);
    if (!prev || when(s) > when(prev)) by.set(s.submission_id, s);
  }
  return [...by.values()].sort((a, b) => (when(a) < when(b) ? -1 : when(a) > when(b) ? 1 : a.submission_id < b.submission_id ? -1 : 1));
}

/** Whether a case review is complete enough to count: a stored blind choice and a verdict 1-7. */
const isCompleteReview = (p: CaseReviewPayload | null | undefined): boolean =>
  !!p &&
  typeof p.case_id === 'string' &&
  typeof p.blind === 'object' &&
  p.blind !== null &&
  typeof p.blind.choice === 'string' &&
  typeof p.reveal === 'object' &&
  p.reveal !== null &&
  typeof p.reveal.verdict === 'number' &&
  p.reveal.verdict >= 1 &&
  p.reveal.verdict <= 7;

/**
 * Case reviews, the latest per (reviewer, case): a reviewer who resubmits counts once. Incomplete reviews
 * (no blind choice or no verdict: a hand-edited or truncated packet) are left out rather than miscounted.
 */
export function latestCaseReviews(subs: readonly Submission[]): Sub<'case_review'>[] {
  const by = new Map<string, Sub<'case_review'>>();
  for (const s of ofKind(dedupeSubmissions(subs), 'case_review')) {
    if (!s.reviewer || !isCompleteReview(s.payload)) continue;
    const k = `${s.reviewer.reviewer_id}|${s.payload.case_id}`;
    const prev = by.get(k);
    if (!prev || when(s) >= when(prev)) by.set(k, s);
  }
  return [...by.values()];
}

/** Whether a case review is an outside-expertise answer (counted, never in an agreement denominator). */
export const isOutsideAnswer = (p: CaseReviewPayload): boolean => p.blind?.choice === DONT_KNOW || p.reveal?.verdict === 7;
const isPersuaded = (v: Verdict | undefined) => v === 2;
const isRejecting = (v: Verdict | undefined) => v === 4 || v === 5 || v === 6;

// ---- per-case agreement ------------------------------------------------------------------------------------

export interface CaseAgreement {
  case_id: string;
  kind: string | null;
  title: string | null;
  anchor: boolean;
  model_choice: string | null;
  model_label: string | null;
  n_reviews: number;
  n_dont_know: number;
  n_verdict7: number;
  /** Reviews outside expertise (DONT_KNOW or verdict 7; a review with both counts once). */
  n_outside: number;
  n_interpretable: number;
  /** Blind choices of the interpretable reviews. */
  blind_distribution: Record<string, number>;
  /** The most frequent blind choice(s); ties listed in option order. */
  modal: string[];
  /** Expert vs expert: share of interpretable reviews on the modal answer (null when too few). */
  modal_share: number | null;
  n_matching_model: number;
  n_persuaded: number;
  n_rejecting: number;
  share_matching_model: number | null;
  share_persuaded: number | null;
  share_rejecting: number | null;
  verdict_counts: Record<number, number>;
  issue_types: Partial<Record<IssueType, number>>;
  suggested_choices: Record<string, number>;
  /** Reviews made against a different snapshot than the current case file (another data release). */
  n_snapshot_mismatch: number;
  too_few: boolean;
  summary_label: string | null;
}

const share = (n: number, d: number): number | null => (d > 0 ? n / d : null);

/** Agreement per case: every case in `cases` (reviewed or not) plus any reviewed case missing from them. */
export function caseAgreement(subs: readonly Submission[], cases: readonly ReviewCase[]): CaseAgreement[] {
  const reviews = latestCaseReviews(subs);
  const byCase = new Map<string, Sub<'case_review'>[]>();
  for (const r of reviews) byCase.set(r.payload.case_id, [...(byCase.get(r.payload.case_id) ?? []), r]);
  const caseById = new Map(cases.map((c) => [c.case_id, c]));
  const ids = [...cases.map((c) => c.case_id), ...[...byCase.keys()].filter((id) => !caseById.has(id)).sort()];
  return ids.map((id) => {
    const c = caseById.get(id) ?? null;
    const rs = byCase.get(id) ?? [];
    const model = c?.snapshot?.conclusion?.value ?? null;
    const interp = rs.filter((r) => !isOutsideAnswer(r.payload));
    const dist: Record<string, number> = {};
    for (const r of interp) dist[r.payload.blind.choice] = (dist[r.payload.blind.choice] ?? 0) + 1;
    const order = c?.answer_options?.map((o) => o.value) ?? [];
    const top = Math.max(0, ...Object.values(dist));
    const modal = top > 0 ? Object.keys(dist).filter((k) => dist[k] === top).sort((a, b) => (order.indexOf(a) + 1 || 999) - (order.indexOf(b) + 1 || 999) || (a < b ? -1 : 1)) : [];
    const nMatch = model === null ? 0 : interp.filter((r) => r.payload.blind.choice === model).length;
    const nPers = interp.filter((r) => isPersuaded(r.payload.reveal?.verdict)).length;
    const nRej = interp.filter((r) => isRejecting(r.payload.reveal?.verdict)).length;
    const verdicts: Record<number, number> = { 1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0, 7: 0 };
    const issues: Partial<Record<IssueType, number>> = {};
    const suggested: Record<string, number> = {};
    for (const r of rs) {
      const v = r.payload.reveal?.verdict;
      if (v) verdicts[v] = (verdicts[v] ?? 0) + 1;
      const t = disagreementType(r.payload);
      if (t) issues[t] = (issues[t] ?? 0) + 1;
      const sc = r.payload.reveal?.suggested_choice;
      if (sc) suggested[sc] = (suggested[sc] ?? 0) + 1;
    }
    const tooFew = interp.length < MIN_REVIEWS_TO_SUMMARISE;
    return {
      case_id: id,
      kind: c?.kind ?? null,
      title: c?.title ?? null,
      anchor: c?.anchor === true,
      model_choice: model,
      model_label: c?.snapshot?.conclusion?.label ?? null,
      n_reviews: rs.length,
      n_dont_know: rs.filter((r) => r.payload.blind?.choice === DONT_KNOW).length,
      n_verdict7: rs.filter((r) => r.payload.reveal?.verdict === 7).length,
      n_outside: rs.length - interp.length,
      n_interpretable: interp.length,
      blind_distribution: dist,
      modal,
      modal_share: tooFew ? null : share(top, interp.length),
      n_matching_model: nMatch,
      n_persuaded: nPers,
      n_rejecting: nRej,
      share_matching_model: tooFew || model === null ? null : share(nMatch, interp.length),
      share_persuaded: tooFew ? null : share(nPers, interp.length),
      share_rejecting: tooFew ? null : share(nRej, interp.length),
      verdict_counts: verdicts,
      issue_types: issues,
      suggested_choices: suggested,
      n_snapshot_mismatch: c ? rs.filter((r) => r.payload.case_snapshot_sha256 !== c.snapshot_sha256).length : 0,
      too_few: tooFew,
      summary_label: tooFew ? TOO_FEW_LABEL : null,
    };
  });
}

/** The disagreement type a case review records (verdicts 3-6 only): the stated issue type, else the verdict's. */
export function disagreementType(p: CaseReviewPayload): IssueType | null {
  const v = p.reveal?.verdict;
  if (v !== 3 && v !== 4 && v !== 5 && v !== 6) return null;
  return p.reveal.main_issue?.type ?? VERDICT_ISSUE_TYPE[v] ?? null;
}

// ---- blind vs reveal ----------------------------------------------------------------------------------------

export type TransitionKind =
  | 'agreed_confirmed'
  | 'agreed_doubtful'
  | 'agreed_rejecting'
  | 'differed_persuaded'
  | 'differed_unconvinced'
  | 'differed_rejecting'
  | 'outside'
  | 'inconsistent'
  | 'no_model';

export interface TransitionSummary {
  counts: Record<TransitionKind, number>;
  /** blind choice x model choice x verdict, for the transition chart. */
  matrix: { blind: string; model: string; verdict: number; n: number }[];
  /** Reviews whose verdict contradicts their own blind answer (verdict 1 with a different blind answer; verdict 2 with the same). */
  inconsistent: { submission_id: string; case_id: string; blind: string; model: string; verdict: number }[];
}

/** How each blind prior moved after the reveal. */
export function blindRevealTransitions(subs: readonly Submission[], cases: readonly ReviewCase[]): TransitionSummary {
  const model = new Map(cases.map((c) => [c.case_id, c.snapshot?.conclusion?.value ?? null]));
  const counts: Record<TransitionKind, number> = {
    agreed_confirmed: 0, agreed_doubtful: 0, agreed_rejecting: 0, differed_persuaded: 0, differed_unconvinced: 0, differed_rejecting: 0, outside: 0, inconsistent: 0, no_model: 0,
  };
  const cells = new Map<string, { blind: string; model: string; verdict: number; n: number }>();
  const inconsistent: TransitionSummary['inconsistent'] = [];
  for (const r of latestCaseReviews(subs)) {
    const p = r.payload;
    const m = model.get(p.case_id) ?? null;
    const v = p.reveal?.verdict;
    if (isOutsideAnswer(p)) {
      counts.outside++;
      continue;
    }
    if (m === null) {
      counts.no_model++;
      continue;
    }
    const same = p.blind.choice === m;
    const key = `${p.blind.choice}|${m}|${v}`;
    const cell = cells.get(key) ?? { blind: p.blind.choice, model: m, verdict: v, n: 0 };
    cell.n++;
    cells.set(key, cell);
    if ((v === 1 && !same) || (v === 2 && same)) {
      counts.inconsistent++;
      inconsistent.push({ submission_id: r.submission_id, case_id: p.case_id, blind: p.blind.choice, model: m, verdict: v });
      continue;
    }
    if (same) counts[v === 1 ? 'agreed_confirmed' : v === 3 ? 'agreed_doubtful' : 'agreed_rejecting']++;
    else counts[v === 2 ? 'differed_persuaded' : v === 3 ? 'differed_unconvinced' : 'differed_rejecting']++;
  }
  return { counts, matrix: [...cells.values()].sort((a, b) => b.n - a.n || (a.blind < b.blind ? -1 : 1)), inconsistent };
}

// ---- coverage -----------------------------------------------------------------------------------------------

export type CoverageDimension = 'sector' | 'country' | 'stage' | 'kind';

export interface CoverageRow {
  dimension: CoverageDimension;
  key: string;
  n_cases: number;
  n_cases_reviewed: number;
  n_reviews: number;
  n_interpretable: number;
}

const inScope = (cases: readonly ReviewCase[], campaign?: Campaign | null) => (campaign?.case_ids?.length ? cases.filter((c) => campaign.case_ids.includes(c.case_id)) : [...cases]);

/** Cases and reviews by sector tag, country, stage and kind (a case counts under each of its tags). */
export function coverage(subs: readonly Submission[], cases: readonly ReviewCase[], campaign?: Campaign | null): CoverageRow[] {
  const agree = new Map(caseAgreement(subs, cases).map((a) => [a.case_id, a]));
  const rows = new Map<string, CoverageRow>();
  const bump = (dimension: CoverageDimension, key: string, c: ReviewCase) => {
    const k = `${dimension}|${key}`;
    const row = rows.get(k) ?? { dimension, key, n_cases: 0, n_cases_reviewed: 0, n_reviews: 0, n_interpretable: 0 };
    const a = agree.get(c.case_id);
    row.n_cases++;
    if (a && a.n_reviews > 0) row.n_cases_reviewed++;
    row.n_reviews += a?.n_reviews ?? 0;
    row.n_interpretable += a?.n_interpretable ?? 0;
    rows.set(k, row);
  };
  for (const c of inScope(cases, campaign)) {
    const t = caseTags(c);
    for (const s of t.sectors.length ? t.sectors : ['(none)']) bump('sector', s, c);
    for (const s of t.countries.length ? t.countries : ['(none)']) bump('country', s, c);
    bump('stage', t.stage ?? '(none)', c);
    bump('kind', c.kind, c);
  }
  const dimOrder: CoverageDimension[] = ['kind', 'sector', 'country', 'stage'];
  return [...rows.values()].sort((a, b) => dimOrder.indexOf(a.dimension) - dimOrder.indexOf(b.dimension) || (a.key < b.key ? -1 : 1));
}

export interface AnchorCoverageRow {
  case_id: string;
  title: string | null;
  target: number;
  n_reviews: number;
  n_interpretable: number;
  remaining: number;
  /** Met on interpretable reviews (outside-expertise answers do not count toward the target). */
  met: boolean;
}

export function anchorCoverage(subs: readonly Submission[], cases: readonly ReviewCase[], campaign: Campaign | null | undefined): AnchorCoverageRow[] {
  const anchors = campaign?.anchor_cases?.length ? campaign.anchor_cases : cases.filter((c) => c.anchor).map((c) => c.case_id);
  const target = campaign?.target_reviews_per_anchor ?? MIN_REVIEWS_TO_SUMMARISE;
  const agree = new Map(caseAgreement(subs, cases).map((a) => [a.case_id, a]));
  const titles = new Map(cases.map((c) => [c.case_id, c.title]));
  return anchors.map((id) => {
    const a = agree.get(id);
    const ni = a?.n_interpretable ?? 0;
    return { case_id: id, title: titles.get(id) ?? null, target, n_reviews: a?.n_reviews ?? 0, n_interpretable: ni, remaining: Math.max(0, target - ni), met: ni >= target };
  });
}

// ---- disagreement taxonomy ---------------------------------------------------------------------------------

export interface TaxonomyRow {
  issue_type: IssueType;
  label: string;
  from_case_reviews: number;
  from_framework: number;
  from_decision_use: number;
  total: number;
}

/** Low actionability: a decision-use answer of 1 or 2 on "actionable". */
export const isLowActionability = (p: DecisionUsePayload): boolean => p.actionable === 1 || p.actionable === 2;
/** A framework mismatch (an answer other than the intended one; "I don't know" is not a mismatch). */
export const isFrameworkMismatch = (p: FrameworkAnswerPayload): boolean => p.answer !== DONT_KNOW && p.answer !== p.intended_answer;

export function disagreementTaxonomy(subs: readonly Submission[]): TaxonomyRow[] {
  const rows = new Map<IssueType, TaxonomyRow>(ISSUE_TYPES.map((t) => [t, { issue_type: t, label: ISSUE_TYPE_LABEL[t], from_case_reviews: 0, from_framework: 0, from_decision_use: 0, total: 0 }]));
  for (const r of latestCaseReviews(subs)) {
    const t = disagreementType(r.payload);
    if (t) rows.get(t)!.from_case_reviews++;
  }
  const all = dedupeSubmissions(subs);
  for (const f of ofKind(all, 'framework_answer')) if (f.payload && isFrameworkMismatch(f.payload)) rows.get('framework')!.from_framework++;
  for (const d of ofKind(all, 'decision_use')) if (d.payload && isLowActionability(d.payload)) rows.get('actionability')!.from_decision_use++;
  for (const row of rows.values()) row.total = row.from_case_reviews + row.from_framework + row.from_decision_use;
  return [...rows.values()];
}

// ---- priority fixes -----------------------------------------------------------------------------------------

export type PrioritySource = 'data_issue' | 'break_model' | 'case_review' | 'method_feedback';

export interface PriorityItem {
  /** Stable for the same inputs: the grouping key of the issue. */
  group_id: string;
  source: PrioritySource;
  title: string;
  issue_type: IssueType;
  severity: Severity;
  materiality: number;
  confidence_real: number;
  confidence_basis: 'adjudication' | 'reports';
  affected_outputs: number;
  breadth: number;
  score: number;
  /** Frequency: shown, never used alone. */
  n_reports: number;
  n_reviewers: number;
  submission_ids: string[];
  adjudication_status: AdjudicationStatus | null;
  unresolved: boolean;
  case_id: string | null;
  metric_id: string | null;
  country: string | null;
  tech: string | null;
  stage: string | null;
}

interface Report {
  sub: Submission;
  source: PrioritySource;
  group: string;
  title: string;
  issue_type: IssueType;
  severity: Severity;
  confidence: number;
  affected_default: number;
  case_id: string | null;
  metric_id: string | null;
  country: string | null;
  tech: string | null;
  stage: string | null;
}

/** A case review's materiality (design 6.6 gives none for verdicts): "probably wrong" 4-6 material, 3 minor. */
export const VERDICT_SEVERITY: Readonly<Partial<Record<Verdict, Severity>>> = { 3: 'minor', 4: 'material', 5: 'material', 6: 'material' };
/** A method card's materiality answer (1-4) as a severity; unanswered = minor. */
export const METHOD_MATERIALITY_SEVERITY: Readonly<Record<1 | 2 | 3 | 4, Severity>> = { 1: 'cosmetic', 2: 'minor', 3: 'material', 4: 'critical' };

/** The expertise match of a report on a data point (reviewer tags vs the point's country / technology / stage). */
export function reportMatch(sub: Submission, at: { country: string | null; tech: string | null; stage: string | null }): { match: ExpertiseMatch; reporter: number | null } {
  const ex = sub.reviewer.expertise;
  if (!ex) return { match: 'adjacent', reporter: null };
  const holder = { ...ex };
  const sector = at.tech ? normaliseSectorTag(at.tech) : null;
  const pseudo = { sector_groups: sector ? [sector] : [], country_groups: at.country ? [at.country] : [], stage: at.stage, key: {} };
  return {
    match: profileMatchesCase(holder, pseudo, DEFAULT_MIN_CONFIDENCE),
    reporter: reporterConfidence(holder, { sectors: pseudo.sector_groups, countries: pseudo.country_groups, stage: at.stage }),
  };
}

function reportsOf(subs: readonly Submission[], cases: readonly ReviewCase[], cards: readonly MethodCard[]): Report[] {
  const out: Report[] = [];
  const caseById = new Map(cases.map((c) => [c.case_id, c]));
  const cardById = new Map(cards.map((c) => [c.card_id, c]));
  const all = dedupeSubmissions(subs);
  for (const s of all) {
    if (s.kind === 'data_issue' || s.kind === 'break_model') {
      const p = s.payload as DataIssuePayload;
      if (!p) continue;
      const pc = s.page_context ?? null;
      const country = normaliseCountryTag(p.metric?.country ?? pc?.country ?? null);
      const tech = normaliseSectorTag(p.metric?.tech ?? pc?.tech ?? null) ?? p.metric?.tech ?? pc?.tech ?? null;
      const stage = normaliseStageTag(p.metric?.stage ?? pc?.stage ?? null);
      const metricId = p.metric?.id ?? null;
      const { match, reporter } = reportMatch(s, { country, tech, stage });
      const where = [country, tech, stage].filter(Boolean).join(' / ');
      out.push({
        sub: s,
        source: s.kind,
        group: `data:${metricId ?? (pc?.route ?? '-')}:${country ?? '-'}:${tech ?? '-'}:${stage ?? '-'}${metricId ? '' : `:${pc?.node ?? pc?.pair ?? pc?.sid ?? '-'}`}`,
        title: `${p.metric?.label ?? metricId ?? 'Page'}${where ? ` (${where})` : ''}: ${p.what_is_wrong}`,
        issue_type: 'data',
        severity: SEVERITIES.includes(p.severity) ? p.severity : 'minor',
        confidence: (reporter ?? DEFAULT_REPORTER_CONFIDENCE) * MATCH_WEIGHT[match],
        affected_default: 1,
        case_id: null,
        metric_id: metricId,
        country,
        tech,
        stage,
      });
    } else if (s.kind === 'case_review') {
      const p = s.payload as CaseReviewPayload;
      const t = p ? disagreementType(p) : null;
      // verdict 7 gives no type; a reviewer who answered "I don't know" blind but still judged the result
      // wrong after the reveal is a report, weighted as outside their expertise
      if (!p || !t) continue;
      const c = caseById.get(p.case_id);
      const ct = c ? caseTags(c) : { sectors: [], countries: [], stage: null };
      const conf = typeof p.blind?.confidence === 'number' ? p.blind.confidence / 5 : DEFAULT_REPORTER_CONFIDENCE;
      const match: ExpertiseMatch = isOutsideAnswer(p) ? 'outside' : (p.expertise_match ?? 'adjacent');
      out.push({
        sub: s,
        source: 'case_review',
        group: `case:${p.case_id}:${t}`,
        title: `${c?.title ?? p.case_id}: ${p.reveal.main_issue?.text ?? ISSUE_TYPE_LABEL[t]}`,
        issue_type: t,
        severity: VERDICT_SEVERITY[p.reveal.verdict] ?? 'minor',
        confidence: conf * MATCH_WEIGHT[match],
        affected_default: 1,
        case_id: p.case_id,
        metric_id: p.reveal.main_issue?.metric_ids?.[0] ?? null,
        country: ct.countries[0] ?? c?.key?.iso3 ?? null,
        tech: ct.sectors[0] ?? null,
        stage: ct.stage,
      });
    } else if (s.kind === 'method_feedback') {
      const p = s.payload as MethodFeedbackPayload;
      if (!p || (p.stance !== 'reject' && p.stance !== 'modify') || p.outside_expertise) continue;
      const card = cardById.get(p.card_id);
      const ex = s.reviewer.expertise;
      let match: ExpertiseMatch = 'adjacent';
      let rep: number | null = null;
      if (ex && card) {
        match = card.method_tags.some((t) => isExpertIn(ex, 'method', t, DEFAULT_MIN_CONFIDENCE)) ? 'in' : 'outside';
        const rs = card.method_tags.map((t) => tagConfidence(ex, 'method', t)).filter((x): x is number => x !== null);
        rep = rs.length ? Math.max(...rs) / 5 : null;
      }
      out.push({
        sub: s,
        source: 'method_feedback',
        group: `method:${p.card_id}`,
        title: `${card?.title ?? p.card_id}: ${p.stance}${p.alternative ? ` (${p.alternative})` : ''}`,
        issue_type: 'method',
        severity: p.materiality ? METHOD_MATERIALITY_SEVERITY[p.materiality] : 'minor',
        confidence: (rep ?? DEFAULT_REPORTER_CONFIDENCE) * MATCH_WEIGHT[match],
        affected_default: card?.affected_outputs?.length || 1,
        case_id: null,
        metric_id: null,
        country: null,
        tech: null,
        stage: null,
      });
    }
  }
  return out;
}

/** The latest adjudication of each submission (a decided status wins over 'open' at the same time). */
export function latestAdjudications(adjs: readonly Adjudication[]): Map<string, Adjudication> {
  const by = new Map<string, Adjudication>();
  for (const a of adjs) {
    if (!a || typeof a.submission_id !== 'string') continue;
    const prev = by.get(a.submission_id);
    if (!prev || a.decided_at > prev.decided_at || (a.decided_at === prev.decided_at && prev.status === 'open')) by.set(a.submission_id, a);
  }
  return by;
}

const affectedCount = (x: Adjudication['affected_outputs']): number | null => (typeof x === 'number' && Number.isFinite(x) && x >= 0 ? x : Array.isArray(x) ? x.length : null);
/** breadth = 1 + log2(1 + affected outputs) (design 6.6). */
export const breadthOf = (affected: number): number => 1 + Math.log2(1 + Math.max(0, affected));

export interface PriorityOptions {
  cards?: readonly MethodCard[];
}

/**
 * Priority fixes, ranked by materiality x confidence-real x breadth (design 6.6). Reports on the same issue
 * (same metric and cell, same case and issue type, same method card; or linked by an adjudication's
 * duplicate_of) are one item. Confidence: the adjudication's when one is decided (rejected = 0), else the
 * reports' combined as independent evidence, 1 - prod(1 - c_reviewer), capped at 1, where c = reporter
 * confidence x expertise match weight. Ordered by score, then materiality, then reports, then id.
 */
export function priorityFixes(subs: readonly Submission[], cases: readonly ReviewCase[], adjudications: readonly Adjudication[] = [], opts: PriorityOptions = {}): PriorityItem[] {
  const reports = reportsOf(subs, cases, opts.cards ?? []);
  const adj = latestAdjudications(adjudications);
  // duplicate_of links move a report into the group of the report it duplicates
  const groupOfSub = new Map(reports.map((r) => [r.sub.submission_id, r.group]));
  for (const r of reports) {
    const a = adj.get(r.sub.submission_id);
    const target = a?.duplicate_of ? groupOfSub.get(a.duplicate_of) : undefined;
    if (target) r.group = target;
  }
  const groups = new Map<string, Report[]>();
  for (const r of reports) groups.set(r.group, [...(groups.get(r.group) ?? []), r]);

  const items: PriorityItem[] = [];
  for (const [gid, rs] of groups) {
    const first = rs[0];
    const severity = rs.map((r) => r.severity).sort((a, b) => SEVERITIES.indexOf(b) - SEVERITIES.indexOf(a))[0];
    // the deciding adjudication: the latest decided one among the group's reports (a duplicate link is not a verdict on the issue)
    const decided = rs
      .map((r) => adj.get(r.sub.submission_id))
      .filter((a): a is Adjudication => !!a && !(a.status === 'duplicate' && a.duplicate_of && groupOfSub.get(a.duplicate_of) === gid))
      .sort((a, b) => (a.decided_at < b.decided_at ? 1 : -1));
    const ruling = decided.find((a) => a.status !== 'open') ?? decided[0] ?? null;
    let confidence: number;
    let basis: PriorityItem['confidence_basis'] = 'reports';
    if (ruling && ruling.status !== 'open') {
      basis = 'adjudication';
      confidence = ruling.status === 'rejected' || ruling.status === 'duplicate' ? (ruling.confidence_real ?? 0) : (ruling.confidence_real ?? 1);
    } else if (ruling && ruling.confidence_real !== null && ruling.confidence_real !== undefined) {
      basis = 'adjudication';
      confidence = ruling.confidence_real;
    } else {
      const perReviewer = new Map<string, number>();
      for (const r of rs) perReviewer.set(r.sub.reviewer.reviewer_id, Math.max(perReviewer.get(r.sub.reviewer.reviewer_id) ?? 0, r.confidence));
      confidence = 1 - [...perReviewer.values()].reduce((p, c) => p * (1 - Math.min(1, Math.max(0, c))), 1);
    }
    confidence = Math.min(1, Math.max(0, confidence));
    const affected = (ruling ? affectedCount(ruling.affected_outputs) : null) ?? Math.max(...rs.map((r) => r.affected_default));
    const materiality = SEVERITY_WEIGHT[severity];
    const breadth = breadthOf(affected);
    const status = ruling?.status ?? null;
    items.push({
      group_id: gid,
      source: first.source,
      title: first.title,
      issue_type: (ruling?.issue_type as IssueType | null) ?? first.issue_type,
      severity,
      materiality,
      confidence_real: confidence,
      confidence_basis: basis,
      affected_outputs: affected,
      breadth,
      score: materiality * confidence * breadth,
      n_reports: rs.length,
      n_reviewers: new Set(rs.map((r) => r.sub.reviewer.reviewer_id)).size,
      submission_ids: rs.map((r) => r.sub.submission_id).sort(),
      adjudication_status: status,
      unresolved: status === null || !CLOSED.includes(status),
      case_id: first.case_id,
      metric_id: first.metric_id,
      country: first.country,
      tech: first.tech,
      stage: first.stage,
    });
  }
  return items.sort((a, b) => b.score - a.score || b.materiality - a.materiality || b.n_reports - a.n_reports || (a.group_id < b.group_id ? -1 : 1));
}

/** Critical issues not resolved, rejected, duplicated or declined (and not judged unreal). */
export const unresolvedCritical = (items: readonly PriorityItem[]): PriorityItem[] => items.filter((i) => i.severity === 'critical' && i.unresolved && i.confidence_real > 0);

// ---- method, framework, decision-use and threat summaries ---------------------------------------------------

export interface MethodSummary {
  card_id: string;
  title: string | null;
  n: number;
  n_outside: number;
  stance: Record<string, number>;
  mean_materiality: number | null;
  bias: Record<string, number>;
  alternatives: string[];
}

export function methodSummary(subs: readonly Submission[], cards: readonly MethodCard[] = []): MethodSummary[] {
  const fb = ofKind(dedupeSubmissions(subs), 'method_feedback').filter((s) => s.payload && typeof s.payload.card_id === 'string');
  const ids = [...new Set([...cards.map((c) => c.card_id), ...fb.map((s) => s.payload.card_id)])];
  return ids.map((id) => {
    const rs = fb.filter((s) => s.payload.card_id === id).map((s) => s.payload);
    const stance: Record<string, number> = { retain: 0, modify: 0, reject: 0, unsure: 0 };
    const bias: Record<string, number> = {};
    for (const p of rs) {
      stance[p.stance] = (stance[p.stance] ?? 0) + 1;
      bias[p.bias_direction] = (bias[p.bias_direction] ?? 0) + 1;
    }
    const mats = rs.map((p) => p.materiality).filter((m): m is 1 | 2 | 3 | 4 => typeof m === 'number');
    return {
      card_id: id,
      title: cards.find((c) => c.card_id === id)?.title ?? null,
      n: rs.length,
      n_outside: rs.filter((p) => p.outside_expertise).length,
      stance,
      mean_materiality: mats.length ? mats.reduce((a, b) => a + b, 0) / mats.length : null,
      bias,
      alternatives: rs.map((p) => p.alternative ?? '').filter((x) => x.trim()),
    };
  });
}

export interface FrameworkSummary {
  scenario_id: string;
  concept: string | null;
  intended_answer: string | null;
  n: number;
  n_dont_know: number;
  n_correct: number;
  share_correct: number | null;
  answers: Record<string, number>;
  confusing_terms: Record<string, number>;
  too_few: boolean;
}

export function frameworkSummary(subs: readonly Submission[], scenarios: readonly FrameworkScenario[] = []): FrameworkSummary[] {
  const fa = ofKind(dedupeSubmissions(subs), 'framework_answer').filter((s) => s.payload && typeof s.payload.scenario_id === 'string');
  const ids = [...new Set([...scenarios.map((s) => s.scenario_id), ...fa.map((s) => s.payload.scenario_id)])];
  return ids.map((id) => {
    const sc = scenarios.find((s) => s.scenario_id === id) ?? null;
    const rs = fa.filter((s) => s.payload.scenario_id === id).map((s) => s.payload);
    const intended = sc?.intended_answer ?? rs[0]?.intended_answer ?? null;
    const answers: Record<string, number> = {};
    const terms: Record<string, number> = {};
    for (const p of rs) {
      answers[p.answer] = (answers[p.answer] ?? 0) + 1;
      for (const t of p.confusing_terms ?? []) terms[t] = (terms[t] ?? 0) + 1;
    }
    const known = rs.filter((p) => p.answer !== DONT_KNOW);
    // correctness is recomputed against the current scenario, not trusted from the payload
    const nCorrect = known.filter((p) => p.answer === intended).length;
    const tooFew = known.length < MIN_REVIEWS_TO_SUMMARISE;
    return {
      scenario_id: id,
      concept: sc?.concept ?? null,
      intended_answer: intended,
      n: rs.length,
      n_dont_know: rs.length - known.length,
      n_correct: nCorrect,
      share_correct: tooFew ? null : share(nCorrect, known.length),
      answers,
      confusing_terms: terms,
      too_few: tooFew,
    };
  });
}

const mean = (xs: (number | null | undefined)[]): number | null => {
  const v = xs.filter((x): x is number => typeof x === 'number' && Number.isFinite(x));
  return v.length ? v.reduce((a, b) => a + b, 0) / v.length : null;
};

export interface DecisionUseSummary {
  case_id: string;
  n: number;
  mean_actionable: number | null;
  mean_identifies_problem: number | null;
  mean_separates: number | null;
  mean_right_geography: number | null;
  n_low_actionability: number;
}

export function decisionUseSummary(subs: readonly Submission[]): DecisionUseSummary[] {
  const du = ofKind(dedupeSubmissions(subs), 'decision_use').filter((s) => s.payload && typeof s.payload.case_id === 'string');
  return [...new Set(du.map((s) => s.payload.case_id))].sort().map((id) => {
    const rs = du.filter((s) => s.payload.case_id === id).map((s) => s.payload);
    return {
      case_id: id,
      n: rs.length,
      mean_actionable: mean(rs.map((p) => p.actionable)),
      mean_identifies_problem: mean(rs.map((p) => p.identifies_problem)),
      mean_separates: mean(rs.map((p) => p.separates_diagnosis_from_prescription)),
      mean_right_geography: mean(rs.map((p) => p.right_geography)),
      n_low_actionability: rs.filter(isLowActionability).length,
    };
  });
}

export interface ThreatSummary {
  case_id: string;
  n: number;
  mean_reduces_threat: number | null;
  mean_scale_sufficient: number | null;
  mean_timing_plausible: number | null;
  mean_commercially_plausible: number | null;
  missing_constraints: Record<string, number>;
  /** Reviews of a threat snapshot other than the current one. */
  n_snapshot_mismatch: number;
}

/** Threat-review means per case; with the current threat snapshot hashes by case, reviews of an older snapshot are counted. */
export function threatSummary(subs: readonly Submission[], currentThreatSha?: Readonly<Record<string, string>>): ThreatSummary[] {
  const tr = ofKind(dedupeSubmissions(subs), 'threat_review').filter((s) => s.payload && typeof s.payload.case_id === 'string');
  return [...new Set(tr.map((s) => s.payload.case_id))].sort().map((id) => {
    const rs: ThreatReviewPayload[] = tr.filter((s) => s.payload.case_id === id).map((s) => s.payload);
    const mc: Record<string, number> = {};
    for (const p of rs) for (const m of p.missing_constraints ?? []) mc[m] = (mc[m] ?? 0) + 1;
    const cur = currentThreatSha?.[id];
    return {
      case_id: id,
      n: rs.length,
      mean_reduces_threat: mean(rs.map((p) => p.reduces_threat)),
      mean_scale_sufficient: mean(rs.map((p) => p.scale_sufficient)),
      mean_timing_plausible: mean(rs.map((p) => p.timing_plausible)),
      mean_commercially_plausible: mean(rs.map((p) => p.commercially_plausible)),
      missing_constraints: mc,
      n_snapshot_mismatch: cur ? rs.filter((p) => p.threat_snapshot_sha256 !== cur).length : 0,
    };
  });
}

export interface RobustnessSummary {
  counts: Record<RobustnessClass | 'missing', number>;
  drivers: Record<string, number>;
}

/** Robustness classes of the cases (from their snapshots; 'missing' when a case carries none). */
export function robustnessSummary(cases: readonly ReviewCase[]): RobustnessSummary {
  const counts: RobustnessSummary['counts'] = { stable: 0, sensitive: 0, fragile: 0, not_testable: 0, missing: 0 };
  const drivers: Record<string, number> = {};
  for (const c of cases) {
    const r = c.snapshot?.robustness;
    if (!r) {
      counts.missing++;
      continue;
    }
    counts[r.class] = (counts[r.class] ?? 0) + 1;
    for (const d of r.drivers ?? []) drivers[d] = (drivers[d] ?? 0) + 1;
  }
  return { counts, drivers };
}

// ---- overview -----------------------------------------------------------------------------------------------

export interface Overview {
  n_submissions: number;
  n_reviewers: number;
  by_kind: Record<SubmissionKind, number>;
  n_cases_in_scope: number;
  n_cases_reviewed: number;
  n_case_reviews: number;
  n_outside_answers: number;
  n_anchor_cases: number;
  n_anchor_met: number;
  sectors_covered: string[];
  countries_covered: string[];
  n_data_reports: number;
  n_open_data_reports: number;
  n_unresolved_critical: number;
  /** Data releases the submissions were made against: more than one means agreement mixes releases. */
  data_releases: string[];
  n_snapshot_mismatch: number;
}

export interface DashboardInput {
  submissions: readonly Submission[];
  cases: readonly ReviewCase[];
  campaign?: Campaign | null;
  adjudications?: readonly Adjudication[];
  cards?: readonly MethodCard[];
  scenarios?: readonly FrameworkScenario[];
}

export interface Dashboard {
  overview: Overview;
  coverage: CoverageRow[];
  anchors: AnchorCoverageRow[];
  agreement: CaseAgreement[];
  transitions: TransitionSummary;
  taxonomy: TaxonomyRow[];
  priorities: PriorityItem[];
  unresolved_critical: PriorityItem[];
  methods: MethodSummary[];
  framework: FrameworkSummary[];
  decision_use: DecisionUseSummary[];
  threat: ThreatSummary[];
  robustness: RobustnessSummary;
}

/** Everything the admin dashboard shows, from one call. Missing inputs give empty sections, never a throw. */
export function buildDashboard(input: DashboardInput): Dashboard {
  const subs = dedupeSubmissions(input.submissions ?? []).filter((s) => !input.campaign || s.campaign_id === null || s.campaign_id === input.campaign.campaign_id);
  const cases = inScope(input.cases ?? [], input.campaign);
  const adjs = input.adjudications ?? [];
  const agreement = caseAgreement(subs, cases);
  const anchors = anchorCoverage(subs, cases, input.campaign);
  const priorities = priorityFixes(subs, cases, adjs, { cards: input.cards });
  const crit = unresolvedCritical(priorities);
  const byKind = { case_review: 0, method_feedback: 0, framework_answer: 0, decision_use: 0, threat_review: 0, data_issue: 0, break_model: 0 } as Record<SubmissionKind, number>;
  for (const s of subs) byKind[s.kind] = (byKind[s.kind] ?? 0) + 1;
  const reviewed = agreement.filter((a) => a.n_reviews > 0);
  const caseById = new Map(cases.map((c) => [c.case_id, c]));
  const sectors = new Set<string>();
  const countries = new Set<string>();
  for (const a of reviewed) {
    const c = caseById.get(a.case_id);
    if (!c) continue;
    const t = caseTags(c);
    t.sectors.forEach((x) => sectors.add(x));
    t.countries.forEach((x) => countries.add(x));
  }
  const dataReports = priorities.filter((p) => p.source === 'data_issue' || p.source === 'break_model');
  return {
    overview: {
      n_submissions: subs.length,
      n_reviewers: new Set(subs.map((s) => s.reviewer.reviewer_id)).size,
      by_kind: byKind,
      n_cases_in_scope: cases.length,
      n_cases_reviewed: reviewed.length,
      n_case_reviews: agreement.reduce((n, a) => n + a.n_reviews, 0),
      n_outside_answers: agreement.reduce((n, a) => n + a.n_outside, 0),
      n_anchor_cases: anchors.length,
      n_anchor_met: anchors.filter((a) => a.met).length,
      sectors_covered: [...sectors].sort(),
      countries_covered: [...countries].sort(),
      n_data_reports: dataReports.reduce((n, p) => n + p.n_reports, 0),
      n_open_data_reports: dataReports.filter((p) => p.unresolved).reduce((n, p) => n + p.n_reports, 0),
      n_unresolved_critical: crit.length,
      data_releases: [...new Set(subs.map((s) => s.data_release).filter((x): x is string => !!x))].sort(),
      n_snapshot_mismatch: agreement.reduce((n, a) => n + a.n_snapshot_mismatch, 0),
    },
    coverage: coverage(subs, cases),
    anchors,
    agreement,
    transitions: blindRevealTransitions(subs, cases),
    taxonomy: disagreementTaxonomy(subs),
    priorities,
    unresolved_critical: crit,
    methods: methodSummary(subs, input.cards ?? []),
    framework: frameworkSummary(subs, input.scenarios ?? []),
    decision_use: decisionUseSummary(subs),
    threat: threatSummary(subs),
    robustness: robustnessSummary(cases),
  };
}
