// What the admin dashboard summarises (review/aggregate.ts; design 6.6): agreement per case with the
// "too few reviews" rule, outside-expertise answers counted but kept out of denominators, blind-vs-reveal
// transitions, the disagreement taxonomy, priority fixes (one critical confirmed issue outranks ten
// cosmetic ones; adjudication overrides; corroboration capped at 1) and missing data.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME } from '../content/vocabulary.ts';
import { DONT_KNOW, type Adjudication, type Campaign, type CaseReviewPayload, type DataIssuePayload, type ReviewCase, type ReviewerStamp, type Severity, type Submission, type SubmissionKind, type Verdict } from './model.ts';
import {
  MIN_REVIEWS_TO_SUMMARISE,
  TOO_FEW_LABEL,
  anchorCoverage,
  blindRevealTransitions,
  breadthOf,
  buildDashboard,
  caseAgreement,
  coverage,
  disagreementTaxonomy,
  priorityFixes,
  unresolvedCritical,
} from './aggregate.ts';

const OPTIONS = [...Object.entries(CHOICE_NAME).map(([value, label]) => ({ value, label })), { value: 'not_assessed', label: 'Not assessed' }];
const rcase = (id: string, model: string, x: Partial<ReviewCase> = {}): ReviewCase => ({
  case_id: id,
  kind: 'posture_cell',
  key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
  title: `${id} title`,
  question: 'q',
  sector_groups: ['batteries'],
  country_groups: ['KOR'],
  stage: 'Components',
  assignment_group: 'batteries',
  anchor: false,
  why_selected: 'w',
  explorer_path: '/countries/KOR',
  context_fields: ['need'],
  reveal_fields: ['choice'],
  snapshot: { conclusion: { value: model, label: CHOICE_NAME[model as keyof typeof CHOICE_NAME] ?? model }, facts: {} },
  snapshot_sha256: `${id}`.padEnd(64, '0'),
  bundle: [],
  data_release: 'rel-0123456789ab',
  app_commit: 'abc',
  built_at: '2027-01-10T00:00:00Z',
  answer_options: OPTIONS,
  ...x,
});

const CASES = [rcase('CASE_001', 'partner', { anchor: true }), rcase('CASE_002', 'secure', { sector_groups: ['solar'], country_groups: ['IND'], key: { iso3: 'IND', tech: 'Solar', stage: 'Refining' }, stage: 'Refining' }), rcase('CASE_003', 'watch')];

const reviewer = (i: number, conf = 4, sectors = ['batteries'], countries = ['KOR']): ReviewerStamp => ({
  reviewer_id: `R${i}`,
  name: null,
  anonymous: true,
  profile_digest: `d${i}`,
  expertise: { sectors, countries, stages: [], methods: [], confidence: Object.fromEntries([...sectors.map((s) => [`sector:${s}`, conf]), ...countries.map((c) => [`country:${c}`, conf])]) },
});

let seq = 0;
function env<K extends SubmissionKind>(kind: K, payload: Submission<K>['payload'], r: ReviewerStamp, extra: Partial<Submission> = {}): Submission<K> {
  seq++;
  const at = new Date(Date.UTC(2027, 0, 20, 9, seq)).toISOString();
  return { submission_id: `SUB-${String(seq).padStart(4, '0')}`, schema: 'aib-review-submission', schema_version: 1, campaign_id: 'round1', kind, reviewer: r, created_at: at, submitted_at: at, app_commit: 'abc', data_release: 'rel-0123456789ab', page_context: null, payload, ...extra } as Submission<K>;
}
const review = (r: ReviewerStamp, caseId: string, blind: string, verdict: Verdict, issue?: CaseReviewPayload['reveal']['main_issue'], match: CaseReviewPayload['expertise_match'] = 'in') =>
  env('case_review', {
    case_id: caseId,
    case_snapshot_sha256: CASES.find((c) => c.case_id === caseId)?.snapshot_sha256 ?? 'x',
    assignment_reason: 'assigned',
    expertise_match: match,
    blind: { choice: blind, confidence: blind === DONT_KNOW ? null : 4, factors: [], answered_at: '2027-01-20T08:00:00Z' },
    reveal: { revealed_at: '2027-01-20T08:01:00Z', verdict, main_issue: issue ?? null },
    outside_expertise: blind === DONT_KNOW || verdict === 7,
    time_spent_s: 100,
  }, r);
const dataIssue = (r: ReviewerStamp, severity: Severity, metricId: string, what = 'wrong', country = 'KOR') =>
  env('data_issue', { metric: { id: metricId, label: metricId, value: 0.3, display: '30%', country, tech: 'Batteries', stage: 'Components' }, what_is_wrong: what, severity } as DataIssuePayload, r, {
    page_context: { url: '/countries/KOR', route: '/countries/:iso3', view: 'country', country, tech: 'batteries', stage: 'Components', node: 'Batteries|Components', layer: null, pair: null, sid: null, scenario: null, displayed_result: null, metric_ids: [metricId], data_release: null, bundle_stamps: {}, app_commit: 'abc', captured_at: '2027-01-20T09:00:00Z' },
  });
const adj = (submission_id: string, status: Adjudication['status'], x: Partial<Adjudication> = {}): Adjudication => ({ adjudication_id: `ADJ-${submission_id}`, submission_id, status, issue_type: 'data', confidence_real: null, affected_outputs: null, decided_by: 'NEIS', decided_at: '2027-02-01T00:00:00Z', ...x });

describe('caseAgreement', () => {
  const subs = [
    review(reviewer(1), 'CASE_001', 'partner', 1),
    review(reviewer(2), 'CASE_001', 'partner', 1),
    review(reviewer(3), 'CASE_001', 'secure', 2),
    review(reviewer(4), 'CASE_001', 'secure', 5, { text: 'knife-edge', type: 'method' }),
    review(reviewer(5), 'CASE_001', DONT_KNOW, 7, null, 'outside'),
    review(reviewer(6), 'CASE_001', 'watch', 7, null, 'adjacent'),
    review(reviewer(1), 'CASE_002', 'secure', 1),
    review(reviewer(2), 'CASE_002', 'watch', 4, { text: 're-exports', type: 'data' }),
  ];

  it('summarises a case with enough interpretable reviews; outside answers counted, not in denominators', () => {
    const a = caseAgreement(subs, CASES).find((x) => x.case_id === 'CASE_001')!;
    expect(a).toMatchObject({ n_reviews: 6, n_dont_know: 1, n_verdict7: 2, n_outside: 2, n_interpretable: 4, too_few: false, summary_label: null });
    expect(a.blind_distribution).toEqual({ partner: 2, secure: 2 });
    expect(a.modal).toEqual(['secure', 'partner']); // a tie, in answer-option order
    expect(a.share_matching_model).toBe(0.5);
    expect(a.share_persuaded).toBe(0.25);
    expect(a.share_rejecting).toBe(0.25);
    expect(a.modal_share).toBe(0.5);
    expect(a.issue_types).toEqual({ method: 1 });
  });

  it('labels a case with fewer than 3 interpretable reviews "too few" and gives no shares', () => {
    const a = caseAgreement(subs, CASES).find((x) => x.case_id === 'CASE_002')!;
    expect(MIN_REVIEWS_TO_SUMMARISE).toBe(3);
    expect(a).toMatchObject({ n_reviews: 2, too_few: true, summary_label: TOO_FEW_LABEL, share_matching_model: null, share_persuaded: null, share_rejecting: null, modal_share: null });
    expect(a.n_matching_model).toBe(1);
  });

  it('counts a resubmission once (the latest per reviewer and case)', () => {
    const again = review(reviewer(1), 'CASE_001', 'secure', 2);
    const a = caseAgreement([...subs, again], CASES).find((x) => x.case_id === 'CASE_001')!;
    expect(a.n_reviews).toBe(6);
    expect(a.blind_distribution).toEqual({ partner: 1, secure: 3 });
  });

  it('handles missing data: no reviews, an unknown case, a review without a model', () => {
    const none = caseAgreement([], CASES);
    expect(none.map((a) => a.n_reviews)).toEqual([0, 0, 0]);
    expect(none.every((a) => a.too_few)).toBe(true);
    const ghost = caseAgreement([review(reviewer(1), 'CASE_999', 'secure', 1)], CASES).find((a) => a.case_id === 'CASE_999')!;
    expect(ghost).toMatchObject({ model_choice: null, n_reviews: 1, share_matching_model: null, kind: null });
    expect(buildDashboard({ submissions: [], cases: [] }).overview.n_submissions).toBe(0);
    // a truncated review (no blind answer, or no verdict) is left out, never a throw
    const broken = review(reviewer(8), 'CASE_003', 'watch', 1);
    delete (broken.payload as Partial<CaseReviewPayload>).blind;
    const noVerdict = review(reviewer(9), 'CASE_003', 'watch', 1);
    (noVerdict.payload as { reveal: unknown }).reveal = { revealed_at: 'x' };
    const a3 = caseAgreement([broken, noVerdict], CASES).find((a) => a.case_id === 'CASE_003')!;
    expect(a3.n_reviews).toBe(0);
    expect(() => buildDashboard({ submissions: [broken, noVerdict], cases: CASES })).not.toThrow();
  });

  it('flags reviews made against another snapshot', () => {
    const old = review(reviewer(7), 'CASE_003', 'watch', 1);
    (old.payload as CaseReviewPayload).case_snapshot_sha256 = 'stale';
    expect(caseAgreement([old], CASES).find((a) => a.case_id === 'CASE_003')!.n_snapshot_mismatch).toBe(1);
  });
});

describe('transitions and taxonomy', () => {
  it('classifies blind-vs-reveal moves and catches contradictory verdicts', () => {
    const subs = [
      review(reviewer(1), 'CASE_001', 'partner', 1),
      review(reviewer(2), 'CASE_001', 'secure', 2),
      review(reviewer(3), 'CASE_001', 'secure', 6, { text: 'context', type: 'context' }),
      review(reviewer(4), 'CASE_001', 'partner', 2),
      review(reviewer(5), 'CASE_001', DONT_KNOW, 7),
    ];
    const t = blindRevealTransitions(subs, CASES);
    expect(t.counts).toMatchObject({ agreed_confirmed: 1, differed_persuaded: 1, differed_rejecting: 1, inconsistent: 1, outside: 1 });
    expect(t.inconsistent[0]).toMatchObject({ case_id: 'CASE_001', blind: 'partner', model: 'partner', verdict: 2 });
  });

  it('counts verdict issue types, framework mismatches and low actionability', () => {
    const r = reviewer(1);
    const subs = [
      review(r, 'CASE_001', 'secure', 4, { text: 'd', type: 'data' }),
      review(reviewer(2), 'CASE_001', 'secure', 3),
      env('framework_answer', { scenario_id: 'FS_001', answer: 'secure', intended_answer: 'watch', correct: false, confidence: 3, confusing_terms: ['fence'] }, r),
      env('framework_answer', { scenario_id: 'FS_001', answer: DONT_KNOW, intended_answer: 'watch', correct: false, confidence: null, confusing_terms: [] }, r),
      env('decision_use', { case_id: 'CASE_001', would_do_differently: 'x', actionable: 1, identifies_problem: 3, separates_diagnosis_from_prescription: 2, right_geography: 4 }, r),
      env('decision_use', { case_id: 'CASE_001', would_do_differently: 'x', actionable: 4, identifies_problem: 3, separates_diagnosis_from_prescription: 2, right_geography: 4 }, r),
    ];
    const tax = Object.fromEntries(disagreementTaxonomy(subs).map((x) => [x.issue_type, x.total]));
    expect(tax).toEqual({ data: 1, method: 0, context: 0, framework: 1, actionability: 1, insufficient_evidence: 1, decision_rule: 0 });
  });
});

describe('priorityFixes (materiality x confidence-real x breadth)', () => {
  it('ranks one critical confirmed issue above ten cosmetic ones, whatever their frequency', () => {
    const crit = dataIssue(reviewer(1, 3), 'critical', 'home_share', 're-exports counted as production');
    const cosmetic = Array.from({ length: 10 }, (_, i) => dataIssue(reviewer(10 + i, 5), 'cosmetic', 'axis_label', 'typo'));
    const cosmeticSpread = Array.from({ length: 10 }, (_, i) => dataIssue(reviewer(30 + i, 5), 'cosmetic', `label_${i}`, 'typo'));
    const p = priorityFixes([...cosmetic, ...cosmeticSpread, crit], CASES, [adj(crit.submission_id, 'confirmed', { confidence_real: 0.9 })]);
    expect(p[0].submission_ids).toEqual([crit.submission_id]);
    expect(p[0]).toMatchObject({ severity: 'critical', confidence_basis: 'adjudication', confidence_real: 0.9, adjudication_status: 'confirmed', unresolved: true });
    const grouped = p.find((x) => x.metric_id === 'axis_label')!;
    expect(grouped.n_reports).toBe(10);
    expect(grouped.n_reviewers).toBe(10);
    expect(grouped.confidence_real).toBeLessThanOrEqual(1);
    expect(p[0].score).toBeGreaterThan(grouped.score);
    expect(unresolvedCritical(p).map((x) => x.group_id)).toEqual([p[0].group_id]);
  });

  it('raises confidence with independent corroboration, capped at 1, and weights by expertise match', () => {
    const one = priorityFixes([dataIssue(reviewer(1, 3), 'material', 'cp')], CASES)[0];
    const two = priorityFixes([dataIssue(reviewer(1, 3), 'material', 'cp'), dataIssue(reviewer(2, 3), 'material', 'cp')], CASES)[0];
    expect(one.confidence_real).toBeCloseTo(0.6); // 3/5 x in (1.0)
    expect(two.confidence_real).toBeCloseTo(1 - 0.4 * 0.4);
    const many = priorityFixes(Array.from({ length: 30 }, (_, i) => dataIssue(reviewer(i, 5), 'material', 'cp')), CASES)[0];
    expect(many.confidence_real).toBe(1);
    // the same reviewer reporting twice is not corroboration
    const same = priorityFixes([dataIssue(reviewer(1, 3), 'material', 'cp'), dataIssue(reviewer(1, 3), 'material', 'cp')], CASES)[0];
    expect(same.confidence_real).toBeCloseTo(0.6);
    // outside the reporter's expertise (solar / India): weight 0.4 and the default confidence
    const outside = priorityFixes([dataIssue(reviewer(1, 3), 'material', 'cp', 'x', 'IND')], CASES)[0];
    expect(outside.confidence_real).toBeLessThan(one.confidence_real);
  });

  it('lets the adjudication override: rejected issues score zero, breadth follows affected outputs', () => {
    const s = dataIssue(reviewer(1, 5), 'critical', 'need');
    const rejected = priorityFixes([s], CASES, [adj(s.submission_id, 'rejected')])[0];
    expect(rejected).toMatchObject({ confidence_real: 0, score: 0, unresolved: false });
    expect(unresolvedCritical([rejected])).toEqual([]);
    const broad = priorityFixes([s], CASES, [adj(s.submission_id, 'confirmed', { confidence_real: 1, affected_outputs: 63 })])[0];
    expect(broad.breadth).toBe(breadthOf(63));
    expect(broad.breadth).toBe(7);
    expect(broad.score).toBe(7);
    const resolved = priorityFixes([s], CASES, [adj(s.submission_id, 'confirmed', { decided_at: '2027-02-01T00:00:00Z' }), adj(s.submission_id, 'resolved', { decided_at: '2027-03-01T00:00:00Z', adjudication_id: 'ADJ-2' })])[0];
    expect(resolved.adjudication_status).toBe('resolved');
    expect(resolved.unresolved).toBe(false);
  });

  it('merges reports an adjudication marks as duplicates', () => {
    const a = dataIssue(reviewer(1), 'material', 'cp');
    const b = dataIssue(reviewer(2), 'material', 'prize');
    const p = priorityFixes([a, b], CASES, [adj(b.submission_id, 'duplicate', { duplicate_of: a.submission_id })]);
    expect(p).toHaveLength(1);
    expect(p[0].submission_ids.sort()).toEqual([a.submission_id, b.submission_id].sort());
  });

  it('includes case-review rejections and method rejections; verdict 7 never', () => {
    const subs = [
      review(reviewer(1), 'CASE_001', 'secure', 5, { text: 'threshold', type: 'method' }),
      review(reviewer(2), 'CASE_001', 'secure', 7),
      review(reviewer(3), 'CASE_001', DONT_KNOW, 4, { text: 'odd data', type: 'data' }, 'outside'),
      env('method_feedback', { card_id: 'MC_001', stance: 'reject', materiality: 4, bias_direction: 'favours_incumbents', outside_expertise: false }, reviewer(4)),
      env('method_feedback', { card_id: 'MC_002', stance: 'retain', materiality: 1, bias_direction: 'none', outside_expertise: false }, reviewer(4)),
    ];
    const p = priorityFixes(subs, CASES);
    expect(p.map((x) => x.source).sort()).toEqual(['case_review', 'case_review', 'method_feedback']);
    const dk = p.find((x) => x.issue_type === 'data')!;
    expect(dk.confidence_real).toBeCloseTo(0.5 * 0.4); // no blind confidence, outside weight
    expect(p.find((x) => x.source === 'method_feedback')!.severity).toBe('critical');
  });
});

describe('coverage and the dashboard', () => {
  const campaign: Campaign = {
    campaign_id: 'round1',
    name: 'R1',
    status: 'active',
    review_types: ['case'],
    case_ids: ['CASE_001', 'CASE_002', 'CASE_003'],
    anchor_cases: ['CASE_001'],
    target_reviews_per_anchor: 3,
    assignment: { assigned_per_reviewer: { min: 2, max: 4 }, anchors_per_reviewer: 1, out_of_specialty: 'none', allow_self_selected: false, min_domain_confidence: 3 },
  };
  const subs = [review(reviewer(1), 'CASE_001', 'partner', 1), review(reviewer(2), 'CASE_001', 'partner', 1), review(reviewer(3), 'CASE_001', DONT_KNOW, 7)];

  it('measures anchor coverage on interpretable reviews only', () => {
    expect(anchorCoverage(subs, CASES, campaign)).toEqual([{ case_id: 'CASE_001', title: 'CASE_001 title', target: 3, n_reviews: 3, n_interpretable: 2, remaining: 1, met: false }]);
  });
  it('counts coverage by kind, sector, country and stage', () => {
    const c = coverage(subs, CASES, campaign);
    expect(c.find((r) => r.dimension === 'sector' && r.key === 'batteries')).toMatchObject({ n_cases: 2, n_cases_reviewed: 1, n_reviews: 3, n_interpretable: 2 });
    expect(c.find((r) => r.dimension === 'country' && r.key === 'IND')).toMatchObject({ n_cases: 1, n_cases_reviewed: 0 });
    expect(c.find((r) => r.dimension === 'kind')).toMatchObject({ key: 'posture_cell', n_cases: 3 });
  });
  it('builds every section from one call and filters to the campaign', () => {
    const other = env('case_review', { ...(subs[0].payload as CaseReviewPayload) }, reviewer(9), { campaign_id: 'other' });
    const d = buildDashboard({ submissions: [...subs, other], cases: CASES, campaign });
    expect(d.overview).toMatchObject({ n_submissions: 3, n_reviewers: 3, n_cases_in_scope: 3, n_cases_reviewed: 1, n_outside_answers: 1, n_anchor_cases: 1, n_anchor_met: 0, sectors_covered: ['batteries'], countries_covered: ['KOR'], data_releases: ['rel-0123456789ab'] });
    expect(d.robustness.counts.missing).toBe(3);
  });
});
