// UX v2 aggregation (review/aggregate.ts "UX v2"): the verdict taxonomy, prior x validity x persuasion and the
// persuaded group, evidence gaps (insufficient-evidence answers and red team) by category / case / product,
// reviewer states kept out of agreement denominators but reported, product tallies, notes, drivers, v2 priority
// fixes and the reviewer's completion summary, with v1 reviews mixed in.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME } from '../content/vocabulary.ts';
import {
  INSUFFICIENT_EVIDENCE,
  NONE_OF_THESE,
  OUTSIDE_EXPERTISE,
  REVIEWER_STATES,
  REVIEW_VERDICT_LEGACY,
  type CaseReviewPayload,
  type EvidenceGap,
  type ReviewCase,
  type ReviewerStamp,
  type ReviewVerdictCode,
  type Submission,
  type SubmissionKind,
} from './model.ts';
import {
  buildDashboard,
  caseAgreement,
  disagreementTaxonomy,
  evidenceGapMentions,
  evidenceGapSummary,
  noteSummary,
  driverSummary,
  priorPersuasion,
  priorityFixes,
  productQuestionTallies,
  reviewerStateSummary,
  reviewerSummary,
  verdictCodeSummary,
} from './aggregate.ts';
import type { BlindAnswerV2 } from './flow.ts';

const OPTIONS = [...Object.entries(CHOICE_NAME).map(([value, label]) => ({ value, label })), ...REVIEWER_STATES.map((value) => ({ value, label: value }))];
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
  context_fields: [],
  reveal_fields: [],
  snapshot: { conclusion: { value: model, label: model }, facts: {} },
  snapshot_sha256: id.padEnd(64, '0'),
  bundle: [],
  data_release: 'rel-0123456789ab',
  app_commit: 'abc',
  built_at: '2027-01-10T00:00:00Z',
  answer_options: OPTIONS,
  ...x,
});
const CHAIN = rcase('CASE_030', 'SC-SOLAR', {
  kind: 'sector_chain',
  key: { sid: 'SC-SOLAR', scenario: 'decoupling' },
  snapshot: {
    conclusion: { value: 'Assembly', label: 'Assembly' },
    facts: {},
    reasoning: { conclusion: { value: 'Assembly', label: 'Assembly', summary: '' }, steps: [], rule: '', alternatives: [], sector: { explorer_stages: ['Refining', 'Assembly'], lead_stage: 'Assembly' } },
  },
});
const COMPACT = rcase('CASE_018', 'Sectoral', { kind: 'compact', key: { sid: 'SC-BATT', scenario: 'decoupling' } });
const CASES = [rcase('CASE_001', 'partner'), rcase('CASE_002', 'secure'), COMPACT, CHAIN];

const stamp = (i: number): ReviewerStamp => ({ reviewer_id: `R${i}`, name: null, anonymous: true, profile_digest: `d${i}`, expertise: { sectors: ['batteries'], countries: ['KOR'], stages: [], methods: [], confidence: { 'sector:batteries': 4 } } });
let seq = 0;
function env<K extends SubmissionKind>(kind: K, payload: Submission<K>['payload'], r: ReviewerStamp, extra: Partial<Submission> = {}): Submission<K> {
  seq++;
  const at = new Date(Date.UTC(2027, 0, 20, 9, seq)).toISOString();
  return { submission_id: `SUB-${String(seq).padStart(4, '0')}`, schema: 'aib-review-submission', schema_version: 1, campaign_id: 'round1', kind, reviewer: r, created_at: at, submitted_at: at, app_commit: 'abc', data_release: 'rel-0123456789ab', page_context: null, payload, ...extra } as Submission<K>;
}

interface V2 {
  code: ReviewVerdictCode;
  validity?: CaseReviewPayload['reveal']['validity'];
  persuasion?: CaseReviewPayload['reveal']['persuasion'];
  prior?: boolean | null;
  blind?: Partial<BlindAnswerV2>;
  red?: { resolving_evidence?: EvidenceGap | null; resolving_evidence_text?: string | null; must_be_true?: string | null };
  issue?: CaseReviewPayload['reveal']['main_issue'];
  product_answers?: Record<string, string>;
  notes?: CaseReviewPayload['notes'];
}
const v2 = (i: number, caseId: string, choice: string, o: V2) =>
  env('case_review', {
    case_id: caseId,
    case_snapshot_sha256: caseId.padEnd(64, '0'),
    assignment_reason: 'assigned',
    expertise_match: 'in',
    blind: { choice, confidence: 3, factors: ['allied_alternatives'], answered_at: '2027-01-20T08:00:00Z', confidence_level: 'medium', driver: 'allied_alternatives', ux_version: 2, ...o.blind },
    reveal: {
      revealed_at: '2027-01-20T08:01:00Z',
      verdict: REVIEW_VERDICT_LEGACY[o.code],
      main_issue: o.issue ?? null,
      verdict_code: o.code,
      prior_differs: o.prior === undefined ? null : o.prior,
      validity: o.validity ?? null,
      persuasion: o.persuasion ?? null,
      red_team: o.red ? { must_be_true: null, resolving_evidence: null, resolving_evidence_text: null, ...o.red } : null,
      ...(o.product_answers ? { product_answers: o.product_answers } : {}),
    },
    outside_expertise: choice === OUTSIDE_EXPERTISE,
    time_spent_s: 100,
    notes: o.notes ?? [],
    review_mode: 'decision',
  }, stamp(i));
const v1 = (i: number, caseId: string, choice: string, verdict: 1 | 2 | 4 | 7) =>
  env('case_review', {
    case_id: caseId,
    case_snapshot_sha256: caseId.padEnd(64, '0'),
    assignment_reason: 'assigned',
    expertise_match: 'in',
    blind: { choice, confidence: 4, factors: [], answered_at: '2027-01-20T08:00:00Z' },
    reveal: { revealed_at: '2027-01-20T08:01:00Z', verdict, main_issue: verdict === 4 ? { text: 'data', type: 'data' } : null },
    outside_expertise: verdict === 7,
    time_spent_s: 100,
  }, stamp(i));

const T = '2027-01-20T08:00:30Z';
const SUBS = [
  v2(1, 'CASE_001', 'partner', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', prior: false }),
  v2(2, 'CASE_001', 'secure', { code: 'persuaded', validity: 'supported', persuasion: 'changed', prior: true }),
  v2(3, 'CASE_001', 'watch', { code: 'wrong_rule', validity: 'not_supported', persuasion: 'unchanged', prior: true, issue: { text: 'the bar is arbitrary', type: 'decision_rule' }, red: { resolving_evidence: 'qualification_constraints', must_be_true: 'x' } }),
  v2(4, 'CASE_001', NONE_OF_THESE, { code: 'missing_factor', validity: 'not_supported', persuasion: 'unchanged', issue: { text: 'stockpiles', type: 'context' }, blind: { own_recommendation: 'stockpile' } }),
  v2(5, 'CASE_001', INSUFFICIENT_EVIDENCE, { code: 'plausible_insufficient', validity: 'unsure', persuasion: 'unchanged', issue: { text: 'no plant data', type: 'insufficient_evidence' }, blind: { confidence_level: null, confidence: null, driver: null, factors: [], missing_evidence: ['production_capacity', 'ownership'], missing_evidence_text: 'plant list' } }),
  v2(6, 'CASE_001', OUTSIDE_EXPERTISE, { code: 'outside_expertise', blind: { confidence_level: null, confidence: null, driver: null, factors: [] } }),
  v1(7, 'CASE_001', 'partner', 1),
  v2(8, 'CASE_002', INSUFFICIENT_EVIDENCE, { code: 'plausible_insufficient', validity: 'unsure', persuasion: 'unchanged', issue: { text: 'x', type: 'insufficient_evidence' }, blind: { missing_evidence: ['production_capacity'] } }),
  v2(9, 'CASE_002', INSUFFICIENT_EVIDENCE, { code: 'outside_expertise', blind: { missing_evidence: ['project_pipeline'] }, red: { resolving_evidence_text: 'firm list' } }),
  v2(10, 'CASE_002', 'secure', { code: 'persuaded', validity: 'supported', persuasion: 'changed', prior: false }), // contradicts its own prior
  v2(11, 'CASE_018', 'Sectoral', { code: 'same_different_reasons', validity: 'partly', persuasion: 'somewhat', prior: false, issue: { text: 'scale, not count', type: 'method' }, product_answers: { ameliorates_threat: 'partly' }, notes: [{ id: 'a', text: 'coverage looks low', created_at: T, updated_at: T, converted_to: 'data_issue' }, { id: 'b', text: 'ok', created_at: T, updated_at: T }] }),
  v2(12, 'CASE_030', 'Assembly', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', blind: { choices: ['Assembly', 'Components'], driver: 'concentration_security' }, product_answers: { bottleneck_located: 'yes', bogus: 'x' } }),
];

describe('reviewer states', () => {
  it('are counted and reported but kept out of every agreement denominator', () => {
    const a = caseAgreement(SUBS, CASES).find((x) => x.case_id === 'CASE_001')!;
    expect(a.n_reviews).toBe(7);
    expect(a.n_none_of_these).toBe(1);
    expect(a.n_insufficient_evidence).toBe(1);
    expect(a.n_outside).toBe(1);
    expect(a.n_excluded).toBe(3);
    expect(a.n_interpretable).toBe(4);
    expect(a.blind_distribution).toEqual({ partner: 2, secure: 1, watch: 1 });
    expect(a.share_matching_model).toBe(0.5);
    const st = reviewerStateSummary(SUBS, CASES);
    expect(st.overall).toMatchObject({ none_of_these: 1, insufficient_evidence: 3, outside_expertise: 1, dont_know: 0 });
    expect(buildDashboard({ submissions: SUBS, cases: CASES }).transitions.counts.reviewer_state).toBe(3); // the fourth state answer judged outside expertise counts as outside
  });
});

describe('verdict taxonomy', () => {
  it('counts statements per case and overall; v1 reviews count as uncoded', () => {
    const v = verdictCodeSummary(SUBS, CASES);
    expect(v.n_coded).toBe(11);
    expect(v.n_uncoded).toBe(1);
    expect(v.overall).toMatchObject({ same_same: 2, persuaded: 2, wrong_rule: 1, missing_factor: 1, plausible_insufficient: 2, outside_expertise: 2, same_different_reasons: 1, wrong_fact: 0 });
    const c1 = v.by_case.find((r) => r.case_id === 'CASE_001')!;
    expect(c1).toMatchObject({ n_reviews: 7, n_coded: 6, n_disagreeing: 3, product: 'country' });
    expect(caseAgreement(SUBS, CASES).find((x) => x.case_id === 'CASE_001')!.verdict_code_counts).toMatchObject({ same_same: 1, persuaded: 1 });
  });

  it('maps statements to issue types for the taxonomy and the priority fixes', () => {
    const tax = Object.fromEntries(disagreementTaxonomy(SUBS).map((t) => [t.issue_type, t.from_case_reviews]));
    expect(tax).toMatchObject({ decision_rule: 1, context: 1, insufficient_evidence: 2, method: 1, data: 0 });
    const pri = priorityFixes(SUBS, CASES);
    const rule = pri.find((p) => p.group_id === 'case:CASE_001:decision_rule')!;
    expect(rule.severity).toBe('material');
    expect(pri.find((p) => p.group_id === 'case:CASE_018:method')!.severity).toBe('minor');
    expect(pri.some((p) => p.group_id.startsWith('case:CASE_030'))).toBe(false); // agreeing statements raise nothing
  });
});

describe('prior, validity and persuasion', () => {
  it('tables v2 reviews and highlights experts the evidence persuaded', () => {
    const pp = priorPersuasion(SUBS, CASES);
    expect(pp.persuaded_group.map((g) => [g.case_id, g.blind, g.model])).toEqual([['CASE_001', 'secure', 'partner']]);
    expect(pp.inconsistent).toEqual([expect.objectContaining({ case_id: 'CASE_002', verdict_code: 'persuaded', prior: 'same' })]);
    expect(pp.table.find((r) => r.prior === 'different' && r.validity === 'supported' && r.persuasion === 'changed')?.n).toBe(1);
    expect(pp.table.filter((r) => r.prior === 'reviewer_state').reduce((n, r) => n + r.n, 0)).toBe(5);
    expect(pp.by_case.find((r) => r.case_id === 'CASE_001')).toMatchObject({ n_coded: 6, n_prior_same: 1, n_prior_different: 2, n_reviewer_state: 3, n_persuaded_group: 1 });
    expect(caseAgreement(SUBS, CASES).find((x) => x.case_id === 'CASE_001')!.n_prior_differs_persuaded).toBe(1);
  });

  it('recomputes the prior from the case when a payload carries none (the sector chain lead stage)', () => {
    const pp = priorPersuasion(SUBS, CASES);
    expect(pp.by_case.find((r) => r.case_id === 'CASE_030')).toMatchObject({ n_prior_same: 1 });
    expect(caseAgreement(SUBS, CASES).find((x) => x.case_id === 'CASE_030')!.mean_stage_jaccard).toBeCloseTo(1 / 3);
  });
});

describe('evidence gaps', () => {
  it('lists each mention with its source branch and category', () => {
    const m = evidenceGapMentions(SUBS, CASES);
    expect(m.filter((x) => x.source === 'insufficient_evidence').map((x) => [x.case_id, x.category])).toEqual([
      ['CASE_001', 'production_capacity'],
      ['CASE_001', 'ownership'],
      ['CASE_002', 'production_capacity'],
      ['CASE_002', 'project_pipeline'],
    ]);
    expect(m.filter((x) => x.source === 'red_team').map((x) => [x.case_id, x.category, x.text])).toEqual([
      ['CASE_001', 'qualification_constraints', null],
      ['CASE_002', 'other', 'firm list'],
    ]);
    expect(m.find((x) => x.category === 'ownership')!.text).toBe('plant list');
  });

  it('aggregates by category, case and product and flags a repeated inability to judge', () => {
    const g = evidenceGapSummary(SUBS, CASES);
    expect(g.n_mentions).toBe(6);
    expect(g.n_insufficient_evidence_answers).toBe(3);
    expect(g.by_category[0]).toMatchObject({ category: 'production_capacity', n_insufficient_evidence: 2, n_red_team: 0, total: 2, n_cases: 2 });
    const c2 = g.by_case.find((r) => r.case_id === 'CASE_002')!;
    expect(c2).toMatchObject({ n_insufficient_evidence_answers: 2, repeated_inability: true, share_insufficient: 2 / 3 });
    expect(g.by_case[0].case_id).toBe('CASE_002'); // the most insufficient-evidence answers first
    expect(g.by_case.find((r) => r.case_id === 'CASE_001')!.repeated_inability).toBe(false);
    expect(g.by_product).toEqual([{ product: 'country', n_mentions: 6, gaps: { production_capacity: 2, ownership: 1, qualification_constraints: 1, project_pipeline: 1, other: 1 } }]);
  });

  it('ignores missing-evidence categories on an answer that is not "insufficient evidence"', () => {
    const odd = v2(20, 'CASE_001', 'partner', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', blind: { missing_evidence: ['ownership'] } });
    expect(evidenceGapMentions([odd], CASES)).toEqual([]);
  });
});

describe('product questions, notes and drivers', () => {
  it('tallies product answers per product and case, ignoring unknown ids', () => {
    const t = productQuestionTallies(SUBS, CASES);
    expect(t.find((q) => q.question_id === 'ameliorates_threat')).toMatchObject({ product: 'compact', n: 1, counts: { yes: 0, partly: 1, no: 0, unsure: 0 }, by_case: { CASE_018: { partly: 1 } } });
    expect(t.find((q) => q.question_id === 'bottleneck_located')!.n).toBe(1);
    expect(t.find((q) => q.question_id === 'complementarity')!.n).toBe(0);
  });

  it('counts notes by conversion and drivers overall and when the prior differs', () => {
    const n = noteSummary(SUBS, CASES);
    expect(n).toMatchObject({ n_notes: 2, n_reviews_with_notes: 1, by_conversion: { data_issue: 1, unconverted: 1, comment: 0 } });
    const d = driverSummary(SUBS, CASES);
    expect(d.overall.allied_alternatives).toBe(8);
    expect(d.overall.concentration_security).toBe(1);
    expect(d.when_prior_differs.allied_alternatives).toBe(2);
  });

  it('the dashboard carries every v2 section, and v1-only data gives empty ones', () => {
    const d = buildDashboard({ submissions: SUBS, cases: CASES });
    expect(d.overview.n_v2_reviews).toBe(11);
    expect(d.overview.n_reviewer_state_answers).toBe(4);
    expect(d.v2.evidence_gaps.n_mentions).toBe(6);
    const old = buildDashboard({ submissions: [v1(1, 'CASE_001', 'partner', 1), v1(2, 'CASE_001', 'secure', 4)], cases: CASES });
    expect(old.v2.verdict_codes.n_coded).toBe(0);
    expect(old.v2.evidence_gaps.n_mentions).toBe(0);
    expect(old.v2.prior_persuasion.table).toEqual([]);
    expect(old.agreement.find((a) => a.case_id === 'CASE_001')).toMatchObject({ n_interpretable: 2, n_excluded: 0 });
  });
});

describe('reviewerSummary', () => {
  it('counts what the completion screen says', () => {
    const mine = [
      v2(1, 'CASE_001', 'partner', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', prior: false }),
      v2(1, 'CASE_002', 'watch', { code: 'persuaded', validity: 'supported', persuasion: 'changed', prior: true }),
      v2(1, 'CASE_018', 'Sectoral', { code: 'wrong_fact', validity: 'not_supported', persuasion: 'unchanged', prior: false, issue: { text: 'x', type: 'data' } }),
      v2(1, 'CASE_030', INSUFFICIENT_EVIDENCE, { code: 'plausible_insufficient', validity: 'unsure', persuasion: 'unchanged', issue: { text: 'x', type: 'insufficient_evidence' }, blind: { missing_evidence: ['ownership'] } }),
    ];
    const s = reviewerSummary(mine, CASES);
    expect(s).toMatchObject({ n_cases_reviewed: 4, n_matched_initial: 2, n_changed_view: 1, n_raised_data_issue: 1, n_insufficient_evidence: 1, n_outside_expertise: 0, n_data_reports: 0 });
    expect(s.case_ids.data_issue).toEqual(['CASE_018']);
  });

  it('counts a data note or a data report filed from a case as a potential data issue', () => {
    const note = v2(1, 'CASE_001', 'partner', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', prior: false, notes: [{ id: 'n', text: 'share is off', created_at: T, updated_at: T, converted_to: 'data_issue' }] });
    const report = env('data_issue', { metric: null, what_is_wrong: 'x', severity: 'minor' }, stamp(1), {
      page_context: { url: 'https://x/review/case/CASE_002?c=1', route: '/review/case/:caseId', view: 'review-case', country: null, tech: null, stage: null, node: null, layer: null, pair: null, sid: null, scenario: null, displayed_result: null, metric_ids: [], data_release: null, bundle_stamps: {}, app_commit: 'abc', captured_at: T },
    });
    const c2 = v2(1, 'CASE_002', 'secure', { code: 'same_same', validity: 'supported', persuasion: 'unchanged', prior: false });
    const s = reviewerSummary([note, report, c2], CASES);
    expect(s.case_ids.data_issue).toEqual(['CASE_001', 'CASE_002']);
    expect(s.n_data_reports).toBe(1);
  });

  it('reads v1 reviews too (verdict 2 = changed view, 4 = data)', () => {
    const s = reviewerSummary([v1(1, 'CASE_001', 'secure', 2), v1(1, 'CASE_002', 'watch', 4), v1(1, 'CASE_018', DONT_KNOW_V1, 7)], CASES);
    expect(s).toMatchObject({ n_cases_reviewed: 3, n_changed_view: 1, n_raised_data_issue: 1, n_outside_expertise: 1, n_matched_initial: 0 });
  });
});

const DONT_KNOW_V1 = 'dont_know';
