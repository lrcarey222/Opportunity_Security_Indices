// UX v2 exports (review/exports.ts): the v2 columns of qc_review_responses and qc_case_results (after the v1
// columns, empty for v1 reviews), qc_evidence_gaps.csv, names dropped by default and the formula guard on the new
// reviewer free text.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME } from '../content/vocabulary.ts';
import { INSUFFICIENT_EVIDENCE, NONE_OF_THESE, REVIEWER_STATES, REVIEW_VERDICT_LEGACY, type CaseReviewPayload, type ReviewCase, type ReviewerStamp, type Submission } from './model.ts';
import { CASE_RESULT_ALL_COLUMNS, EVIDENCE_GAP_COLUMNS, PRODUCT_ANSWER_COLUMNS, REVIEW_RESPONSE_ALL_COLUMNS, caseResultRows, csvFiles, evidenceGapRows, reviewResponseRows } from './exports.ts';
import { pseudonymReviewerCode } from './privacy.ts';

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
  explorer_path: '/x',
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
const CASES = [rcase('CASE_001', 'partner'), rcase('CASE_018', 'Sectoral', { kind: 'partnership', key: { pair: 'USA_JPN' } })];
const stamp = (i: number, name: string | null = null, anonymous = true): ReviewerStamp => ({ reviewer_id: `R${i}`, name, anonymous, profile_digest: `d${i}` });
const T = '2027-01-20T08:00:30Z';
let seq = 0;
const sub = (r: ReviewerStamp, payload: CaseReviewPayload): Submission => {
  seq++;
  const at = new Date(Date.UTC(2027, 0, 20, 9, seq)).toISOString();
  return { submission_id: `SUB-${seq}`, schema: 'aib-review-submission', schema_version: 1, campaign_id: 'round1', kind: 'case_review', reviewer: r, created_at: at, submitted_at: at, app_commit: 'abc', data_release: 'rel-0123456789ab', page_context: null, payload };
};
const base = (caseId: string): Omit<CaseReviewPayload, 'blind' | 'reveal'> => ({ case_id: caseId, case_snapshot_sha256: caseId.padEnd(64, '0'), assignment_reason: 'assigned', expertise_match: 'in', outside_expertise: false, time_spent_s: 60 });

const V2_INS = sub(stamp(1, 'Named Person', false), {
  ...base('CASE_001'),
  blind: { choice: INSUFFICIENT_EVIDENCE, confidence: null, factors: [], answered_at: T, confidence_level: null, driver: null, missing_evidence: ['production_capacity', 'ownership'], missing_evidence_text: '=HYPERLINK("x")', ux_version: 2 },
  reveal: {
    revealed_at: T,
    verdict: REVIEW_VERDICT_LEGACY.wrong_rule,
    main_issue: { text: 'the bar', type: 'decision_rule' },
    verdict_code: 'wrong_rule',
    prior_differs: null,
    validity: 'not_supported',
    persuasion: 'unchanged',
    red_team: { must_be_true: '+allies qualify', resolving_evidence: 'qualification_constraints', resolving_evidence_text: 'lead times' },
  },
  notes: [
    { id: 'a', text: '@risky note', created_at: T, updated_at: T, converted_to: 'data_issue' },
    { id: 'b', text: 'plain', created_at: T, updated_at: T },
  ],
  review_mode: 'technical',
});
const V2_NONE = sub(stamp(2, 'Anon Person', true), {
  ...base('CASE_018'),
  blind: { choice: NONE_OF_THESE, confidence: 3, factors: ['other'], answered_at: T, confidence_level: 'medium', driver: 'other', driver_text: 'finance', ux_version: 2, own_recommendation: '-cmd stockpile' } as CaseReviewPayload['blind'],
  reveal: {
    revealed_at: T,
    verdict: 1,
    main_issue: null,
    verdict_code: 'same_same',
    prior_differs: null,
    validity: 'supported',
    persuasion: 'unchanged',
    red_team: null,
    product_answers: { complementarity: 'mostly_diplomatic', durability: 'fragile' },
  },
  review_mode: 'decision',
});
const V1 = sub(stamp(3), { ...base('CASE_001'), blind: { choice: 'partner', confidence: 4, factors: ['import_dependence'], answered_at: T }, reveal: { revealed_at: T, verdict: 1 } });
const SUBS = [V2_INS, V2_NONE, V1];

describe('qc_review_responses v2 columns', () => {
  it('fills the v2 columns for v2 reviews and leaves them empty for v1 reviews', () => {
    const rows = reviewResponseRows(SUBS, CASES);
    for (const r of rows) expect(Object.keys(r)).toEqual([...REVIEW_RESPONSE_ALL_COLUMNS]);
    const ins = rows.find((r) => r.submission_id === V2_INS.submission_id)!;
    expect(ins).toMatchObject({
      ux_version: 2,
      review_mode: 'technical',
      product: 'country',
      blind_state: INSUFFICIENT_EVIDENCE,
      blind_matches_model: null,
      outside_expertise: false,
      missing_evidence: 'production_capacity;ownership',
      verdict: 5,
      verdict_code: 'wrong_rule',
      validity: 'not_supported',
      persuasion: 'unchanged',
      prior_differs: null,
      issue_type: 'decision_rule',
      red_team_resolving_evidence: 'qualification_constraints',
      n_notes: 2,
      notes_data_issue: 1,
      notes_unconverted: 1,
      notes_text: '[data_issue] @risky note | [note] plain',
    });
    const none = rows.find((r) => r.submission_id === V2_NONE.submission_id)!;
    expect(none).toMatchObject({ product: 'partnership', blind_state: NONE_OF_THESE, own_recommendation: '-cmd stockpile', driver: 'other', driver_text: 'finance', product_complementarity: 'mostly_diplomatic', product_durability: 'fragile', product_incentives: null, n_notes: 0 });
    const v1 = rows.find((r) => r.submission_id === V1.submission_id)!;
    for (const c of ['ux_version', 'verdict_code', 'validity', 'persuasion', 'blind_state', 'n_notes', 'notes_text', ...PRODUCT_ANSWER_COLUMNS]) expect(v1[c], c).toBeNull();
    expect(v1).toMatchObject({ blind_matches_model: true, verdict: 1 });
  });

  it('qc_case_results appends the v2 counts', () => {
    const rows = caseResultRows(SUBS, CASES);
    for (const r of rows) expect(Object.keys(r)).toEqual([...CASE_RESULT_ALL_COLUMNS]);
    expect(rows.find((r) => r.case_id === 'CASE_001')).toMatchObject({ product: 'country', n_insufficient_evidence: 1, n_excluded: 1, n_interpretable: 1, verdict_code_counts: 'wrong_rule:1' });
    expect(rows.find((r) => r.case_id === 'CASE_018')).toMatchObject({ n_none_of_these: 1, n_interpretable: 0 });
  });
});

describe('qc_evidence_gaps', () => {
  it('has one row per gap mention with its source branch', () => {
    const rows = evidenceGapRows(SUBS, CASES);
    for (const r of rows) expect(Object.keys(r)).toEqual([...EVIDENCE_GAP_COLUMNS]);
    expect(rows.map((r) => [r.case_id, r.product, r.source, r.category])).toEqual([
      ['CASE_001', 'country', 'insufficient_evidence', 'production_capacity'],
      ['CASE_001', 'country', 'insufficient_evidence', 'ownership'],
      ['CASE_001', 'country', 'red_team', 'qualification_constraints'],
    ]);
    expect(rows[2]).toMatchObject({ text: 'lead times', category_label: 'Qualification constraints / lead times', reviewer_code: pseudonymReviewerCode('R1') });
  });

  it('drops names by default, adds a named reviewer only on request and never an anonymous one, and guards formulas', () => {
    const plain = csvFiles(SUBS, CASES, []);
    for (const f of plain) {
      expect(f.text).not.toMatch(/Named Person|Anon Person|reviewer_name/);
      expect(f.text).not.toMatch(/(^|,)"?[=+@]/m);
    }
    const gaps = plain.find((f) => f.name === 'qc_evidence_gaps.csv')!;
    expect(gaps.rows).toBe(3);
    expect(gaps.text).toContain(`'=HYPERLINK`);
    const resp = plain.find((f) => f.name === 'qc_review_responses.csv')!.text;
    expect(resp).toContain(`'+allies qualify`);
    expect(resp).toContain(`'-cmd stockpile`);
    const named = csvFiles(SUBS, CASES, [], { includeNames: true });
    const g2 = named.find((f) => f.name === 'qc_evidence_gaps.csv')!.text;
    expect(g2.split('\r\n')[0]).toMatch(/reviewer_code,reviewer_name,/);
    expect(g2).toMatch(/Named Person/);
    expect(named.map((f) => f.text).join('\n')).not.toMatch(/Anon Person/);
  });
});
