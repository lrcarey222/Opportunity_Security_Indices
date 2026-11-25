// The review exports (review/exports.ts) and the export CLI (scripts/review/export-review.mjs): fixed
// column orders, joins on stable IDs, pseudonymous reviewers (names only on request, never anonymous
// ones), and the CLI's refusal to write private data into a git-tracked directory.

import { afterAll, describe, expect, it } from 'vitest';
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { CHOICE_NAME } from '../content/vocabulary.ts';
import type { FrameworkScenario, MethodCard, ReviewCase } from './model.ts';
import {
  CASE_RESULT_ALL_COLUMNS,
  CASE_RESULT_COLUMNS,
  DATA_ISSUE_COLUMNS,
  EVIDENCE_GAP_COLUMNS,
  REVIEW_RESPONSE_ALL_COLUMNS,
  REVIEW_RESPONSE_COLUMNS,
  caseResultRows,
  csvFiles,
  dataIssueRows,
  frameworkAnswerRows,
  neutraliseFormula,
  reviewResponseRows,
} from './exports.ts';
import { makeDemoSubmissions } from './demo.ts';
import { canonicalAnswerKeys, validateSubmission } from './manifest.ts';
import { pseudonymReviewerCode } from './privacy.ts';
// @ts-expect-error: a plain ES module CLI without type declarations
import { run, checkOutputDir } from '../../scripts/review/export-review.mjs';

// the canonical blind options of a posture cell as the case validator currently defines them (manifest.ts)
const OPTIONS = canonicalAnswerKeys('posture_cell').map((value) => ({ value, label: CHOICE_NAME[value as keyof typeof CHOICE_NAME] ?? value }));
const rcase = (id: string, model: string, x: Partial<ReviewCase> = {}): ReviewCase => ({
  case_id: id,
  kind: 'posture_cell',
  key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
  title: `${id}, a title`,
  question: 'q',
  sector_groups: ['batteries'],
  country_groups: ['KOR'],
  stage: 'Components',
  assignment_group: 'batteries',
  anchor: false,
  why_selected: 'w',
  explorer_path: `/countries/KOR?node=Batteries%7CComponents`,
  context_fields: ['import_dependence'],
  reveal_fields: ['choice'],
  snapshot: {
    conclusion: { value: model, label: CHOICE_NAME[model as keyof typeof CHOICE_NAME] },
    facts: { import_dependence: { id: 'import_dependence', label: 'Import dependence', value: 0.4, display: '40%' }, choice: { id: 'choice', label: 'Choice', value: model, display: model } },
  },
  snapshot_sha256: id.toLowerCase().replace(/[^0-9a-f]/g, 'a').padEnd(64, '0'),
  bundle: [{ file: 'posture/posture-core.f665dc19.json', sha256: 'e'.repeat(64), created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' }],
  data_release: 'rel-0123456789ab',
  app_commit: 'abc1234',
  built_at: '2027-01-10T00:00:00Z',
  answer_options: OPTIONS,
  ...x,
});
const CASES = [rcase('CASE_001', 'partner', { anchor: true, decision_use: true, threat: true }), rcase('CASE_002', 'secure'), rcase('CASE_003', 'watch')];
const CARDS = [
  { card_id: 'MC_001', title: 'Need floor', area: 'need', current_choice: 'c', why: 'w', example: { text: 'e' }, alternatives: ['a'], affected_outputs: ['x', 'y', 'z'], code_refs: [], method_tags: ['indices_statistics'] },
  { card_id: 'MC_002', title: 'CP bar', area: 'position', current_choice: 'c', why: 'w', example: { text: 'e' }, alternatives: ['a'], affected_outputs: ['x'], code_refs: [], method_tags: ['trade_data'] },
] as MethodCard[];
const SCENARIOS = [{ scenario_id: 'FS_001', concept: 'fence', prompt: 'p', options: OPTIONS.slice(0, 6), intended_answer: 'watch', explanation: 'e', terms: ['fence'] }] as FrameworkScenario[];

const SUBS = makeDemoSubmissions(CASES, CARDS, SCENARIOS, 'export-test', 'round1');
const header = (text: string) => text.split('\r\n')[0];

describe('row builders', () => {
  it('qc_review_responses: one row per case review, fixed columns, model choice and match joined from the case', () => {
    const rows = reviewResponseRows(SUBS, CASES);
    expect(rows.length).toBe(SUBS.filter((s) => s.kind === 'case_review').length);
    for (const r of rows) expect(Object.keys(r)).toEqual([...REVIEW_RESPONSE_ALL_COLUMNS]);
    // the v1 columns stay first, in the v1 order
    expect(REVIEW_RESPONSE_ALL_COLUMNS.slice(0, REVIEW_RESPONSE_COLUMNS.length)).toEqual([...REVIEW_RESPONSE_COLUMNS]);
    const r1 = rows.find((r) => r.case_id === 'CASE_001' && r.verdict === 1)!;
    expect(r1).toMatchObject({ model_choice: 'partner', model_label: 'Partner', blind_matches_model: true, snapshot_matches: true, persuaded: false, rejecting: false });
    const dk = rows.find((r) => r.blind_dont_know === true)!;
    expect(dk).toMatchObject({ blind_matches_model: null, outside_expertise: true });
    expect(String(r1.reviewer_code)).toMatch(/^R-[0-9A-F]{8}$/);
  });

  it('qc_case_results joins the responses on case_id and labels too-few cases', () => {
    const res = caseResultRows(SUBS, CASES);
    expect(res.map((r) => r.case_id)).toEqual(['CASE_001', 'CASE_002', 'CASE_003']);
    for (const r of res) expect(Object.keys(r)).toEqual([...CASE_RESULT_ALL_COLUMNS]);
    expect(CASE_RESULT_ALL_COLUMNS.slice(0, CASE_RESULT_COLUMNS.length)).toEqual([...CASE_RESULT_COLUMNS]);
    const responses = reviewResponseRows(SUBS, CASES);
    for (const r of res) expect(r.n_reviews).toBe(new Set(responses.filter((x) => x.case_id === r.case_id).map((x) => x.reviewer_code)).size);
    const c1 = res.find((r) => r.case_id === 'CASE_001')!;
    expect(c1).toMatchObject({ too_few_reviews: false, n_interpretable: 4, share_matching_model: 0.5 });
    const c2 = res.find((r) => r.case_id === 'CASE_002')!;
    expect(c2).toMatchObject({ too_few_reviews: true, summary_label: 'Too few reviews to summarise', share_matching_model: null });
  });

  it('qc_data_issues carries the page context, adjudication status and the priority group joined on submission_id', () => {
    const rows = dataIssueRows(SUBS, CASES);
    expect(rows.length).toBe(SUBS.filter((s) => s.kind === 'data_issue' || s.kind === 'break_model').length);
    for (const r of rows) expect(Object.keys(r)).toEqual([...DATA_ISSUE_COLUMNS]);
    const crit = rows.filter((r) => r.severity === 'critical');
    expect(crit).toHaveLength(2);
    expect(new Set(crit.map((r) => r.priority_group)).size).toBe(1);
    expect(crit[0]).toMatchObject({ page_route: '/countries/:iso3', page_country: 'KOR', page_node: 'Batteries|Components', page_tech: 'batteries', page_stage: 'Components', adjudication_status: 'open' });
    const files = csvFiles(SUBS, CASES, [], { cards: CARDS });
    const pri = files.find((f) => f.name === 'qc_priority_fixes.csv')!.text.split('\r\n');
    expect(pri[1]).toContain(String(crit[0].priority_group));
    expect(pri[1].startsWith('1,')).toBe(true);
  });

  it('qc_framework_answers recomputes correctness and marks mismatches', () => {
    const rows = frameworkAnswerRows(SUBS, { scenarios: SCENARIOS });
    expect(rows.map((r) => r.mismatch)).toEqual([false, false, true, false]);
    expect(rows.map((r) => r.correct)).toEqual([true, true, false, null]);
  });
});

describe('csvFiles', () => {
  it('writes every file with a header even when empty, in a fixed order', () => {
    const files = csvFiles([], [], []);
    expect(files.map((f) => f.name)).toEqual(['qc_review_responses.csv', 'qc_case_results.csv', 'qc_method_feedback.csv', 'qc_data_issues.csv', 'qc_framework_answers.csv', 'qc_decision_use.csv', 'qc_threat_reviews.csv', 'qc_priority_fixes.csv', 'qc_evidence_gaps.csv']);
    expect(header(files[0].text)).toBe(REVIEW_RESPONSE_ALL_COLUMNS.join(','));
    expect(header(files[8].text)).toBe(EVIDENCE_GAP_COLUMNS.join(','));
    for (const f of files) expect(f.rows).toBe(0);
  });

  it('adds reviewer_name after reviewer_code only with includeNames, and never for an anonymous reviewer', () => {
    const named = SUBS.map((s, i) => ({ ...s, reviewer: { ...s.reviewer, name: i % 2 ? 'Named Person' : 'Anon Person', anonymous: i % 2 === 0 } }));
    const plain = csvFiles(named, CASES, []);
    for (const f of plain) {
      expect(f.text).not.toMatch(/Named Person|Anon Person/);
      expect(header(f.text)).not.toMatch(/reviewer_name/);
    }
    const withNames = csvFiles(named, CASES, [], { includeNames: true });
    const resp = withNames[0].text;
    expect(header(resp)).toMatch(/reviewer_code,reviewer_name,/);
    expect(resp).toMatch(/Named Person/);
    expect(resp).not.toMatch(/Anon Person/);
  });

  it('is deterministic and quotes text with commas', () => {
    const a = csvFiles(SUBS, CASES, [], { cards: CARDS, scenarios: SCENARIOS });
    const b = csvFiles([...SUBS].reverse(), CASES, [], { cards: CARDS, scenarios: SCENARIOS });
    expect(b.map((f) => f.text)).toEqual(a.map((f) => f.text));
    expect(a[1].text).toContain('"CASE_001, a title"');
  });

  it('neutralises spreadsheet formulas in reviewer free text, not numbers or IDs', () => {
    expect(neutraliseFormula('=HYPERLINK("http://x")')).toBe(`'=HYPERLINK("http://x")`);
    expect(neutraliseFormula('+1')).toBe(`'+1`);
    expect(neutraliseFormula('@SUM(A1)')).toBe(`'@SUM(A1)`);
    expect(neutraliseFormula('-cmd')).toBe(`'-cmd`);
    expect(neutraliseFormula('-0.3')).toBe('-0.3');
    expect(neutraliseFormula('CASE_001')).toBe('CASE_001');
    const evil = SUBS.map((s) => (s.kind === 'data_issue' ? { ...s, payload: { ...s.payload, what_is_wrong: '=1+1' } } : s));
    const text = csvFiles(evil, CASES, []).find((f) => f.name === 'qc_data_issues.csv')!.text;
    expect(text).toContain(`'=1+1`);
    expect(text).not.toMatch(/(^|,)=1\+1/m);
  });

  it('uses the same pseudonymous code in every file', () => {
    const code = pseudonymReviewerCode(SUBS[0].reviewer.reviewer_id);
    const files = csvFiles(SUBS, CASES, [], { cards: CARDS, scenarios: SCENARIOS });
    expect(files.filter((f) => f.text.includes(code)).length).toBeGreaterThan(2);
    for (const f of files) expect(f.text).not.toContain(SUBS[0].reviewer.reviewer_id + ',');
  });
});

describe('export-review CLI', () => {
  const tmp = mkdtempSync(join(tmpdir(), 'aib-review-export-'));
  const repo = resolve(__dirname, '../../..');
  const privateOut = join(repo, 'qc', 'private', `vitest-export-${process.pid}`);
  afterAll(() => {
    rmSync(tmp, { recursive: true, force: true });
    rmSync(privateOut, { recursive: true, force: true });
    // leave no empty qc/private behind when this test created it
    const dir = join(repo, 'qc', 'private');
    if (existsSync(dir) && readdirSync(dir).length === 0) rmdirSync(dir);
  });
  const quiet = { log: () => {}, warn: () => {}, error: () => {} };
  const casesFile = join(tmp, 'cases.json');
  writeFileSync(casesFile, JSON.stringify({ schema: 'aib-review-cases', schema_version: 1, campaign_id: 'round1', data_release: 'rel-0123456789ab', built_at: '2027-01-10T00:00:00Z', app_commit: 'abc1234', cases: CASES }));
  const subsDir = join(tmp, 'subs');
  const packet = { schema: 'aib-review-packet', schema_version: 1, reviewer_id: SUBS[0].reviewer.reviewer_id, submissions: SUBS.slice(0, 5) };

  it('writes the CSVs from packets and loose submissions outside the repository, skipping invalid ones', () => {
    rmSync(subsDir, { recursive: true, force: true });
    mkdirSync(subsDir, { recursive: true });
    writeFileSync(join(subsDir, 'packet.json'), JSON.stringify(packet));
    writeFileSync(join(subsDir, 'rest.json'), JSON.stringify(SUBS.slice(5)));
    writeFileSync(join(subsDir, 'bad.json'), JSON.stringify({ schema: 'aib-review-submission', kind: 'case_review' }));
    writeFileSync(join(subsDir, 'notes.txt'), 'ignored');
    const out = join(tmp, 'out');
    const warnings: string[] = [];
    const code = run(['--submissions', subsDir, '--cases', casesFile, '--out', out], { ...quiet, warn: (m: string) => warnings.push(m) });
    expect(code).toBe(0);
    expect(warnings.some((w) => w.startsWith('skipped:'))).toBe(true);
    const resp = readFileSync(join(out, 'qc_review_responses.csv'), 'utf8');
    expect(resp.split('\r\n')[0]).toBe(REVIEW_RESPONSE_ALL_COLUMNS.join(','));
    expect(existsSync(join(out, 'qc_evidence_gaps.csv'))).toBe(true);
    const summary = JSON.parse(readFileSync(join(out, 'export_summary.json'), 'utf8'));
    // every demo submission (v1 and v2, each verdict statement included) validates and is exported
    expect(SUBS.every((x) => validateSubmission(x).ok)).toBe(true);
    expect(summary).toMatchObject({ n_submissions: SUBS.length, n_skipped: 1, include_names: false, cases: { campaign_id: 'round1' } });
    expect(summary.n_case_reviews_v2).toBeGreaterThan(0);
    expect(run(['--submissions', subsDir, '--out', join(tmp, 'strict'), '--strict'], quiet)).toBe(1);
    expect(existsSync(join(tmp, 'strict'))).toBe(false);
  });

  it('refuses a git-tracked output directory and accepts the git-ignored qc/private', () => {
    const tracked = join(repo, 'dashboard', 'src', 'review', `export-should-not-exist-${process.pid}`);
    expect(checkOutputDir(tracked).ok).toBe(false);
    expect(run(['--submissions', subsDir, '--out', tracked], quiet)).toBe(2);
    expect(existsSync(tracked)).toBe(false);
    expect(checkOutputDir(join(repo, 'qc', 'results')).ok).toBe(false);
    expect(checkOutputDir(privateOut)).toMatchObject({ ok: true });
    expect(run(['--submissions', subsDir, '--out', privateOut], quiet)).toBe(0);
    expect(existsSync(join(privateOut, 'qc_case_results.csv'))).toBe(true);
  });

  it('fails on a bad command line or an invalid cases file', () => {
    expect(run(['--out', tmp], quiet)).toBe(1);
    expect(run(['--submissions', subsDir], quiet)).toBe(1);
    expect(run(['--submissions', subsDir, '--out', join(tmp, 'o2'), '--bogus'], quiet)).toBe(1);
    const badCases = join(tmp, 'bad-cases.json');
    writeFileSync(badCases, JSON.stringify({ schema: 'nope', cases: [] }));
    expect(run(['--submissions', subsDir, '--cases', badCases, '--out', join(tmp, 'o3')], quiet)).toBe(1);
  });
});
