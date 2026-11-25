// The internal QC dashboard (review/views/AdminDashboardView.tsx): it says it runs in this browser only; it loads
// the synthetic demo fixtures (labelled synthetic) and submission files; Agreement summarises an anchor case
// with three reviews and labels a case with fewer "too few reviews to summarise", with the elicitation caveat;
// Priority fixes put a confirmed critical data issue first and show its three factors; names stay hidden until
// "show names" is ticked, and an anonymous reviewer's name never shows; the Data QC and Robustness tabs read
// qc_summary.json and robustness.json; the pure helpers parse packets and count robustness by tech x stage.
// UX v2: with mixed v1 and v2 demo data (review/demo.ts), the Verdicts tab shows the ten statements, the Prior,
// validity and persuasion tab highlights "expert prior differed, evidence persuaded", Evidence gaps counts by
// category, case and product and flags repeated inability, Product questions tallies the tailored questions, and
// the existing tabs and the privacy default keep working.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, type KeyValueBackend, type ReviewStorage } from '../storage.ts';
import { memoryReviewSource, ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import { newSubmission } from '../context.ts';
import { buildDashboard, TOO_FEW_LABEL } from '../aggregate.ts';
import { makeDemoSubmissions } from '../demo.ts';
import { REVIEW_VERDICT_CODES, REVIEW_VERDICT_LABEL } from '../model.ts';
import { pseudonymReviewerCode } from '../privacy.ts';
import { answerOptions } from '../qc/answerOptions.ts';
import type { CaseReviewPayload, DataIssuePayload, PageContext, ReviewCase, ReviewerProfile, Submission } from '../model.ts';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import AdminDashboardView, { robustnessGrid, submissionsFromJson } from './AdminDashboardView.tsx';

function memoryBackend(): KeyValueBackend {
  const m = new Map<string, string>();
  return {
    get length() {
      return m.size;
    },
    key: (i) => [...m.keys()][i] ?? null,
    getItem: (k) => m.get(k) ?? null,
    setItem: (k, v) => void m.set(k, v),
    removeItem: (k) => void m.delete(k),
  };
}

const NodeRequest = globalThis.Request;
beforeAll(() => {
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
});
afterAll(() => {
  globalThis.Request = NodeRequest;
});

const REL = 'rel-000000000001';
const MANIFEST = {
  schema: 'aib-review-manifest',
  schema_version: 1,
  built_at: '2027-01-01T00:00:00Z',
  app_commit: 'abc1234',
  data_release: { id: REL, stamps: {}, complete: true, missing: [] },
  campaigns: [
    { campaign_id: 'c1', name: 'Round 1', status: 'active', opens: null, closes: null, review_types: ['case', 'break_model'], n_cases: 2, anchor_cases: ['CASE_001'], cases_file: 'cases.c1.json', method_card_ids: [], framework_scenario_ids: [] },
  ],
  files: [],
};

const mkCase = (id: string, anchor: boolean, iso3: string, choice: string): ReviewCase => ({
  case_id: id,
  kind: 'posture_cell',
  key: { iso3, tech: 'Batteries', stage: 'Refining' },
  title: `${iso3} Batteries Refining`,
  question: 'What should the strategic choice be?',
  sector_groups: ['batteries'],
  country_groups: [iso3],
  stage: 'Refining',
  assignment_group: 'batteries',
  anchor,
  why_selected: 'test',
  explorer_path: `/countries/${iso3}?node=Batteries|Refining`,
  context_fields: ['need'],
  reveal_fields: ['choice'],
  snapshot: { conclusion: { value: choice, label: choice === 'secure' ? 'Secure' : 'Partner' }, facts: {} },
  snapshot_sha256: `sha-${id}`,
  bundle: [],
  data_release: REL,
  app_commit: 'abc1234',
  built_at: '2027-01-01T00:00:00Z',
  answer_options: answerOptions('posture_cell'),
});
const CASES = { schema: 'aib-review-cases', schema_version: 1, campaign_id: 'c1', data_release: REL, built_at: '2027-01-01T00:00:00Z', app_commit: 'abc1234', cases: [mkCase('CASE_001', true, 'JPN', 'secure'), mkCase('CASE_002', false, 'KOR', 'partner')] };

const profile = (id: string, name: string | null, anonymous: boolean): ReviewerProfile => ({
  reviewer_id: id, name, anonymous, countries: ['KOR', 'JPN'], sectors: ['batteries'], stages: ['Refining'], methods: [],
  confidence: { 'sector:batteries': 4, 'country:KOR': 4, 'country:JPN': 4, 'stage:Refining': 4 }, familiarity_neis: 1, familiarity_explorer: 1, advised_on: [], created_at: '2027-01-01T00:00:00Z',
});
const A = profile('rev-a', 'Hidden Anonymous', true);
const B = profile('rev-b', null, true);
const C = profile('rev-c', null, true);
const NAMED = profile('rev-named', 'Grace Named', false);

let n = 0;
const ctx = (path: string, country: string, stage: string): PageContext => ({
  url: path, route: '/countries/:iso3', view: 'country', country, tech: 'batteries', stage, node: null, layer: null, pair: null, sid: null, scenario: null,
  displayed_result: null, metric_ids: [], data_release: REL, bundle_stamps: {}, app_commit: 'abc1234', captured_at: '2027-01-02T00:00:00Z',
});
function review(p: ReviewerProfile, caseId: string, choice: string, verdict: 1 | 2 | 5): Submission {
  const payload: CaseReviewPayload = {
    case_id: caseId, case_snapshot_sha256: `sha-${caseId}`, assignment_reason: 'assigned', expertise_match: 'in',
    blind: { choice, confidence: 4, factors: ['import dependence'], answered_at: '2027-01-02T00:00:00Z' },
    reveal: { revealed_at: '2027-01-02T00:01:00Z', verdict, ...(verdict === 5 ? { main_issue: { text: 'The threshold is too strict.', type: 'method' as const } } : {}) },
    outside_expertise: false, time_spent_s: 300,
  };
  return newSubmission('case_review', payload, { profile: p, campaign_id: 'c1', release: { data_release: REL, app_commit: 'abc1234' }, now: '2027-01-02T00:02:00Z', uuid: () => `r${++n}` });
}
function issue(p: ReviewerProfile, metric: string, iso3: string, severity: DataIssuePayload['severity'], what: string, id: string): Submission {
  const payload: DataIssuePayload = { metric: { id: metric, label: metric, value: 0.5, display: '50%', country: iso3, tech: 'batteries', stage: 'Refining' }, what_is_wrong: what, severity };
  return newSubmission('data_issue', payload, { profile: p, campaign_id: 'c1', page_context: ctx(`/countries/${iso3}`, iso3, 'Refining'), release: { data_release: REL, app_commit: 'abc1234' }, now: '2027-01-03T00:00:00Z', uuid: () => id });
}

const CRITICAL_ID = 'crit';
const DEMO_SUBS: Submission[] = [
  review(A, 'CASE_001', 'secure', 1),
  review(B, 'CASE_001', 'secure', 1),
  review(C, 'CASE_001', 'partner', 5),
  review(A, 'CASE_002', 'partner', 1),
  issue(A, 'prize', 'JPN', 'cosmetic', 'The label wraps badly.', 'cos1'),
  issue(B, 'prize', 'JPN', 'cosmetic', 'The label wraps badly here too.', 'cos2'),
  issue(C, 'prize', 'JPN', 'cosmetic', 'Typo in the prize label.', 'cos3'),
  issue(NAMED, 'need', 'KOR', 'critical', 'Imports double-count re-exports: need is overstated and the choice reverses.', CRITICAL_ID),
];
const DEMO = { schema: 'aib-review-demo-submissions', synthetic: true, marker: '[synthetic]', submissions: DEMO_SUBS };

const QC = {
  schema: 'aib-review-qc-summary', schema_version: 1, data_release: REL, built_at: '2027-01-01T00:00:00Z', layer: 'bundle',
  status: { pass: 2, warn: 1, fail: 1, info: 0 },
  checks: [
    { check_id: 'BQ-T-01', layer: 'bundle', area: 'posture.numeric', status: 'fail', n: 3, detail: 'three shares above one', examples: ['KOR|Batteries|Refining'] },
    { check_id: 'BQ-PROV-AIB-01', layer: 'bundle', area: 'provenance', status: 'warn', n: 4, detail: 'AIB files carry no git commit', examples: [] },
    { check_id: 'BQ-T-02', layer: 'bundle', area: 'schema', status: 'pass', n: 0, detail: 'ok', examples: [] },
  ],
  reproduction: { cells: 100, reproduced: 99, rate: 0.99, exceptions_total: 1 },
  robustness: { posture: { by_class: { stable: 1 }, by_driver: { large_yard_min_signals: 7 } }, aib: null },
  golden: { summary: { pass: 35, fail: 1, missing: 0, not_applicable: 0 }, results: [{ id: 'GC_010', status: 'fail', failures: ['choice partner, expected secure'] }] },
};
const ROBUSTNESS = {
  schema: 'aib-review-robustness', schema_version: 1, data_release: REL, layer: 'bundle',
  codes: { S: 'stable', N: 'sensitive', F: 'fragile', X: 'not_testable' },
  posture: { techs: ['Batteries', 'Solar'], stages: ['Extraction', 'Refining'], cells: { AAA: 'SFNX', BBB: 'FFSS' }, fence_change: [], summary: { by_class: { stable: 3, sensitive: 1, fragile: 3, not_testable: 1 }, fence_change: 2 } },
  aib: null,
  not_runnable_on_bundle: ['need floor 0.05'],
};

let store: ReviewStorage;
beforeEach(() => {
  n = 0;
  store = createReviewStorage(memoryBackend());
  setReviewStoreForTests(
    new ReviewStore(
      memoryReviewSource({ 'manifest.json': MANIFEST, 'cases.c1.json': CASES, 'method_cards.json': { cards: [] }, 'framework_scenarios.json': { scenarios: [] }, 'qc_summary.json': QC, 'robustness.json': ROBUSTNESS, 'fixtures/demo_submissions.json': DEMO }),
    ),
  );
});
afterEach(() => {
  cleanup();
  setReviewStoreForTests(null);
});

function show() {
  const router = createMemoryRouter([{ path: '/review/admin', element: <AdminDashboardView storage={store} /> }], { initialEntries: ['/review/admin'] });
  return render(<RouterProvider router={router} />);
}

async function loadDemo() {
  const v = show();
  await screen.findByRole('tablist');
  fireEvent.click(screen.getByRole('button', { name: 'Load the demo data (synthetic)' }));
  await screen.findByTestId('admin-synthetic');
  return v;
}

describe('AdminDashboardView', () => {
  it('says it runs in this browser only and labels the demo data synthetic', async () => {
    const { container } = await loadDemo();
    expect(screen.getByText('Runs in this browser only: files you load are not uploaded.')).toBeTruthy();
    expect(screen.getByTestId('admin-sets').textContent).toContain('Demo fixtures (synthetic) (8)');
    expect(screen.getByTestId('admin-overview').textContent).toContain('2 of 2');
    expect(bareAcronyms(container)).toEqual([]);
    expect(AVOID_RE.test(container.textContent ?? '')).toBe(false);
  });

  it('agreement: an anchor with three reviews is summarised, a case with one is "too few"; the elicitation caveat', async () => {
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Agreement' }));
    expect(screen.getByTestId('elicitation-note').textContent).toMatch(/structured expert elicitation, not representative polling/);
    const cards = screen.getAllByTestId('agreement-case');
    expect(cards[0].getAttribute('data-case')).toBe('CASE_001');
    expect(cards[0].querySelector('[data-testid="share-matching"]')!.textContent).toBe('67%');
    expect(cards[0].textContent).toContain('Secure (Explorer)');
    const few = cards.find((c) => c.getAttribute('data-case') === 'CASE_002')!;
    expect(few.querySelector('[data-testid="too-few"]')!.textContent).toContain(TOO_FEW_LABEL);
    expect(few.querySelector('[data-testid="share-matching"]')).toBeNull();
  });

  it('priority fixes: a confirmed critical issue first, with its three factors; the adjudication editor saves', async () => {
    store.saveAdjudication({ adjudication_id: 'ADJ-1', submission_id: `SUB-${CRITICAL_ID}`, status: 'confirmed', issue_type: 'data', confidence_real: 1, affected_outputs: 3, note: null, decided_by: 'NEIS', decided_at: '2027-01-04T00:00:00Z' });
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Priority fixes' }));
    const items = screen.getAllByTestId('priority-item');
    expect(items[0].getAttribute('data-severity')).toBe('critical');
    expect(items[0].getAttribute('data-status')).toBe('confirmed');
    expect(items[0].querySelector('[data-testid="priority-factors"]')!.textContent).toBe('1.00 × 1.00 × 3.00');
    expect(items.at(-1)!.getAttribute('data-severity')).not.toBe('critical');
    // adjudicate the cosmetic group as rejected
    const cosmetic = items.find((i) => i.getAttribute('data-severity') === 'cosmetic')!;
    fireEvent.click(cosmetic.querySelector('button')!);
    fireEvent.change(screen.getByLabelText('Status'), { target: { value: 'rejected' } });
    fireEvent.change(screen.getByLabelText('Confidence it is real (0 to 1)'), { target: { value: '0' } });
    fireEvent.click(screen.getByRole('button', { name: 'Save the adjudication' }));
    await waitFor(() => expect(store.listAdjudications()).toHaveLength(2));
    expect(store.listAdjudications().find((a) => a.adjudication_id !== 'ADJ-1')).toMatchObject({ status: 'rejected', confidence_real: 0 });
  });

  it('privacy: no names by default; "show names" shows a named reviewer but never an anonymous one', async () => {
    const { container } = await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Priority fixes' }));
    expect(container.textContent).not.toContain('Grace Named');
    expect(container.textContent).toContain(pseudonymReviewerCode('rev-named'));
    fireEvent.click(screen.getByLabelText('Show names (never for anonymous reviewers)'));
    expect(container.textContent).toContain('Grace Named');
    expect(container.textContent).not.toContain('Hidden Anonymous');
  });

  it('data QC and robustness read the built summaries', async () => {
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Data QC' }));
    const qc = screen.getByTestId('admin-dataqc');
    expect(qc.querySelector('[data-check="BQ-T-01"]')!.getAttribute('data-status')).toBe('fail');
    expect(qc.querySelector('[data-check="BQ-PROV-AIB-01"]')).toBeTruthy();
    expect(qc.textContent).toContain('99%');
    expect(qc.textContent).toContain('GC_010');
    expect(qc.textContent).toContain('Imports double-count re-exports');
    fireEvent.click(screen.getByRole('tab', { name: 'Robustness' }));
    const grid = screen.getByTestId('robustness-grid');
    // Batteries/Extraction: AAA S, BBB F -> 1 fragile, 0 sensitive; Batteries/Refining: AAA F, BBB F -> 2 / 0
    const rows = grid.querySelectorAll('tbody tr');
    expect([...rows[0].querySelectorAll('td')].map((td) => td.textContent)).toEqual(['Batteries', '1 / 0', '2 / 0']);
    expect(screen.getByTestId('admin-robustness').textContent).toContain('large yard min signals');
  });
});

describe('pure helpers', () => {
  it('submissionsFromJson reads packets, lists, single submissions and demo documents; invalid ones are reported', () => {
    const one = DEMO_SUBS[0];
    expect(submissionsFromJson({ schema: 'aib-review-packet', schema_version: 1, reviewer_id: 'rev-a', submissions: [one] }).submissions).toHaveLength(1);
    expect(submissionsFromJson([one, one]).submissions).toHaveLength(2);
    expect(submissionsFromJson(one).submissions).toHaveLength(1);
    expect(submissionsFromJson(DEMO).synthetic).toBe(true);
    const bad = submissionsFromJson([{ ...one, schema: 'nope' }], 'x.json');
    expect(bad.submissions).toEqual([]);
    expect(bad.errors[0]).toMatch(/^x\.json #1:/);
    expect(submissionsFromJson(42).errors).toHaveLength(1);
  });

  it('robustnessGrid counts classes by technology x stage (technology-major codes)', () => {
    const g = robustnessGrid(ROBUSTNESS as never)!;
    expect(g.counts.Batteries.Extraction).toEqual({ stable: 1, sensitive: 0, fragile: 1, not_testable: 0 });
    expect(g.counts.Solar.Refining).toEqual({ stable: 1, sensitive: 0, fragile: 0, not_testable: 1 });
  });
});

// ---- UX v2: the new tabs on mixed v1 + v2 demo data --------------------------------------------------------

const v2Case = (id: string, kind: ReviewCase['kind'], value: string, over: Partial<ReviewCase> = {}): ReviewCase => ({
  ...mkCase(id, false, 'KOR', 'partner'),
  kind,
  title: `${kind} case ${id}`,
  snapshot: { conclusion: { value, label: value }, facts: {} },
  answer_options: answerOptions(kind),
  ...over,
});
const V2_CASES = [
  mkCase('CASE_001', true, 'JPN', 'secure'),
  mkCase('CASE_002', false, 'KOR', 'partner'),
  v2Case('CASE_003', 'posture_cell', 'watch'),
  v2Case('CASE_004', 'partnership', 'partner'),
  v2Case('CASE_005', 'compact', 'sectoral', { key: { sid: 'SC-SOLAR', scenario: 'decoupling' }, answer_options: answerOptions('compact', { sid: 'SC-SOLAR' }) }),
];
const V2_SUBS = makeDemoSubmissions(V2_CASES, [], [], 'admin-v2-test', 'c1');
const V2_DEMO = { schema: 'aib-review-demo-submissions', synthetic: true, marker: '[synthetic]', submissions: V2_SUBS };

describe('AdminDashboardView: UX v2 tabs', () => {
  beforeEach(() => {
    setReviewStoreForTests(
      new ReviewStore(
        memoryReviewSource({
          'manifest.json': { ...MANIFEST, campaigns: [{ ...MANIFEST.campaigns[0], n_cases: V2_CASES.length }] },
          'cases.c1.json': { ...CASES, cases: V2_CASES },
          'method_cards.json': { cards: [] },
          'framework_scenarios.json': { scenarios: [] },
          'qc_summary.json': QC,
          'robustness.json': ROBUSTNESS,
          'fixtures/demo_submissions.json': V2_DEMO,
        }),
      ),
    );
  });

  const expected = () => buildDashboard({ submissions: V2_SUBS, cases: V2_CASES, campaign: null });

  it('the demo data mix v1 and v2 reviews; the tab list keeps every earlier tab', async () => {
    const d = expected();
    expect(d.overview.n_v2_reviews).toBeGreaterThan(0);
    expect(d.overview.n_case_reviews).toBeGreaterThan(d.overview.n_v2_reviews);
    const { container } = await loadDemo();
    expect(screen.getAllByRole('tab').map((t) => t.textContent)).toEqual([
      'Overview',
      'Agreement',
      'Verdicts',
      'Prior, validity and persuasion',
      'Evidence gaps',
      'Product questions',
      'Disagreement taxonomy',
      'Data QC',
      'Robustness',
      'Priority fixes',
    ]);
    expect(screen.getByTestId('admin-overview').textContent).toMatch(/with the new questions/);
    for (const name of ['Verdicts', 'Prior, validity and persuasion', 'Evidence gaps', 'Product questions', 'Agreement']) {
      fireEvent.click(screen.getByRole('tab', { name }));
      expect(bareAcronyms(container)).toEqual([]);
      expect(AVOID_RE.test(container.textContent ?? '')).toBe(false);
    }
  });

  it('Verdicts: the ten statements with their counts, the answers kept out of agreement, the drivers', async () => {
    const d = expected().v2;
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Verdicts' }));
    const table = screen.getByTestId('admin-verdict-codes');
    const rows = [...table.querySelectorAll('tbody tr')];
    expect(rows.map((r) => r.getAttribute('data-code'))).toEqual([...REVIEW_VERDICT_CODES]);
    for (const r of rows) {
      const code = r.getAttribute('data-code') as (typeof REVIEW_VERDICT_CODES)[number];
      expect(r.querySelector('td')!.textContent).toContain(REVIEW_VERDICT_LABEL[code]);
      expect(r.querySelectorAll('td')[1].textContent).toBe(String(d.verdict_codes.overall[code]));
    }
    expect(rows.find((r) => r.getAttribute('data-code') === 'wrong_rule')!.getAttribute('data-disagreeing')).toBe('true');
    expect(screen.getByTestId('admin-verdicts').textContent).toContain(`${d.verdict_codes.n_uncoded} earlier reviews used the 1 to 7 scale`);
    expect(screen.getByTestId('admin-reviewer-states').textContent).toContain(`Insufficient evidence to judge: ${d.reviewer_states.overall.insufficient_evidence}`);
    expect(screen.getByTestId('admin-drivers').querySelectorAll('tbody tr').length).toBeGreaterThan(0);
  });

  it('Prior, validity and persuasion: highlights expert prior differed, evidence persuaded', async () => {
    const d = expected().v2.prior_persuasion;
    expect(d.persuaded_group.length).toBeGreaterThan(0);
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Prior, validity and persuasion' }));
    const box = screen.getByTestId('admin-persuaded');
    expect(box.textContent).toContain('Expert prior differed from the Explorer, and the evidence persuaded them');
    expect(screen.getByTestId('persuaded-count').textContent).toBe(String(d.persuaded_group.length));
    expect(box.querySelectorAll('li')).toHaveLength(d.persuaded_group.length);
    const table = screen.getByTestId('admin-prior-table');
    expect(table.querySelectorAll('tbody tr')).toHaveLength(d.table.length);
    expect(table.querySelector('[data-highlight]')!.textContent).toContain('Different from the Explorer');
  });

  it('Evidence gaps: by category, case and product, with the insufficient-evidence rate and repeated inability flagged', async () => {
    const g = expected().v2.evidence_gaps;
    expect(g.by_case.some((r) => r.repeated_inability)).toBe(true);
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Evidence gaps' }));
    const cat = screen.getByTestId('admin-gaps-by-category');
    expect(cat.querySelectorAll('tbody tr')).toHaveLength(g.by_category.length);
    expect(cat.querySelector('[data-category="production_capacity"]')!.textContent).toContain('Actual production capacity');
    const byCase = screen.getByTestId('admin-gaps-by-case');
    const flagged = byCase.querySelector('[data-repeated]')!;
    expect(flagged.textContent).toContain('repeatedly could not judge');
    const row = g.by_case.find((r) => r.repeated_inability)!;
    expect(flagged.getAttribute('data-case')).toBe(row.case_id);
    expect(flagged.querySelectorAll('td')[4].textContent).toBe(row.share_insufficient === null ? 'too few reviews' : `${Math.round(row.share_insufficient * 100)}%`);
    expect(screen.getByTestId('admin-gaps-by-product').querySelectorAll('tbody tr')).toHaveLength(g.by_product.length);
  });

  it('Product questions: the tailored questions tallied, and the notes', async () => {
    const t = expected().v2.product_questions.filter((x) => x.n > 0);
    expect(t.length).toBeGreaterThan(0);
    await loadDemo();
    fireEvent.click(screen.getByRole('tab', { name: 'Product questions' }));
    const panel = screen.getByTestId('admin-product');
    expect(panel.querySelectorAll('[data-question]')).toHaveLength(t.length);
    expect(panel.textContent).toContain('Does this compact materially ameliorate the stated threat?');
    expect(panel.textContent).toContain('Does this relationship represent genuine industrial complementarity or primarily diplomatic compatibility?');
    expect(screen.getByTestId('admin-notes').textContent).toMatch(/\d+ notes from \d+ reviews/);
  });

  it('privacy: reviewer codes, never names, on the new tabs', async () => {
    const { container } = await loadDemo();
    for (const name of ['Verdicts', 'Prior, validity and persuasion', 'Evidence gaps', 'Product questions']) {
      fireEvent.click(screen.getByRole('tab', { name }));
      expect(container.textContent).not.toMatch(/DEMO-\d\d/);
    }
  });
});
