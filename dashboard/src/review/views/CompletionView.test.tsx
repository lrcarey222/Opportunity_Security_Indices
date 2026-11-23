// The completion summary (/review/summary, review/views/CompletionView.tsx): the counts come from the reviewer's
// own submissions through aggregate.ts reviewerSummary ("You reviewed N cases. N matched your initial
// assessment. N changed your view after reviewing the evidence. N raised a potential data issue."), another
// reviewer's answers in the same browser are left out, a v1 review still counts, the next steps are optional
// links (send, another case, the methodology, another problematic case), open assigned cases are mentioned and
// not pushed, and with nothing submitted the page says so. One h1, no bare acronyms or banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { reviewerSummary } from '../aggregate.ts';
import { newSubmission } from '../context.ts';
import type { BlindAnswer, CaseReviewPayload, ReviewerProfile, RevealResponse } from '../model.ts';
import type { ReviewStorage } from '../storage.ts';
import { SEND_TEXT, summaryLines } from './CompletionView.tsx';
import { CAMPAIGN_ID, CASES, installDomStubs, koreaBatteriesProfile, RELEASE, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

let restore: () => void = () => undefined;
beforeAll(() => {
  restore = installDomStubs();
});
afterAll(() => restore());
afterEach(() => {
  cleanup();
  resetReviewFixture();
  resetLastLocations();
});

const T = { timeout: 20_000 };
const renderAt = (path: string) => {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
};
const main = () => document.querySelector('main') as HTMLElement;

let n = 0;
function save(storage: ReviewStorage, p: ReviewerProfile, caseId: string, blind: Partial<BlindAnswer>, reveal: Partial<RevealResponse>) {
  const payload: CaseReviewPayload = {
    case_id: caseId,
    case_snapshot_sha256: 'x'.repeat(64),
    assignment_reason: 'assigned',
    expertise_match: 'in',
    blind: { choice: 'watch', confidence: 3, factors: [], answered_at: '2026-11-20T00:00:00Z', ux_version: 2, confidence_level: 'medium', driver: 'domestic_capability', ...blind },
    reveal: { revealed_at: '2026-11-20T00:01:00Z', verdict: 1, ...reveal },
    outside_expertise: false,
    time_spent_s: 300,
  };
  storage.saveSubmission(newSubmission('case_review', payload, { profile: p, campaign_id: CAMPAIGN_ID, release: { data_release: RELEASE, app_commit: 'test' }, now: `2026-11-20T00:0${++n % 10}:00Z`, uuid: () => `sum-${n}` }));
}

describe('/review/summary', () => {
  it('counts matched, changed view and data issues from this reviewer only, then offers optional next steps', async () => {
    const { storage } = useReviewFixture();
    const me = koreaBatteriesProfile();
    const other = koreaBatteriesProfile({ reviewer_id: 'rv-other', name: 'Someone Else' });
    storage.saveProfile(me);
    // CASE_001: the Explorer concluded Preserve; the blind answer was Preserve -> matched
    save(storage, me, 'CASE_001', { choice: 'preserve' }, { verdict_code: 'same_same', verdict: 1, validity: 'supported', persuasion: 'unchanged' });
    // CASE_002: Watch; blind Secure, persuaded -> changed view
    save(storage, me, 'CASE_002', { choice: 'secure' }, { verdict_code: 'persuaded', verdict: 2, validity: 'supported', persuasion: 'changed' });
    // CASE_003: Watch; blind Partner, a fact is wrong -> data issue
    save(storage, me, 'CASE_003', { choice: 'partner' }, { verdict_code: 'wrong_fact', verdict: 4, validity: 'not_supported', persuasion: 'unchanged', main_issue: { text: 'the share is wrong', type: 'data' } });
    // another reviewer in the same browser: not counted
    save(storage, other, 'CASE_004', { choice: 'watch' }, { verdict_code: 'same_same', verdict: 1 });
    renderAt('/review/summary');
    await screen.findByRole('heading', { level: 2, name: 'Your summary' }, T);
    expect(screen.getAllByRole('heading', { level: 1 }).map((h) => h.textContent)).toEqual(['Expert review']);
    expect(screen.getByTestId('summary-lead').textContent).toBe('You reviewed 3 cases.');
    const lines = screen.getByTestId('summary-lines');
    const line = (id: string) => lines.querySelector(`[data-line="${id}"]`)?.textContent ?? '';
    expect(line('matched')).toMatch(/^1 matched your initial assessment\./);
    expect(line('matched')).toContain('Korea: battery component manufacturing');
    expect(line('changed')).toMatch(/^1 changed your view after reviewing the evidence\./);
    expect(line('data')).toMatch(/^1 raised a potential data issue\./);
    expect(line('data')).toContain('KOR: Batteries extraction');
    expect(lines.querySelector('[data-line="insufficient"]')).toBeNull();
    // optional next steps, nothing forced
    expect(screen.getByRole('button', { name: SEND_TEXT.local.button })).toBeTruthy();
    const actions = screen.getByTestId('summary-actions');
    expect(within(actions).getAllByRole('link').map((a) => [a.textContent, a.getAttribute('href')])).toEqual([
      ['Review another case', '/review/cases'],
      ['Challenge the methodology', '/review/methods'],
      ['Report another problematic case', '/review/break'],
    ]);
    expect(main().textContent).toContain('Nothing more is required');
    expect(screen.queryByTestId('summary-open')).toBeNull();
    expect(main().textContent).not.toContain('Someone Else');
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('mentions assigned cases still open without pushing them; a v1 review and an insufficient-evidence answer count', async () => {
    const { storage } = useReviewFixture();
    const me = koreaBatteriesProfile();
    storage.saveProfile(me);
    // a v1 review: verdict 2 (persuaded)
    save(storage, me, 'CASE_002', { choice: 'secure', ux_version: undefined, confidence_level: undefined, driver: undefined, confidence: 4, factors: ['home_share'] }, { verdict: 2 });
    save(storage, me, 'CASE_003', { choice: 'insufficient_evidence', missing_evidence: ['production_capacity'] }, { verdict_code: 'plausible_insufficient', verdict: 3, validity: 'unsure', persuasion: 'unchanged' });
    renderAt('/review/summary');
    await screen.findByRole('heading', { level: 2, name: 'Your summary' }, T);
    expect(screen.getByTestId('summary-lead').textContent).toBe('You reviewed 2 cases.');
    expect(screen.getByTestId('summary-lines').querySelector('[data-line="changed"]')?.textContent).toMatch(/^1 changed your view/);
    expect(screen.getByTestId('summary-lines').querySelector('[data-line="insufficient"]')?.textContent).toMatch(/^1 had too little evidence/);
    expect(screen.getByTestId('summary-open').textContent).toBe('1 assigned case is still open: Korea: battery component manufacturing. Come back to it whenever you like.');
  });

  it('with nothing submitted, says so', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review/summary');
    await screen.findByRole('heading', { level: 2, name: 'Your summary' }, T);
    expect(main().textContent).toContain('You have not submitted a case review in this browser yet.');
    expect(screen.getByRole('link', { name: 'See your cases' }).getAttribute('href')).toBe('/review/cases');
  });

  it('summaryLines words the counts (singular and plural)', () => {
    const s = reviewerSummary([], CASES);
    expect(summaryLines(s).lead).toBe('You reviewed 0 cases.');
    expect(summaryLines({ ...s, n_cases_reviewed: 1 }).lead).toBe('You reviewed 1 case.');
    expect(summaryLines({ ...s, n_outside_expertise: 2 }).lines.map((l) => l.id)).toEqual(['matched', 'changed', 'data', 'outside']);
  });
});
