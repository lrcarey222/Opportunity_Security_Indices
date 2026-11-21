// The review start page and layout (/review, review/views/ReviewHome.tsx, review/ReviewLayout.tsx): the page head
// "Expert review" with the reviewer's own links first and the optional deeper work demoted (no admin link), the
// journey indicator (profile, calibration, cases), "Help us break the AIB Explorer" with its three paragraphs, the
// time of the reviewer's assignment, ONE primary action whose target follows the journey (profile -> calibration
// -> the next open case, an unfinished one first -> the summary once every assigned case is in), "Want to go
// deeper?" only after the assigned cases, the storage mode said plainly, the screens for a build without review
// data and for a failed load, no top-level tab for /review, the document title, one h1, and no bare acronyms or
// banned words. Plus the layout's pure journey helpers.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { placeOf, tabOf } from '../../shell/modules.ts';
import { pageTitle } from '../../shell/title.ts';
import { newSubmission } from '../context.ts';
import { ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import type { CaseReviewPayload, ReviewerProfile } from '../model.ts';
import { draftStatus, estimateMinutes, primaryAction, REVIEW_DEEPER_LINKS, REVIEW_LINKS, safeReturnPath, withQuery } from '../ReviewLayout.tsx';
import type { ReviewStorage } from '../storage.ts';
import { HOME_LEAD, SINK_MODE_TEXT } from './ReviewHome.tsx';
import { CAMPAIGN_ID, CASES, installDomStubs, koreaBatteriesProfile, manifest, RELEASE, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

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
const primary = () => screen.getByTestId('home-primary');
const lead = () => screen.findByRole('heading', { level: 2, name: HOME_LEAD.title }, T);

let n = 0;
/** A v2 case review of `caseId` by `p`, stored in this browser. */
function submitReview(storage: ReviewStorage, p: ReviewerProfile, caseId: string, choice = 'watch') {
  const payload: CaseReviewPayload = {
    case_id: caseId,
    case_snapshot_sha256: 'x'.repeat(64),
    assignment_reason: 'assigned',
    expertise_match: 'in',
    blind: { choice, confidence: 3, factors: ['domestic_capability'], answered_at: '2026-11-20T00:00:00Z', ux_version: 2, confidence_level: 'medium', driver: 'domestic_capability' },
    reveal: { revealed_at: '2026-11-20T00:01:00Z', verdict: 1, verdict_code: 'same_same', validity: 'supported', persuasion: 'unchanged' },
    outside_expertise: false,
    time_spent_s: 300,
  };
  storage.saveSubmission(newSubmission('case_review', payload, { profile: p, campaign_id: CAMPAIGN_ID, release: { data_release: RELEASE, app_commit: 'test' }, now: '2026-11-20T00:02:00Z', uuid: () => `home-${++n}` }));
}

describe('/review', () => {
  it('without a profile: leads with "Help us break the AIB Explorer", one primary action to the profile, no deeper work yet', async () => {
    useReviewFixture();
    renderAt('/review');
    await lead();
    expect(screen.getAllByRole('heading', { level: 1 }).map((h) => h.textContent)).toEqual(['Expert review']);
    const paragraphs = screen.getByTestId('home-lead').textContent ?? '';
    for (const t of [HOME_LEAD.testing, HOME_LEAD.wantToKnow, HOME_LEAD.toSelect]) expect(paragraphs).toContain(t);
    // the reviewer's own pages lead; the deeper work is a quieter second row; no admin link
    const nav = screen.getByRole('navigation', { name: 'Expert review pages' });
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual(REVIEW_LINKS.map((l) => l.label));
    const optional = screen.getByRole('navigation', { name: 'Optional review pages' });
    expect(within(optional).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(['/review/methods', '/review/break', '/review/framework']);
    expect(screen.queryByRole('link', { name: /admin/i })).toBeNull();
    // one primary action
    expect(primary().textContent).toBe('Review your assigned cases');
    expect(primary().getAttribute('href')).toBe('/review/profile');
    expect(primary().getAttribute('data-state')).toBe('no_profile');
    expect(main().querySelectorAll('.btn-primary')).toHaveLength(1);
    expect(screen.getByTestId('home-estimate').textContent).toMatch(/^~2 minutes for your profile, then about 6 to 8 minutes per case$/);
    expect(screen.queryByRole('heading', { name: 'Want to go deeper?' })).toBeNull();
    // lower down: the storage mode, the campaign, the release
    expect(screen.getByText(SINK_MODE_TEXT.local.title)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Test campaign' })).toBeTruthy();
    expect(main().textContent).toContain('Nothing you write is published');
    expect(main().textContent).toContain('draft: answers given now are for testing');
    expect(main().textContent).toContain(RELEASE);
    expect(main().textContent).toMatch(/does not carry the campaign.s assignment and target reviews per anchor/);
    await waitFor(() => expect(document.title).toBe(pageTitle('Overview', 'review')));
    expect(document.title.startsWith('Overview | Expert review |')).toBe(true);
    // a place, no top-level tab
    expect(placeOf('/review/case/CASE_001')).toBe('review');
    expect(tabOf('/review')).toBeNull();
    expect(screen.getByRole('navigation', { name: 'Modules' }).querySelector('[aria-current]')).toBeNull();
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('with a profile, before the calibration: the action goes through the calibration to the first case; the time comes from the assignment', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review');
    await lead();
    expect(screen.getByTestId('home-lead').textContent).toContain(HOME_LEAD.selected);
    expect(primary().getAttribute('data-state')).toBe('calibration');
    expect(primary().getAttribute('href')).toBe(`/review/start?next=${encodeURIComponent('/review/case/CASE_001')}`);
    // CASE_001 has the decision-use questions (8 minutes), CASE_002 and CASE_003 6 each: 20
    expect(screen.getByTestId('home-estimate').textContent).toBe('~20 minutes · 3 cases');
    expect(screen.getByTestId('review-progress').textContent).toContain('2. Before you start: one minute');
    expect(screen.queryByRole('heading', { name: 'Want to go deeper?' })).toBeNull();
  });

  it('in progress: resumes the unfinished case (a v2 draft), says so, and a v1 draft still counts as started', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    storage.markCalibrationSeen('2026-11-20T00:00:00Z', CAMPAIGN_ID);
    storage.saveCaseDraftV2(CAMPAIGN_ID, 'CASE_002', { version: 2, case_id: 'CASE_002', kind: 'posture_cell', step: 'blind', reached: 'blind', blind: { choice: 'partner', choices: [] }, notes: [] });
    storage.saveDraft(CAMPAIGN_ID, 'case', 'CASE_003', { version: 1, case_id: 'CASE_003', step: 'blind', blind: { choice: 'secure', factors: ['home_share'] }, reveal: {} });
    renderAt('/review');
    await lead();
    expect(primary().getAttribute('data-state')).toBe('resume');
    expect(primary().getAttribute('href')).toBe('/review/case/CASE_002');
    expect(screen.getByTestId('home-resume').textContent).toContain('You have an unfinished case: KOR: Batteries refining');
    expect(screen.getByTestId('review-progress').textContent).toContain('0 of 3 submitted');
    expect(screen.getByTestId('review-progress').textContent).toContain('2. Before you start: read');
    expect((screen.getByRole('button', { name: /Download your answers \(0\)/ }) as HTMLButtonElement).disabled).toBe(true);
    // the cases page shows both drafts as started
    cleanup();
    renderAt('/review/cases');
    await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    for (const id of ['CASE_002', 'CASE_003']) expect(within(screen.getByTestId(`assignment-${id}`)).getByTestId('case-status').textContent).toBe('In progress');
    // the head offers to resume off the home page
    expect(screen.getByRole('link', { name: /resume KOR: Batteries refining/ }).getAttribute('href')).toBe('/review/case/CASE_002');
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('some submitted: the next open case, and the time left', async () => {
    const { storage } = useReviewFixture();
    const p = koreaBatteriesProfile();
    storage.saveProfile(p);
    storage.markCalibrationSeen('2026-11-20T00:00:00Z', CAMPAIGN_ID);
    submitReview(storage, p, 'CASE_001');
    renderAt('/review');
    await lead();
    expect(primary().getAttribute('data-state')).toBe('next');
    expect(primary().getAttribute('href')).toBe('/review/case/CASE_002');
    expect(screen.getByTestId('home-estimate').textContent).toBe('~10 minutes left · 1 of 3 cases submitted');
    expect(screen.getByRole('link', { name: 'Your summary so far' }).getAttribute('href')).toBe('/review/summary');
  });

  it('all assigned cases in: the action is the summary and only now "Want to go deeper?" appears', async () => {
    const { storage } = useReviewFixture();
    const p = koreaBatteriesProfile();
    storage.saveProfile(p);
    for (const id of ['CASE_001', 'CASE_002', 'CASE_003']) submitReview(storage, p, id);
    renderAt('/review');
    await lead();
    expect(primary().getAttribute('data-state')).toBe('all_done');
    expect(primary().textContent).toBe('See your summary');
    expect(primary().getAttribute('href')).toBe('/review/summary');
    expect(screen.getByTestId('home-estimate').textContent).toBe('3 of 3 cases submitted');
    const deeper = screen.getByTestId('home-deeper');
    expect(within(deeper).getByRole('heading', { level: 2, name: 'Want to go deeper?' })).toBeTruthy();
    expect(within(deeper).getAllByRole('link').map((a) => [a.textContent, a.getAttribute('href')])).toEqual([
      ['Challenge the methodology', '/review/methods'],
      ['Find a case we got wrong', '/review/break'],
      ['Review the framework', '/review/framework'],
    ]);
    expect((screen.getByRole('button', { name: /Download your answers \(3\)/ }) as HTMLButtonElement).disabled).toBe(false);
    expect(main().querySelectorAll('.btn-primary')).toHaveLength(1);
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('keeps ?campaign= on its links, the calibration return path included', async () => {
    const { storage } = useReviewFixture();
    renderAt(`/review?campaign=${CAMPAIGN_ID}`);
    await lead();
    expect(primary().getAttribute('href')).toBe(`/review/profile?campaign=${CAMPAIGN_ID}`);
    cleanup();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt(`/review?campaign=${CAMPAIGN_ID}`);
    await lead();
    expect(primary().getAttribute('href')).toBe(`/review/start?campaign=${CAMPAIGN_ID}&next=${encodeURIComponent(`/review/case/CASE_001?campaign=${CAMPAIGN_ID}`)}`);
  });

  it('a build without review data says so with the command that builds it; a failed load offers Retry', async () => {
    useReviewFixture({});
    renderAt('/review');
    expect(await screen.findByText('No review data in this build', {}, T)).toBeTruthy();
    expect(main().textContent).toContain('build-review-data.mjs');
    cleanup();

    let k = 0;
    setReviewStoreForTests(
      new ReviewStore({
        json: async (file) => {
          if (file === 'manifest.json' && ++k === 1) throw new Error('offline');
          if (file === 'manifest.json') return manifest();
          throw new Error('offline');
        },
      }),
    );
    renderAt('/review');
    expect(await screen.findByText('The review data could not be loaded', {}, T)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    // the manifest loads on retry; the cases file still fails
    expect(await screen.findByText(/The review cases could not be loaded/, {}, T)).toBeTruthy();
  });

  it('an unknown review path goes to /review', async () => {
    useReviewFixture();
    const router = renderAt('/review/nothing-here');
    await lead();
    expect(router.state.location.pathname).toBe('/review');
  });
});

describe('journey helpers (ReviewLayout.tsx)', () => {
  const ctx = (o: Partial<Parameters<typeof primaryAction>[0]> = {}) => ({
    profile: koreaBatteriesProfile(),
    statusOf: () => 'not_started' as const,
    nextCase: CASES[0],
    allAssignedDone: false,
    calibrationSeen: true,
    carry: '',
    ...o,
  });

  it('primaryAction follows the journey', () => {
    expect(primaryAction(ctx({ profile: null }))).toMatchObject({ state: 'no_profile', to: '/review/profile' });
    expect(primaryAction(ctx({ nextCase: null }))).toMatchObject({ state: 'no_cases', to: '/review/cases' });
    expect(primaryAction(ctx({ calibrationSeen: false }))).toMatchObject({ state: 'calibration', to: '/review/start?next=%2Freview%2Fcase%2FCASE_001', caseId: 'CASE_001' });
    expect(primaryAction(ctx())).toMatchObject({ state: 'next', to: '/review/case/CASE_001' });
    expect(primaryAction(ctx({ statusOf: () => 'in_progress' }))).toMatchObject({ state: 'resume', to: '/review/case/CASE_001' });
    expect(primaryAction(ctx({ allAssignedDone: true, nextCase: null }))).toMatchObject({ state: 'all_done', to: '/review/summary', label: 'See your summary' });
  });

  it('estimateMinutes rounds to five minutes; withQuery keeps the campaign', () => {
    expect(estimateMinutes([])).toBe(0);
    expect(estimateMinutes([{ kind: 'posture_cell' }])).toBe(5);
    expect(estimateMinutes([{ kind: 'partnership' }, { kind: 'compact', threat: true, decision_use: true }])).toBe(20);
    expect(withQuery('/review/start', '?campaign=a b', { next: '/review/case/X?campaign=a' })).toBe('/review/start?campaign=a+b&next=%2Freview%2Fcase%2FX%3Fcampaign%3Da');
    expect(withQuery('/review', '')).toBe('/review');
  });

  it('safeReturnPath accepts review pages only', () => {
    expect(safeReturnPath('/review/case/CASE_001?campaign=x')).toBe('/review/case/CASE_001?campaign=x');
    expect(safeReturnPath('/review')).toBe('/review');
    for (const bad of [null, '', 'https://evil.example/review', '//evil.example/review', '/reviewer', '/countries/KOR', '/review/start?next=/review', 'javascript:alert(1)']) expect(safeReturnPath(bad)).toBeNull();
  });

  it('draftStatus reads v2 and v1 drafts', () => {
    expect(draftStatus(null)).toBe('not_started');
    expect(draftStatus({ version: 2, step: 'brief', reached: 'brief', blind: { choice: null, choices: [] }, notes: [] })).toBe('not_started');
    expect(draftStatus({ version: 2, step: 'brief', reached: 'brief', blind: { choice: null, choices: [] }, notes: [{ id: 'n', text: 't' }] })).toBe('in_progress');
    expect(draftStatus({ version: 2, step: 'reveal', reached: 'reveal', blind: { choice: 'partner' } })).toBe('in_progress');
    expect(draftStatus({ version: 1, step: 'context', blind: { choice: null, factors: [] } })).toBe('not_started');
    expect(draftStatus({ version: 1, step: 'blind', blind: { choice: null, factors: [] } })).toBe('in_progress');
  });

  it('the deeper links carry the names the home page uses', () => {
    expect(REVIEW_DEEPER_LINKS.map((l) => l.label)).toEqual(['Challenge the methodology', 'Find a case we got wrong', 'Review the framework']);
  });
});
