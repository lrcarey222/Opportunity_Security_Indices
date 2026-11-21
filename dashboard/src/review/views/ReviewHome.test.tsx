// The review start page and layout (/review, review/views/ReviewHome.tsx, review/ReviewLayout.tsx): the page head
// "Expert review" with the module links (no admin link) and the progress indicator, the campaign intro (time per
// case, nothing published), the storage mode said plainly, start vs continue, a resume link to an unfinished
// case, the injected store, the screens for a build without review data and for a failed load, no top-level tab
// for /review, the document title, and no bare acronyms or banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { placeOf, tabOf } from '../../shell/modules.ts';
import { pageTitle } from '../../shell/title.ts';
import { ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import { REVIEW_LINKS } from '../ReviewLayout.tsx';
import { SINK_MODE_TEXT } from './ReviewHome.tsx';
import { CAMPAIGN_ID, installDomStubs, koreaBatteriesProfile, manifest, RELEASE, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

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

describe('/review', () => {
  it('introduces the campaign, says how answers travel, and starts with the profile', async () => {
    useReviewFixture();
    renderAt('/review');
    expect(await screen.findByRole('heading', { level: 1, name: 'Expert review' }, T)).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 2, name: 'Test campaign' }, T)).toBeTruthy();
    const nav = screen.getByRole('navigation', { name: 'Expert review pages' });
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual(REVIEW_LINKS.map((l) => l.label));
    expect(within(nav).queryByRole('link', { name: /admin/i })).toBeNull();
    expect(main().textContent).toContain('10 to 15 minutes');
    expect(main().textContent).toContain('Nothing you write is published');
    // the build's sink is local in tests: said plainly
    expect(screen.getByText(SINK_MODE_TEXT.local.title)).toBeTruthy();
    expect(main().textContent).toContain('draft: answers given now are for testing');
    expect(screen.getByRole('link', { name: 'Start with your profile' }).getAttribute('href')).toBe('/review/profile');
    expect(main().textContent).toContain(RELEASE);
    // the build does not carry the campaign's rules yet: said, for NEIS
    expect(main().textContent).toMatch(/does not carry the campaign.s assignment and target reviews per anchor/);
    expect(document.title).toBe(pageTitle('Overview', 'review'));
    expect(document.title.startsWith('Overview | Expert review |')).toBe(true);
    // a place, no top-level tab
    expect(placeOf('/review/case/CASE_001')).toBe('review');
    expect(tabOf('/review')).toBeNull();
    expect(screen.getByRole('navigation', { name: 'Modules' }).querySelector('[aria-current]')).toBeNull();
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('with a profile: progress, continue, a resume link to an unfinished case and the download of answers', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    storage.saveDraft(CAMPAIGN_ID, 'case', 'CASE_001', { version: 1, case_id: 'CASE_001', step: 'blind', blind: { choice: 'secure', factors: ['home_share'] }, reveal: {} });
    renderAt('/review');
    await screen.findByRole('heading', { level: 2, name: 'Continue' }, T);
    expect(screen.getByRole('link', { name: 'Resume your case' }).getAttribute('href')).toBe('/review/case/CASE_001');
    expect(screen.getByRole('link', { name: /resume Korea: battery component manufacturing/ })).toBeTruthy();
    const progress = screen.getByTestId('review-progress');
    expect(progress.textContent).toContain('0 of 3 submitted');
    expect((screen.getByRole('button', { name: /Download your answers \(0\)/ }) as HTMLButtonElement).disabled).toBe(true);
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('keeps ?campaign= on its links', async () => {
    useReviewFixture();
    renderAt(`/review?campaign=${CAMPAIGN_ID}`);
    const link = await screen.findByRole('link', { name: 'Start with your profile' }, T);
    expect(link.getAttribute('href')).toBe(`/review/profile?campaign=${CAMPAIGN_ID}`);
  });

  it('a build without review data says so with the command that builds it; a failed load offers Retry', async () => {
    useReviewFixture({});
    renderAt('/review');
    expect(await screen.findByText('No review data in this build', {}, T)).toBeTruthy();
    expect(main().textContent).toContain('build-review-data.mjs');
    cleanup();

    let n = 0;
    setReviewStoreForTests(
      new ReviewStore({
        json: async (file) => {
          if (file === 'manifest.json' && ++n === 1) throw new Error('offline');
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
    await screen.findByRole('heading', { level: 2, name: 'Test campaign' }, T);
    expect(router.state.location.pathname).toBe('/review');
  });
});
