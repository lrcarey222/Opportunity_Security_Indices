// The calibration screen (/review/start, review/views/CalibrationView.tsx): seven short points (managed
// interdependence, dependence and domestic production not automatically good or bad, Partner and Watch (keep open)
// as successes, surprises, both kinds of error, the reviewer's job), no methods tutorial; "Continue" records the
// flag for the campaign and goes to the return path (?next=, review pages only), else the next open case, else
// the profile; afterwards the home page and the case links go straight to the case. One h1, no bare acronyms or
// banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE, CHOICE_NAME } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { CALIBRATION_POINTS } from './CalibrationView.tsx';
import { CAMPAIGN_ID, installDomStubs, koreaBatteriesProfile, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

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
const heading = () => screen.findByRole('heading', { level: 2, name: 'Before you start' }, T);

describe('/review/start', () => {
  it('says the seven points in plain words, with Watch shown as "Watch (keep open)"', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review/start');
    await heading();
    expect(screen.getAllByRole('heading', { level: 1 }).map((h) => h.textContent)).toEqual(['Expert review']);
    const points = screen.getByTestId('calibration-points');
    expect(within(points).getAllByRole('listitem')).toHaveLength(7);
    expect(CALIBRATION_POINTS.map((p) => p.id)).toEqual(['managed', 'dependence', 'domestic', 'success', 'surprise', 'both_ways', 'job']);
    const text = points.textContent ?? '';
    expect(text).toContain('managed interdependence, not self-sufficiency');
    expect(text).toContain('Dependence is not automatically a problem');
    expect(text).toContain('Domestic production is not automatically preferred');
    expect(text).toContain(`${CHOICE_NAME.partner} and Watch (keep open) can be successful conclusions`);
    expect(text).toContain('A surprising result is not necessarily an error');
    expect(text).toMatch(/false alarms and missed risks/);
    expect(text).toContain('not to agree with NEIS');
    // short: not a methods tutorial
    expect(text.split(/\s+/).length).toBeLessThan(260);
    expect(storage.loadCalibrationSeen(CAMPAIGN_ID)).toBeNull();
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('Continue records the flag and goes to the return path; afterwards the home action goes straight to the case', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    const router = renderAt(`/review/start?next=${encodeURIComponent('/review/case/CASE_002')}`);
    await heading();
    fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/case/CASE_002'), T);
    expect(storage.loadCalibrationSeen(CAMPAIGN_ID)).toMatch(/^\d{4}-\d\d-\d\dT/);
    cleanup();
    renderAt('/review');
    const cta = await screen.findByTestId('home-primary', {}, T);
    expect(cta.getAttribute('href')).toBe('/review/case/CASE_001');
    cleanup();
    renderAt('/review/cases');
    await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    expect(within(screen.getByTestId('assignment-CASE_002')).getByRole('link', { name: 'Start' }).getAttribute('href')).toBe('/review/case/CASE_002');
  });

  it('keeps the campaign; ignores a return path outside the review pages and falls back to the next open case', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    const router = renderAt(`/review/start?campaign=${CAMPAIGN_ID}&next=${encodeURIComponent('https://example.org/review')}`);
    await heading();
    fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/case/CASE_001'), T);
    expect(router.state.location.search).toBe(`?campaign=${CAMPAIGN_ID}`);
  });

  it('without a profile, Continue goes to the profile', async () => {
    const { storage } = useReviewFixture();
    const router = renderAt('/review/start');
    await heading();
    fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/profile'), T);
    expect(storage.loadCalibrationSeen(CAMPAIGN_ID)).not.toBeNull();
  });

  it('before the calibration, a case not yet started opens through it', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review/cases');
    await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    expect(within(screen.getByTestId('assignment-CASE_002')).getByRole('link', { name: 'Start' }).getAttribute('href')).toBe(`/review/start?next=${encodeURIComponent('/review/case/CASE_002')}`);
  });
});
