// The reviewer profile (/review/profile, review/views/ProfileView.tsx): the shared tags with a 1-5 rating per
// chosen one, an unrated choice blocks saving, the form autosaves as a draft and resumes after a reload, saving
// stores a normalised profile with a random reviewer id and goes to the calibration (first time) or the cases,
// a saved profile reloads into the form, and the review mode (decision by default, or technical) is saved on the
// profile and as the stored mode. Plus the pure form helpers. No bare acronyms or banned words.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { EMPTY_FORM, formErrors, formOf, profileOf, REVIEW_MODE_OPTION, typedCountries } from './ProfileView.tsx';
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
const box = (name: string) => screen.getByRole('checkbox', { name }) as HTMLInputElement;

describe('the profile form', () => {
  it('rates each chosen tag, autosaves a draft that resumes after a reload, then saves and goes to the cases', async () => {
    const { storage } = useReviewFixture();
    const router = renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
    const save = screen.getByRole('button', { name: 'Save profile' }) as HTMLButtonElement;
    expect(save.disabled).toBe(true);

    fireEvent.click(box('Batteries'));
    fireEvent.click(box('KOR'));
    fireEvent.change(screen.getByLabelText('Organization (optional)'), { target: { value: 'A ministry' } });
    // chosen but unrated: saving is blocked and the form says why
    expect(await screen.findByText(/Rate each area you chose/, {}, T)).toBeTruthy();
    expect(save.disabled).toBe(true);
    fireEvent.change(screen.getByRole('combobox', { name: 'Batteries' }), { target: { value: '5' } });

    // reload before saving: the draft resumes
    cleanup();
    renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    expect(box('Batteries').checked).toBe(true);
    expect(box('KOR').checked).toBe(true);
    expect((screen.getByLabelText('Organization (optional)') as HTMLInputElement).value).toBe('A ministry');
    expect((screen.getByRole('combobox', { name: 'Batteries' }) as HTMLSelectElement).value).toBe('5');
    expect(screen.getByTestId('draft-state').textContent).toContain('Resumed your unsaved draft');
    expect(storage.loadProfile()).toBeNull();

    fireEvent.change(screen.getByRole('combobox', { name: 'KOR' }), { target: { value: '4' } });
    fireEvent.click(screen.getByRole('switch', { name: /Stay anonymous/ }));
    fireEvent.click(screen.getByRole('button', { name: 'Save profile' }));
    cleanup();
    void router;

    const p = storage.loadProfile();
    expect(p).toMatchObject({ sectors: ['batteries'], countries: ['KOR'], confidence: { 'sector:batteries': 5, 'country:KOR': 4 }, organization: 'A ministry', anonymous: true, name: null });
    expect(p?.reviewer_id).toMatch(/^rv-/);
    expect(storage.listDrafts('profile')).toEqual([]);

    // a saved profile loads into the form, and keeps its id when saved again
    renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    expect(box('Batteries').checked).toBe(true);
    expect(screen.getByTestId('draft-state').textContent).toContain('Your saved profile');
    fireEvent.click(box('Refining'));
    fireEvent.change(screen.getByRole('combobox', { name: 'Refining' }), { target: { value: '3' } });
    fireEvent.click(screen.getByRole('button', { name: 'Save profile' }));
    await waitFor(() => expect(storage.loadProfile()?.stages).toEqual(['Refining']));
    expect(storage.loadProfile()?.reviewer_id).toBe(p?.reviewer_id);
  });

  it('the first save goes to the calibration; once it has been read, saving goes to the cases, which then list assignments', async () => {
    const { storage } = useReviewFixture();
    const fill = () => {
      fireEvent.click(box('Batteries'));
      fireEvent.click(box('KOR'));
      fireEvent.change(screen.getByRole('combobox', { name: 'Batteries' }), { target: { value: '5' } });
      fireEvent.change(screen.getByRole('combobox', { name: 'KOR' }), { target: { value: '5' } });
    };
    let router = renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    fill();
    fireEvent.click(screen.getByRole('button', { name: 'Save profile' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/start'));
    cleanup();
    storage.clearProfile();
    storage.markCalibrationSeen('2026-11-20T00:00:00Z', CAMPAIGN_ID);
    router = renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    fill();
    fireEvent.click(screen.getByRole('button', { name: 'Save profile' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/review/cases'));
    expect(await screen.findByTestId('assignment-CASE_001', {}, T)).toBeTruthy();
  });

  it('review mode: decision review by default; technical review is saved on the profile and as the stored mode', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    const group = screen.getByRole('group', { name: 'Review mode' });
    const decision = screen.getByRole('radio', { name: new RegExp(REVIEW_MODE_OPTION.decision.label) }) as HTMLInputElement;
    const technical = screen.getByRole('radio', { name: new RegExp(REVIEW_MODE_OPTION.technical.label) }) as HTMLInputElement;
    expect(decision.checked).toBe(true);
    expect(group.textContent).toContain('factual realism, strategic reasoning');
    expect(group.textContent).toContain('raw value → transformation → normalized indicator → contribution → score → threshold → classification');
    fireEvent.click(technical);
    fireEvent.click(screen.getByRole('button', { name: 'Save profile' }));
    await waitFor(() => expect(storage.loadProfile()?.review_mode).toBe('technical'));
    expect(storage.loadReviewMode()).toBe('technical');
    cleanup();
    renderAt('/review/profile');
    await screen.findByRole('heading', { level: 2, name: 'Your profile' }, T);
    expect((screen.getByRole('radio', { name: new RegExp(REVIEW_MODE_OPTION.technical.label) }) as HTMLInputElement).checked).toBe(true);
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });
});

describe('form helpers', () => {
  it('reads typed country codes and reports bad ones', () => {
    expect(typedCountries('idn, mex;; zz FRA')).toEqual(['IDN', 'MEX', 'FRA']);
    expect(formErrors({ ...EMPTY_FORM })).toEqual(['Choose at least one country, sector, stage or method you know.']);
    expect(formErrors({ ...EMPTY_FORM, sectors: ['solar'], countryText: 'IDN, Indonesia' }).join(' ')).toMatch(/Rate each area you chose \(1 to 5\): Solar, IDN\..*Indonesia is not one/);
  });

  it('keeps the id and creation time of the stored profile and drops the name when anonymous', () => {
    const prev = koreaBatteriesProfile();
    const p = profileOf({ ...EMPTY_FORM, name: 'X', anonymous: true, sectors: ['batteries'], confidence: { 'sector:batteries': 4, 'sector:solar': 2 } }, prev, '2027-01-01T00:00:00Z');
    expect(p).toMatchObject({ reviewer_id: prev.reviewer_id, created_at: prev.created_at, name: null, anonymous: true, confidence: { 'sector:batteries': 4 } });
    const fresh = profileOf({ ...EMPTY_FORM, name: ' Ana ', countryText: 'kor', confidence: { 'country:KOR': 3 } }, null, '2027-01-01T00:00:00Z', () => 'rv-x');
    expect(fresh).toMatchObject({ reviewer_id: 'rv-x', name: 'Ana', countries: ['KOR'], created_at: '2027-01-01T00:00:00Z', familiarity_neis: 0, review_mode: 'decision' });
    expect(profileOf({ ...EMPTY_FORM, review_mode: 'technical', sectors: ['batteries'], confidence: { 'sector:batteries': 4 } }, prev, 'x').review_mode).toBe('technical');
    // a stored v1 profile has no mode: decision review
    expect(formOf(koreaBatteriesProfile()).review_mode).toBe('decision');
  });
});
