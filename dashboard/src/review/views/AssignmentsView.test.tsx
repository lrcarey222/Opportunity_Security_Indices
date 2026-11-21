// The assignments page (/review/cases, review/views/AssignmentsView.tsx): a Korea-batteries reviewer is given only
// in-expertise and anchor cases as "Your cases" (never the solar anchor, the graphite step or the method-wide
// case), the one case beyond the stated expertise appears separately as optional and says so, a self-selected
// case is added and remembered, completion shows per case, and without a profile the page asks for one.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
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
const items = (list: HTMLElement) => within(list).getAllByRole('listitem').map((li) => ({ id: li.getAttribute('data-testid')?.replace('assignment-', ''), reason: li.getAttribute('data-reason'), match: li.getAttribute('data-match') }));

describe('/review/cases', () => {
  it('gives a Korea-batteries reviewer only in-expertise and anchor cases, with one optional case beyond it labelled so', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    renderAt('/review/cases');
    const list = await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    const mine = items(list);
    expect(mine.map((x) => x.id)).toEqual(['CASE_001', 'CASE_002', 'CASE_003']);
    expect(mine[0]).toMatchObject({ reason: 'anchor', match: 'in' });
    for (const x of mine) expect(x.match).toBe('in');
    // never the solar anchor, the graphite step or the method-wide case among the assigned ones
    for (const id of ['CASE_004', 'CASE_005', 'CASE_006']) expect(within(list).queryByTestId(`assignment-${id}`)).toBeNull();
    const optional = items(screen.getByRole('list', { name: 'Optional cases' }));
    expect(optional).toHaveLength(1);
    expect(optional[0]).toMatchObject({ reason: 'outside', match: 'outside' });
    expect(optional[0].id).not.toBe('CASE_006');
    expect(within(screen.getByRole('list', { name: 'Optional cases' })).getByText('Optional: beyond your stated expertise')).toBeTruthy();
    // the head's indicator and the page's bar both count the three required cases
    expect(screen.getAllByRole('progressbar').map((b) => b.getAttribute('aria-valuemax'))).toEqual(['3', '3']);
    expect(bareAcronyms(main())).toEqual([]);
    expect(document.body.textContent).not.toMatch(AVOID_RE);
  });

  it('adds a self-selected case and remembers it; shows each case’s completion', async () => {
    const { storage } = useReviewFixture();
    storage.saveProfile(koreaBatteriesProfile());
    storage.saveDraft(CAMPAIGN_ID, 'case', 'CASE_002', { version: 1, case_id: 'CASE_002', step: 'blind', blind: { choice: 'watch', factors: [] }, reveal: {} });
    renderAt('/review/cases');
    await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    expect(within(screen.getByTestId('assignment-CASE_002')).getByTestId('case-status').textContent).toBe('In progress');
    expect(within(screen.getByTestId('assignment-CASE_003')).getByTestId('case-status').textContent).toBe('Not started');
    fireEvent.change(screen.getByRole('combobox', { name: 'Case' }), { target: { value: 'CASE_006' } });
    const optional = await screen.findByRole('list', { name: 'Optional cases' }, T);
    expect(items(optional).find((x) => x.id === 'CASE_006')).toMatchObject({ reason: 'self_selected' });
    expect(storage.loadDraft(CAMPAIGN_ID, 'meta', 'self_selected')).toBe('CASE_006');
    // the required list keeps its minimum
    expect(items(screen.getByRole('list', { name: 'Your assigned cases' })).length).toBeGreaterThanOrEqual(2);
  });

  it('asks for a profile first', async () => {
    useReviewFixture();
    renderAt('/review/cases');
    expect(await screen.findByRole('link', { name: 'Fill in your profile first' }, T)).toBeTruthy();
    expect(screen.queryByRole('list', { name: 'Your assigned cases' })).toBeNull();
  });
});
