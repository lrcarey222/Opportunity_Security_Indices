// The assignments page (/review/cases, review/views/AssignmentsView.tsx): a Korea-batteries reviewer is given only
// in-expertise and anchor cases as "Your cases" (never the solar anchor, the graphite step or the method-wide
// case), the one case beyond the stated expertise appears separately as optional and says so, a self-selected
// case is added and remembered, completion shows per case, and without a profile the page asks for one. UX v2:
// every case kind has a label (sector chains included), each open case shows its rough time, the optional cases
// name the reviewer states, and once every assigned case is in the page links to the summary.

import { afterAll, afterEach, beforeAll, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { newSubmission } from '../context.ts';
import { CASE_KINDS, type CaseReviewPayload } from '../model.ts';
import { CASE_KIND_LABEL } from './AssignmentsView.tsx';
import { CAMPAIGN_ID, installDomStubs, koreaBatteriesProfile, RELEASE, resetReviewFixture, useReviewFixture } from './__fixtures__/reviewFixture.ts';

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

  it('labels every case kind, shows the time per open case, names the reviewer states, and links to the summary once all are in', async () => {
    for (const k of CASE_KINDS) expect(CASE_KIND_LABEL[k]).toBeTruthy();
    expect(CASE_KIND_LABEL.sector_chain).toBe('Sector supply chain');
    const { storage } = useReviewFixture();
    const p = koreaBatteriesProfile();
    storage.saveProfile(p);
    renderAt('/review/cases');
    await screen.findByRole('list', { name: 'Your assigned cases' }, T);
    expect(screen.getByTestId('assignment-CASE_001').textContent).toContain('~8 minutes');
    expect(screen.getByTestId('assignment-CASE_002').textContent).toContain('~6 minutes');
    expect(main().textContent).toContain('About 20 minutes left.');
    expect(main().textContent).toContain('“Insufficient evidence to judge” or “Outside my expertise”');
    expect(main().textContent).not.toMatch(/I don.t know/);
    expect(screen.queryByTestId('assignments-done')).toBeNull();
    cleanup();
    let k = 0;
    for (const id of ['CASE_001', 'CASE_002', 'CASE_003']) {
      const payload: CaseReviewPayload = {
        case_id: id, case_snapshot_sha256: 'x'.repeat(64), assignment_reason: 'assigned', expertise_match: 'in',
        blind: { choice: 'watch', confidence: 3, factors: [], answered_at: '2026-11-20T00:00:00Z', ux_version: 2 },
        reveal: { revealed_at: '2026-11-20T00:01:00Z', verdict: 1, verdict_code: 'same_same' }, outside_expertise: false, time_spent_s: 60,
      };
      storage.saveSubmission(newSubmission('case_review', payload, { profile: p, campaign_id: CAMPAIGN_ID, release: { data_release: RELEASE, app_commit: 'test' }, uuid: () => `as-${++k}` }));
    }
    renderAt('/review/cases');
    const done = await screen.findByTestId('assignments-done', {}, T);
    expect(within(done).getByRole('link', { name: 'See your summary' }).getAttribute('href')).toBe('/review/summary');
    expect(within(screen.getByTestId('assignment-CASE_001')).getByRole('link', { name: 'Look again' }).getAttribute('href')).toBe('/review/case/CASE_001');
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('asks for a profile first', async () => {
    useReviewFixture();
    renderAt('/review/cases');
    expect(await screen.findByRole('link', { name: 'Fill in your profile first' }, T)).toBeTruthy();
    expect(screen.queryByRole('list', { name: 'Your assigned cases' })).toBeNull();
  });
});
