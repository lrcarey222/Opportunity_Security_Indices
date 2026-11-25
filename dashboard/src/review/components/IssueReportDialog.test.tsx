// The data-issue / break-the-model dialog (review/components/IssueReportDialog.tsx): the page context comes from
// the router (country, technology and stage from /countries/KOR?node=Batteries|Components), the build stamps
// give the data release, the metadata is shown read-only, "what is wrong" and the severity are required, a
// break needs its kind, the report is saved through the local sink with an anonymous reviewer id when there is
// no profile, Escape closes without reaching a listener underneath, and a pasted address is parsed.

import { webcrypto } from 'node:crypto';
import { afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, type KeyValueBackend, type ReviewStorage } from '../storage.ts';
import { LocalSink } from '../sink.ts';
import { resetBuildInfo, setAibBuild, setCountryBuild, setPostureBuild } from '../../shell/buildInfo.ts';
import type { DataIssuePayload, Submission } from '../model.ts';
import { IssueReportDialog, locationOfAddress, reviewerProfileOrAnonymous } from './IssueReportDialog.tsx';
import { memoryReviewSource, ReviewStore } from '../data/store.ts';
import type { ReleaseStamps } from '../release.ts';

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

// jsdom's crypto has no SubtleCrypto: the release ID needs it
const hadSubtle = !!globalThis.crypto?.subtle;
beforeAll(() => {
  if (!hadSubtle) Object.defineProperty(globalThis, 'crypto', { configurable: true, value: webcrypto });
});

let store: ReviewStorage;
beforeEach(() => {
  store = createReviewStorage(memoryBackend());
  setPostureBuild({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
  setCountryBuild({ created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' });
  setAibBuild({ created: '2026-11-17T19:07:29Z' });
});
afterEach(() => {
  cleanup();
  resetBuildInfo();
});

function open(path: string, props: Partial<React.ComponentProps<typeof IssueReportDialog>> = {}) {
  const onClose = vi.fn();
  const onSubmitted = vi.fn();
  const router = createMemoryRouter(
    [{ path: '/countries/:iso3', element: <IssueReportDialog open onClose={onClose} store={store} sink={new LocalSink(store)} onSubmitted={onSubmitted} now={() => '2027-01-20T12:00:00.000Z'} {...props} /> }],
    { initialEntries: [path] },
  );
  render(<RouterProvider router={router} />);
  return { onClose, onSubmitted };
}

const field = (name: string) => screen.getByTestId('issue-metadata').querySelector(`[data-field="${name}"]`)?.textContent ?? null;

describe('IssueReportDialog', () => {
  it('captures the country, technology and stage from the route and the data release from the stamps', async () => {
    open('/countries/KOR?node=Batteries|Components', { metric: { id: 'home_share', label: 'World supply share', value: 0.301, display: '30.1%', tech: 'Batteries', stage: 'Components' } });
    const dlg = screen.getByRole('dialog');
    expect(dlg.getAttribute('aria-modal')).toBe('true');
    expect(field('Country')).toBe('KOR');
    expect(field('Sector / technology')).toBe('Batteries');
    expect(field('Stage')).toBe('Components');
    expect(field('Metric')).toBe('World supply share');
    expect(field('Value shown')).toBe('30.1%');
    expect(field('Source')).toBe('not stated on this page');
    expect(field('Page')).toBe('/countries/KOR?node=Batteries|Components');
    expect(field('App version (git commit)')).toBe('unknown');
    await waitFor(() => expect(field('Data release')).toMatch(/^rel-[0-9a-f]{12} \(partial: sector, strategic, memos not loaded on this page\)$/));
    // the metadata is read-only: no input inside it
    expect(within(screen.getByTestId('issue-metadata')).queryAllByRole('textbox')).toEqual([]);
  });

  it('requires what is wrong and a severity, then saves locally with the page context (anonymous without a profile)', async () => {
    const { onSubmitted } = open('/countries/KOR?node=Batteries|Components');
    await waitFor(() => expect(field('Data release')).toMatch(/^rel-/));
    fireEvent.click(screen.getByRole('button', { name: 'Submit the report' }));
    expect(screen.getByText('Say what is wrong.')).toBeTruthy();
    expect(screen.getByText('Choose a severity.')).toBeTruthy();
    expect(store.listSubmissions()).toEqual([]);

    fireEvent.change(screen.getByLabelText(/What is wrong\?/), { target: { value: 'The share counts re-exports through Hong Kong.' } });
    fireEvent.change(screen.getByLabelText(/How serious is it\?/), { target: { value: 'material' } });
    fireEvent.change(screen.getByLabelText('Corrected value (optional)'), { target: { value: '12%' } });
    fireEvent.click(screen.getByRole('button', { name: 'Submit the report' }));
    await screen.findByTestId('issue-result');
    const subs = store.listSubmissions();
    expect(subs).toHaveLength(1);
    const s = subs[0] as Submission<'data_issue'>;
    expect(s.kind).toBe('data_issue');
    expect(s.reviewer.anonymous).toBe(true);
    expect(s.reviewer.reviewer_id).toMatch(/^anon-/);
    expect(s.data_release).toMatch(/^rel-[0-9a-f]{12}$/);
    expect(s.page_context).toMatchObject({ country: 'KOR', tech: 'batteries', stage: 'Components', node: 'Batteries|Components', route: '/countries/:iso3' });
    const p: DataIssuePayload = s.payload;
    expect(p).toMatchObject({ what_is_wrong: 'The share counts re-exports through Hong Kong.', severity: 'material', corrected_value: '12%', citation: null });
    expect(p.break_type).toBeUndefined();
    expect(onSubmitted).toHaveBeenCalledTimes(1);
    // the same anonymous id is reused next time
    expect(reviewerProfileOrAnonymous(store).reviewer_id).toBe(s.reviewer.reviewer_id);
  });

  it('break-the-model mode also requires the kind of break', async () => {
    open('/countries/KOR', { mode: 'break_model' });
    fireEvent.change(screen.getByLabelText(/What is wrong\?/), { target: { value: 'Threshold artifact at 0.75.' } });
    fireEvent.change(screen.getByLabelText(/How serious is it\?/), { target: { value: 'critical' } });
    fireEvent.click(screen.getByRole('button', { name: 'Submit the report' }));
    expect(screen.getByText('Choose the kind of break.')).toBeTruthy();
    fireEvent.change(screen.getByLabelText(/What kind of break is it\?/), { target: { value: 'threshold_artifact' } });
    fireEvent.click(screen.getByRole('button', { name: 'Submit the report' }));
    await screen.findByTestId('issue-result');
    const s = store.listSubmissions()[0] as Submission<'break_model'>;
    expect(s.kind).toBe('break_model');
    expect(s.payload.break_type).toBe('threshold_artifact');
  });

  it('a target address replaces the router location', async () => {
    open('/countries/USA', { mode: 'break_model', target: 'https://lrcarey222.github.io/aib-explorer/#/compacts/s/SC-BATTERIES?scn=decoupling' });
    expect(field('Page')).toBe('/compacts/s/SC-BATTERIES?scn=decoupling');
    expect(field('Country')).toBe('–');
  });

  it('Escape closes the dialog and does not reach a window listener underneath (the cell drawer)', () => {
    const under = vi.fn();
    window.addEventListener('keydown', under);
    const { onClose } = open('/countries/KOR');
    fireEvent.keyDown(screen.getByLabelText(/What is wrong\?/), { key: 'Escape' });
    expect(onClose).toHaveBeenCalledTimes(1);
    expect(under).not.toHaveBeenCalled();
    window.removeEventListener('keydown', under);
  });

  it('keeps Tab inside the dialog', () => {
    open('/countries/KOR');
    const dlg = screen.getByRole('dialog');
    const cancel = screen.getByRole('button', { name: 'Cancel' });
    cancel.focus();
    fireEvent.keyDown(cancel, { key: 'Tab' });
    expect(dlg.contains(document.activeElement)).toBe(true);
    expect(document.activeElement).toBe(screen.getByRole('button', { name: 'Close the report' }));
  });
});

describe('locationOfAddress', () => {
  it('reads paths, full links and hash routes', () => {
    expect(locationOfAddress('/countries/KOR?node=Batteries|Components')).toMatchObject({ pathname: '/countries/KOR', search: '?node=Batteries|Components' });
    expect(locationOfAddress('https://x.org/aib/partnerships?country=JPN')).toMatchObject({ pathname: '/aib/partnerships', search: '?country=JPN' });
    expect(locationOfAddress('https://x.org/#/countries/JPN')).toMatchObject({ pathname: '/countries/JPN' });
    expect(locationOfAddress('   ')).toBeNull();
  });
});

describe('IssueReportDialog: the data release a report records', () => {
  // the full release of the review manifest; the page published posture, country and aib (beforeEach)
  const FULL: ReleaseStamps = {
    posture: { created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' },
    country: { created: '2026-10-31T15:58:15Z', git_commit: '4bb2adc' },
    sector: { bundle_id: 'sec1' },
    aib: { created: '2026-11-17T19:07:29Z' },
    strategic: { bundle_id: 'str1' },
    memos: { manifest_sha256: 'abc' },
  };
  const reviewData = (stamps: ReleaseStamps) =>
    new ReviewStore(memoryReviewSource({ 'manifest.json': { schema: 'aib-review-manifest', schema_version: 1, built_at: 't', app_commit: 'x', data_release: { id: 'rel-aaaaaaaaaaaa', stamps, complete: true, missing: [] }, campaigns: [], files: [] } }));
  const send = async () => {
    fireEvent.change(screen.getByLabelText(/What is wrong\?/), { target: { value: 'Wrong share.' } });
    fireEvent.change(screen.getByLabelText(/How serious is it\?/), { target: { value: 'minor' } });
    fireEvent.click(screen.getByRole('button', { name: 'Submit the report' }));
    await screen.findByTestId('issue-result');
    return store.listSubmissions()[0] as Submission<'data_issue'>;
  };

  it('records the review manifest release when every stamp the page loaded matches it', async () => {
    open('/countries/KOR', { reviewData: reviewData(FULL) });
    await waitFor(() => expect(field('Data release')).toBe('rel-aaaaaaaaaaaa'));
    expect(field('Release check')).toMatch(/match the review release/);
    const s = await send();
    expect(s.data_release).toBe('rel-aaaaaaaaaaaa');
    expect(s.page_context!.data_release).toBe('rel-aaaaaaaaaaaa');
    expect(s.page_context!.release_note).toBeNull();
    // the page's own stamps are still recorded one by one
    expect(s.page_context!.bundle_stamps.posture).toMatchObject({ created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' });
    expect(s.page_context!.bundle_stamps.sector).toBeNull();
  });

  it('keeps the partial page ID and names the differing stamps when they do not match', async () => {
    open('/countries/KOR', { reviewData: reviewData({ ...FULL, posture: { created: '2026-11-18T00:00:00Z', git_commit: 'ffffffff' } }) });
    await waitFor(() => expect(field('Release check')).toBe('page stamps differ from review release rel-aaaaaaaaaaaa in: posture'));
    expect(field('Data release')).toMatch(/^rel-[0-9a-f]{12} \(partial: sector, strategic, memos not loaded on this page\)$/);
    const s = await send();
    expect(s.data_release).toMatch(/^rel-[0-9a-f]{12}$/);
    expect(s.data_release).not.toBe('rel-aaaaaaaaaaaa');
    expect(s.page_context!.release_note).toBe('page stamps differ from review release rel-aaaaaaaaaaaa in: posture');
  });

  it('keeps the partial page ID with a note when the review manifest cannot be read', async () => {
    open('/countries/KOR', { reviewData: new ReviewStore(memoryReviewSource({})) });
    await waitFor(() => expect(field('Release check')).toMatch(/review manifest could not be read/));
    const s = await send();
    expect(s.data_release).toMatch(/^rel-[0-9a-f]{12}$/);
    expect(s.page_context!.release_note).toMatch(/review manifest could not be read/);
  });
});
