// The break-the-model page (review/views/BreakModelView.tsx): it explains the mode, lists the six kinds of break,
// offers the last Explorer page visited as the page to report on (or a pasted address), opens the dialog in
// break mode with that page's context, and lists the reviewer's past break reports.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, type KeyValueBackend, type ReviewStorage } from '../storage.ts';
import { memoryReviewSource, ReviewStore, setReviewStoreForTests } from '../data/store.ts';
import { BREAK_TYPE_LABEL, type ReviewerProfile } from '../model.ts';
import { newSubmission, capturePageContext } from '../context.ts';
import { rememberExplorerLocation, resetLastExplorerLocation } from '../components/ReviewToolbar.tsx';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import BreakModelView from './BreakModelView.tsx';

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

const PROFILE: ReviewerProfile = {
  reviewer_id: 'rev-1', name: null, anonymous: true, countries: [], sectors: [], stages: [], methods: [], confidence: {},
  familiarity_neis: 0, familiarity_explorer: 0, advised_on: [], created_at: '2027-01-01T00:00:00Z',
};

let store: ReviewStorage;
beforeEach(() => {
  store = createReviewStorage(memoryBackend());
  resetLastExplorerLocation();
  resetLastLocations();
  setReviewStoreForTests(new ReviewStore(memoryReviewSource({})));
});
afterEach(() => {
  cleanup();
  setReviewStoreForTests(null);
});

function show() {
  const router = createMemoryRouter([{ path: '/review/break', element: <BreakModelView storage={store} /> }], { initialEntries: ['/review/break'] });
  return render(<RouterProvider router={router} />);
}

describe('BreakModelView', () => {
  it('lists the six kinds of break and defaults to the last Explorer page', async () => {
    rememberExplorerLocation('/countries/KOR', '?node=Batteries|Components');
    show();
    const kinds = screen.getByTestId('break-types');
    for (const label of Object.values(BREAK_TYPE_LABEL)) expect(within(kinds).getByText(label)).toBeTruthy();
    const input = screen.getByLabelText('Explorer page') as HTMLInputElement;
    expect(input.value).toBe('/countries/KOR?node=Batteries|Components');
    fireEvent.click(screen.getByRole('button', { name: 'Report a break on this page' }));
    const dlg = await screen.findByRole('dialog');
    expect(dlg.getAttribute('data-mode')).toBe('break_model');
    const meta = screen.getByTestId('issue-metadata');
    expect(meta.querySelector('[data-field="Country"]')!.textContent).toBe('KOR');
    expect(meta.querySelector('[data-field="Stage"]')!.textContent).toBe('Components');
    expect(screen.getByLabelText(/What kind of break is it\?/)).toBeTruthy();
  });

  it('a pasted address that is not an Explorer page cannot be reported on', () => {
    show();
    const input = screen.getByLabelText('Explorer page');
    fireEvent.change(input, { target: { value: '/review/admin' } });
    expect((screen.getByRole('button', { name: 'Report a break on this page' }) as HTMLButtonElement).disabled).toBe(true);
    fireEvent.change(input, { target: { value: 'https://example.org/#/compacts/s/SC-BATTERIES' } });
    expect((screen.getByRole('button', { name: 'Report a break on this page' }) as HTMLButtonElement).disabled).toBe(false);
  });

  it('lists the reviewer past break reports', () => {
    const ctx = capturePageContext({ pathname: '/countries/DEU', search: '?node=Solar|Assembly' }, {}, null, { captured_at: '2027-01-02T00:00:00Z' });
    store.saveSubmission(
      newSubmission(
        'break_model',
        { metric: null, what_is_wrong: 'Competitive Position lands exactly on 0.75 and the inclusive rule makes it Preserve.', severity: 'material', break_type: 'threshold_artifact' },
        { profile: PROFILE, campaign_id: null, page_context: ctx, release: { data_release: null, app_commit: 'unknown' }, now: '2027-01-02T00:00:00Z', uuid: () => 'b1' },
      ),
    );
    show();
    const past = screen.getByTestId('break-past');
    expect(past.textContent).toContain(BREAK_TYPE_LABEL.threshold_artifact);
    expect(past.textContent).toContain('/countries/DEU?node=Solar|Assembly');
    expect(past.textContent).toContain('Material');
  });
});
