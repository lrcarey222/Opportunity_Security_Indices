// The review controls placed on Explorer pages (posture/CellDrawer.tsx): for an ordinary visitor the drawer shows
// its five questions exactly as before, with no report button and no "Why this result?"; with a reviewer session
// (or VITE_REVIEW_UI=1), a collapsed "Why this result?" under the choice and a report button on the decision card
// and on the measure rows, each with its metric prefilled, opening the dialog with the drawer's route context. The drawer's own tests (posture/__tests__,
// country/__tests__) cover its existing content; this file covers only what the review system adds.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { setPostureStore } from '../../posture/store.ts';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { DRAWER_QUESTIONS, PostureNodeDrawer } from '../../posture/CellDrawer.tsx';
import { createReviewStorage, setReviewStorage, type KeyValueBackend } from '../storage.ts';
import type { ReviewerProfile } from '../model.ts';

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

function drawer() {
  const router = createMemoryRouter([{ path: '/countries/:iso3', element: <PostureNodeDrawer iso3="AAA" selected="Batteries|Refining" onClose={() => undefined} /> }], {
    initialEntries: ['/countries/AAA?node=Batteries|Refining'],
  });
  render(<RouterProvider router={router} />);
}

beforeEach(() => {
  setPostureStore(fixtureStore());
  setReviewStorage(createReviewStorage(memoryBackend()));
});
afterEach(() => {
  cleanup();
  setReviewStorage(null);
});

describe('the cell drawer with the review additions', () => {
  it('an ordinary visitor: the five questions, no report button and no "Why this result?"', async () => {
    drawer();
    const d = await screen.findByTestId('cell-drawer');
    expect(within(d).getAllByTestId('drawer-question').map((q) => q.getAttribute('data-question'))).toEqual(DRAWER_QUESTIONS.map((q) => q.id));
    // the choice answer has rendered (its detail is loaded) before checking what is absent
    await within(d).findByText(/Refining/);
    expect(within(d).queryAllByTestId('report-issue')).toEqual([]);
    expect(within(d).queryByTestId('why-this-result')).toBeNull();
  });

  it('with a reviewer session: a collapsed "Why this result?" in the choice', async () => {
    const store = createReviewStorage(memoryBackend());
    store.saveProfile(PROFILE);
    setReviewStorage(store);
    drawer();
    const d = await screen.findByTestId('cell-drawer');
    const why = await within(d).findByTestId('why-this-result');
    expect((why as HTMLDetailsElement).open).toBe(false);
    expect(why.closest('[data-question]')!.getAttribute('data-question')).toBe('choice');
    expect(within(d).queryByTestId('why-chain')).toBeNull();
  });

  it('with a reviewer session: report buttons on the decision card and the measure rows, with their metrics', async () => {
    const store = createReviewStorage(memoryBackend());
    store.saveProfile(PROFILE);
    setReviewStorage(store);
    drawer();
    const d = await screen.findByTestId('cell-drawer');
    const ids = within(d)
      .getAllByTestId('report-issue')
      .map((b) => b.getAttribute('data-metric'));
    expect(ids).toEqual(expect.arrayContaining(['choice', 'need', 'prize', 'cp', 'home_share']));
    const choice = within(d).getAllByTestId('report-issue').find((b) => b.getAttribute('data-metric') === 'choice')!;
    expect(choice.closest('[data-question]')!.getAttribute('data-question')).toBe('choice');
    fireEvent.click(choice);
    const dlg = await screen.findByRole('dialog', { name: 'What is wrong with this?' }, { timeout: 15_000 });
    const meta = within(dlg).getByTestId('issue-metadata');
    expect(meta.querySelector('[data-field="Metric"]')!.textContent).toBe('Strategic choice');
    expect(meta.querySelector('[data-field="Country"]')!.textContent).toBe('AAA');
    expect(meta.querySelector('[data-field="Stage"]')!.textContent).toBe('Refining');
    // the drawer stays open underneath
    expect(screen.getByTestId('cell-drawer')).toBeTruthy();
  });
});
