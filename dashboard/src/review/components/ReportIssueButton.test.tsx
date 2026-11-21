// The review controls on Explorer pages (review/components/ReportIssueButton.tsx, ReviewToolbar.tsx): nothing at
// all for an ordinary visitor (no reviewer profile, VITE_REVIEW_UI unset), the button and the bar once a reviewer
// session is active; the bar stays off the review pages, remembers the last Explorer page, and its buttons open
// the dialog in the right mode.

import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { createReviewStorage, setReviewStorage, type KeyValueBackend } from '../storage.ts';
import type { ReviewerProfile } from '../model.ts';
import { ReportIssueButton } from './ReportIssueButton.tsx';
import { lastExplorerLocation, resetLastExplorerLocation, ReviewToolbar } from './ReviewToolbar.tsx';

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

const PROFILE: ReviewerProfile = {
  reviewer_id: 'rev-1',
  name: 'Ada Reviewer',
  anonymous: false,
  countries: ['KOR'],
  sectors: ['batteries'],
  stages: [],
  methods: ['trade_data'],
  confidence: { 'sector:batteries': 4, 'country:KOR': 4, 'method:trade_data': 4 },
  familiarity_neis: 1,
  familiarity_explorer: 1,
  advised_on: [],
  created_at: '2027-01-01T00:00:00Z',
};

function page(path: string, el: React.ReactNode) {
  const router = createMemoryRouter(
    [
      { path: '/countries/:iso3', element: el },
      { path: '/review', element: el },
    ],
    { initialEntries: [path] },
  );
  return render(<RouterProvider router={router} />);
}

beforeEach(() => {
  setReviewStorage(createReviewStorage(memoryBackend()));
  resetLastExplorerLocation();
});
afterEach(() => {
  cleanup();
  setReviewStorage(null);
});

describe('ReportIssueButton', () => {
  it('renders nothing for an ordinary visitor', () => {
    const { container } = page('/countries/KOR', <ReportIssueButton metric={{ id: 'need', label: 'Need', value: 0.5, display: '50%' }} />);
    expect(container.innerHTML).toBe('');
    expect(screen.queryByTestId('report-issue')).toBeNull();
  });

  it('shows with a reviewer session and opens the dialog with the metric', async () => {
    const store = createReviewStorage(memoryBackend());
    store.saveProfile(PROFILE);
    setReviewStorage(store);
    page('/countries/KOR?node=Batteries|Components', <ReportIssueButton metric={{ id: 'need', label: 'Supply-chain vulnerability', value: 0.5, display: '50%' }} />);
    const b = screen.getByTestId('report-issue');
    expect(b.getAttribute('aria-label')).toBe('Report an issue: Supply-chain vulnerability');
    fireEvent.click(b);
    const dlg = await screen.findByRole('dialog', {}, { timeout: 15_000 });
    expect(dlg.getAttribute('data-mode')).toBe('data_issue');
    expect(screen.getByText('Supply-chain vulnerability')).toBeTruthy();
  });

  it('the enabled prop forces it on', () => {
    page('/countries/KOR', <ReportIssueButton enabled label="Flag" />);
    expect(screen.getByTestId('report-issue').textContent).toContain('Flag');
  });
});

describe('ReviewToolbar', () => {
  it('renders nothing for an ordinary visitor but still remembers the page', () => {
    const { container } = page('/countries/KOR?node=Batteries|Components', <ReviewToolbar />);
    expect(container.innerHTML).toBe('');
    expect(lastExplorerLocation()).toBe('/countries/KOR?node=Batteries|Components');
  });

  it('with a session: report, break and a link back to the review; break opens the break dialog', async () => {
    const store = createReviewStorage(memoryBackend());
    store.saveProfile(PROFILE);
    setReviewStorage(store);
    page('/countries/KOR', <ReviewToolbar />);
    const bar = screen.getByTestId('review-toolbar');
    expect(bar.textContent).toContain('Report a data issue');
    expect(screen.getByRole('link', { name: 'Back to the review' }).getAttribute('href')).toBe('/review');
    fireEvent.click(screen.getByRole('button', { name: 'Break the model' }));
    const dlg = await screen.findByRole('dialog', {}, { timeout: 15_000 });
    expect(dlg.getAttribute('data-mode')).toBe('break_model');
  });

  it('is not shown on the review pages, which are not remembered', () => {
    const store = createReviewStorage(memoryBackend());
    store.saveProfile(PROFILE);
    setReviewStorage(store);
    page('/review', <ReviewToolbar />);
    expect(screen.queryByTestId('review-toolbar')).toBeNull();
    expect(lastExplorerLocation()).toBeNull();
  });
});
