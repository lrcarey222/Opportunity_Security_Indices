// The "Report an issue" buttons beside an AIB node's class on the operational compact page (aib/views/compact/
// NodeSection.tsx) and in a Strategic Sectoral Compact's node evidence (aib/views/strategic/ChainSection.tsx): none
// for an ordinary visitor, one per node class with a reviewer session, carrying the class as its metric. Runs on
// the committed synthetic AIB fixture.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { resetLastCompact } from '../../aib/nav.ts';
import { fixtureSource, haveFixture, readDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { createReviewStorage, setReviewStorage } from '../storage.ts';
import type { ReviewerProfile } from '../model.ts';
import { installDomStubs, memoryBackend } from '../views/__fixtures__/reviewFixture.ts';

const PROFILE: ReviewerProfile = {
  reviewer_id: 'rev-1', name: null, anonymous: true, countries: [], sectors: [], stages: [], methods: [], confidence: {},
  familiarity_neis: 0, familiarity_explorer: 0, advised_on: [], created_at: '2027-01-01T00:00:00Z',
};

let restore: () => void = () => undefined;
beforeAll(() => {
  restore = installDomStubs();
});
afterAll(() => restore());
beforeEach(() => {
  setAibStoreForTests(new AibStore(fixtureSource(SYNTHETIC_DIR)));
  setReviewStorage(createReviewStorage(memoryBackend()));
});
afterEach(() => {
  cleanup();
  setAibStoreForTests(null);
  setReviewStorage(null);
  resetLastLocations();
  resetLastCompact();
});

const T = { timeout: 30_000 };
const reviewer = () => {
  const s = createReviewStorage(memoryBackend());
  s.saveProfile(PROFILE);
  setReviewStorage(s);
};
const renderAt = (path: string) => render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [path] })} />);
const classButtons = (el: HTMLElement) => [...el.querySelectorAll('[data-testid="report-issue"][data-metric="class"]')];

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('report buttons beside an AIB class', () => {
  const doc = haveFixture(SYNTHETIC_DIR) ? readDoc(SYNTHETIC_DIR) : null;
  const compact = doc?.compacts[0];

  async function nodeItems(): Promise<HTMLElement[]> {
    await screen.findAllByTestId('node-figures', {}, T);
    return screen.getAllByTestId('node-figures').map((f) => f.closest('li') as HTMLElement);
  }

  it('operational compact nodes: none for a visitor', async () => {
    renderAt(`/compacts/${compact!.id}#nodes`);
    const items = await nodeItems();
    expect(items.length).toBeGreaterThan(0);
    for (const li of items) expect(classButtons(li)).toEqual([]);
  });

  it('operational compact nodes: one per node class with a reviewer session', async () => {
    reviewer();
    renderAt(`/compacts/${compact!.id}#nodes`);
    const items = await nodeItems();
    for (const li of items) {
      const b = classButtons(li);
      expect(b.length).toBeGreaterThanOrEqual(1);
      expect(b[0].getAttribute('aria-label')).toMatch(/^Report an issue: Treatment of /);
    }
  });

  it("a strategic compact's node evidence: a button beside the class only with a reviewer session", async () => {
    for (const asReviewer of [false, true]) {
      cleanup();
      if (asReviewer) reviewer();
      renderAt('/compacts/s/SC-MAGNETS');
      const spine = await screen.findByRole('group', { name: /under Managed Interdependence$/ }, T);
      fireEvent.click(within(spine).getAllByRole('button')[1]);
      const ev = screen.getByTestId('node-evidence');
      expect(classButtons(ev).length).toBe(asReviewer ? 1 : 0);
    }
  });
});
