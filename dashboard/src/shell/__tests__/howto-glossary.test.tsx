// How to use the Explorer (/how-to-use) and the trusted network on the glossary page (usability pass, 2026-11-25).

import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { AVOID_RE } from '../../content/vocabulary.ts';
import { HOW_TO_WORKFLOWS } from '../HowToUseView.tsx';
import HowToUseView from '../HowToUseView.tsx';
import GlossaryView, { TRUSTED_NETWORK_TERM } from '../GlossaryView.tsx';
import { aboutNavKeyOf, placeOf, tabOf } from '../modules.ts';

// the posture core the trusted-network list reads: two tiers, one economy treated as allied, one outside
vi.mock('../../posture/store.ts', () => ({
  usePostureCore: () => ({
    status: 'ready',
    data: {
      created: '2026-11-21T14:51:17Z',
      countries: [
        { iso3: 'JPN', name: 'Japan', tier: 'core' },
        { iso3: 'IND', name: 'India', tier: 'partner' },
        { iso3: 'TWN', name: 'Taiwan', tier: 'other' },
        { iso3: 'CHN', name: 'China', tier: 'other' },
      ],
    },
    error: null,
    retry: () => undefined,
  }),
}));

afterEach(cleanup);

// react-router's Request with an AbortSignal from jsdom (the same shim as the other view tests)
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

function renderAt(path: string, element: React.ReactNode) {
  const [pathname] = path.split('#');
  const router = createMemoryRouter([{ path: pathname, element }], { initialEntries: [path] });
  render(<RouterProvider router={router} />);
}

describe('How to use the Explorer', () => {
  it('five workflows, each a heading, a few lines and links to pages that exist; plain words', () => {
    renderAt('/how-to-use', <HowToUseView />);
    expect(screen.getByRole('heading', { level: 1, name: 'How to use the Explorer' })).toBeTruthy();
    const list = screen.getByRole('list', { name: 'Workflows' });
    expect(within(list).getAllByRole('heading', { level: 2 }).map((h) => h.textContent)).toEqual([
      '1. Understand a sector',
      '2. Develop a country strategy',
      '3. Assess a partnership',
      '4. Design a compact',
      '5. Answer a specific research question',
    ]);
    // the entry routes of each workflow
    const hrefs = (id: string) => [...(document.getElementById(`how-${id}`)?.querySelectorAll('a') ?? [])].map((a) => a.getAttribute('href'));
    expect(hrefs('sector')).toContain('/sector');
    expect(hrefs('country')).toContain('/countries');
    expect(hrefs('partnership')).toContain('/aib/partnerships');
    expect(hrefs('compact')).toContain('/compacts');
    expect(hrefs('research')).toContain('/sector/explore');
    for (const w of HOW_TO_WORKFLOWS) {
      // short: two or three sentences
      const n = w.steps.split(/(?<=[.!?])\s+(?=[A-Z])/).length;
      expect(n, w.id).toBeGreaterThanOrEqual(2);
      expect(n, w.id).toBeLessThanOrEqual(3);
      // every link goes to a page of the Explorer (never an unknown path)
      for (const l of w.links) expect(placeOf(l.to.split(/[?#]/)[0]), l.to).not.toBeNull();
    }
    expect(document.body.textContent).not.toMatch(AVOID_RE);
    expect(document.body.textContent).not.toMatch(/the model (says|finds)|\bnodes?\b|residual/i);
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('is an About page: the About tab is current and its nav item is How to use', () => {
    expect(placeOf('/how-to-use')).toBe('about');
    expect(tabOf('/how-to-use')).toBe('about');
    expect(aboutNavKeyOf('/how-to-use')).toBe('howto');
  });
});

describe('the glossary page names who is in the trusted network', () => {
  it('shows the list inside the Trusted network entry, and the coalition entry links to it', async () => {
    renderAt('/glossary', <GlossaryView />);
    const entry = document.getElementById(`term-${TRUSTED_NETWORK_TERM}`) as HTMLElement;
    expect(entry).toBeTruthy();
    // jsdom has no IntersectionObserver, so the list mounts at once
    const control = await waitFor(() => within(entry).getByTestId('trusted-network'));
    expect(control.textContent).toContain('Trusted network: 3 economies');
    expect(control.textContent).toContain('Core members (1): Japan.');
    expect(control.textContent).toContain('Partners (1): India.');
    expect(control.textContent).toContain('Treated as allied (1): Taiwan.');
    expect(control.textContent).not.toContain('China,');
    // the list sits in the entry's definition, after its text
    expect(entry.querySelector('dd')?.contains(control)).toBe(true);
    const coalition = document.getElementById('term-coalition') as HTMLElement;
    expect(within(coalition).getByRole('link', { name: 'Trusted network' }).getAttribute('href')).toBe(`#term-${TRUSTED_NETWORK_TERM}`);
  });

  it('keeps one list when the filter hides and shows the entry again', async () => {
    renderAt('/glossary', <GlossaryView />);
    await waitFor(() => expect(screen.getAllByTestId('trusted-network')).toHaveLength(1));
    fireEvent.click(screen.getByRole('radio', { name: 'Sector' }));
    await waitFor(() => expect(screen.queryAllByTestId('trusted-network')).toHaveLength(0));
    fireEvent.click(screen.getByRole('radio', { name: 'Allied Industrial Base' }));
    await waitFor(() => expect(screen.getAllByTestId('trusted-network')).toHaveLength(1));
    expect(document.getElementById(`term-${TRUSTED_NETWORK_TERM}`)?.contains(screen.getByTestId('trusted-network'))).toBe(true);
  });
});
