import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { resetYFCache } from '../data/store.ts';
import { SYN_BUNDLE, SYN_MANIFEST, SYN_REFERENCE } from '../__fixtures__/synthetic.ts';
import { decodeState, stateSearch, type AssessState } from '../state/assessment.ts';

const restore: (() => void)[] = [];

beforeAll(() => {
  window.matchMedia = ((q: string) => ({
    matches: /min-width: 1024px/.test(q), media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  // the data router builds a Request with jsdom's AbortSignal, which Node rejects (as in app.test.tsx)
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
  const realFetch = globalThis.fetch;
  globalThis.fetch = vi.fn(async (url: RequestInfo | URL) => {
    const u = String(url);
    const body = /yards-fences\/manifest\.json$/.test(u) ? SYN_MANIFEST : /reference\./.test(u) ? SYN_REFERENCE : /evidence-USA/.test(u) ? SYN_BUNDLE : null;
    return body ? new Response(JSON.stringify(body), { status: 200 }) : new Response('not found', { status: 404 });
  }) as unknown as typeof fetch;
  restore.push(() => (globalThis.fetch = realFetch));
});

afterEach(() => {
  cleanup();
  resetLastLocations();
  resetYFCache();
});
afterAll(() => restore.forEach((f) => f()));

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const state = (patch: Partial<AssessState>): AssessState => ({ country: 'USA', sel: null, mode: 'analyst', layers: {}, step: 0, ...patch });

describe('Draw the Fence', () => {
  it('the landing page frames three different problems and links into the tool', async () => {
    renderAt('/yards-fences');
    expect(await screen.findByRole('heading', { level: 1, name: 'Draw the Fence' }, { timeout: 20_000 })).toBeTruthy();
    for (const verb of ['Secure it', 'Compete in it', 'Deploy it']) expect(screen.getByRole('heading', { name: verb })).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Start an assessment' })).toBeTruthy();
    // the module tab is registered in the shared header
    expect(screen.getAllByRole('link', { name: 'Draw the Fence' }).some((a) => a.getAttribute('aria-current') === 'true')).toBe(true);
  });

  it('a system result shows every layer in its own yard and explains the focused one', async () => {
    const s = state({ sel: { tech: 'Testtech', stage: 'Midstream', stack: 'syn-stack' }, focus: 'syn-cells', layers: { 'syn-cells': { answers: { premium: 15 }, scenario: {}, strategies: ['preserve'] } } });
    renderAt(`/yards-fences/result${stateSearch(s)}`);
    const fig = await screen.findByRole('figure', { name: /Synthetic system: layers and their yards/ }, { timeout: 20_000 });
    const rows = within(fig).getAllByRole('listitem');
    expect(rows.map((r) => r.textContent)).toEqual([
      expect.stringMatching(/Synthetic control software.*Small Yard/i),
      expect.stringMatching(/Synthetic cells.*Medium Yard/i),
      expect.stringMatching(/Synthetic magnets.*Small Yard/i),
    ]);
    expect(screen.getByRole('heading', { name: 'Why did I get this answer?' })).toBeTruthy();
    expect(screen.getByText(/Not by the sector's headline Economic Opportunity score/)).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'What would change the answer?' })).toBeTruthy();
    // data and judgment are kept apart in the assumptions table
    const table = screen.getByRole('table', { name: /Every input to this result/ });
    expect(within(table).getAllByText('Data').length).toBeGreaterThan(0);
    expect(within(table).getAllByText('Assessment').length).toBeGreaterThan(0);
    expect(within(table).getAllByText('Your judgment').length).toBeGreaterThan(0);
  });

  it('the policy sandbox flags over-fencing of a control-layer vulnerability', async () => {
    const s = state({
      sel: { tech: 'Testtech', stage: 'Midstream', component: 'syn-control' },
      focus: 'syn-control',
      layers: { 'syn-control': { answers: {}, scenario: {}, strategies: [], policy: { levels: { trustedSource: 3, tariff: 3 }, scope: 'whole-product' } } },
    });
    renderAt(`/yards-fences/fence${stateSearch(s)}`);
    expect(await screen.findByText('Over-fencing risk', {}, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText(/restricts the entire hardware stack/)).toBeTruthy();
  });

  it('the scenario lab separates the observed baseline from the scenario and explains a change of yard', async () => {
    const s = state({
      sel: { tech: 'Testtech', stage: 'Midstream', component: 'syn-cells' },
      focus: 'syn-cells',
      layers: { 'syn-cells': { answers: { premium: 15 }, scenario: { premium: 50, processKnowledge: 'no', workforceSuppliers: 'partly', adjacentInnovation: 'no', homeShare: 0.01 }, strategies: [] } },
    });
    renderAt(`/yards-fences/scenario${stateSearch(s)}`);
    expect(await screen.findByText('Observed baseline', {}, { timeout: 20_000 })).toBeTruthy();
    await act(async () => undefined);
    await waitFor(() => expect(screen.getByText(/Medium Yard → Large Yard/)).toBeTruthy());
    expect(screen.getByText(/industrial-capability rationale weakens/)).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Reset to observed data' })).toBeTruthy();
  });

  it('the guided assessment writes its state to the URL and moves from choice to evidence', async () => {
    const router = renderAt('/yards-fences/assess');
    const tech = await screen.findByRole('combobox', { name: 'Technology' }, { timeout: 20_000 });
    await act(async () => {
      fireEvent.change(tech, { target: { value: 'Testtech' } });
    });
    const choice = await screen.findByRole('radio', { name: /Synthetic cells/ });
    await act(async () => {
      fireEvent.click(choice);
    });
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Next' }));
    });
    expect(await screen.findByRole('heading', { name: 'What the data tell us' })).toBeTruthy();
    expect(screen.getByText(/Measured for/)).toBeTruthy();
    const a = new URLSearchParams(router.state.location.search).get('a');
    expect(a).toBeTruthy();
    expect(decodeState(a)).toMatchObject({ sel: { tech: 'Testtech', stage: 'Midstream', component: 'syn-cells' }, step: 1 });
    // Unknown is always offered at a gate
    await act(async () => {
      fireEvent.click(screen.getByRole('button', { name: 'Next' }));
    });
    const group = await screen.findByRole('radiogroup', { name: 'Consequence of disruption' });
    expect(within(group).getByRole('radio', { name: 'Unknown' })).toBeTruthy();
  });

  it('the system badge agrees with the engine (mixed or undecided systems are never shown as one layer’s yard)', async () => {
    const s = state({ sel: { tech: 'Testtech', stage: 'Midstream', stack: 'syn-stack' }, focus: 'syn-control' });
    renderAt(`/yards-fences/result${stateSearch(s)}`);
    await screen.findByRole('figure', { name: /Synthetic system/ }, { timeout: 20_000 });
    // cells has no premium here: medium or large; the other layers are small, so no yard fits every layer
    expect(screen.getByText('The system as a whole').parentElement?.textContent).toMatch(/Mixed/);
  });

  it('the diversification, examples and methodology views render from the bundle', async () => {
    const s = state({ sel: { tech: 'Testtech', stage: 'Midstream', component: 'syn-cells' }, focus: 'syn-cells' });
    renderAt(`/yards-fences/diversify${stateSearch(s)}`);
    expect(await screen.findByRole('heading', { name: /Can trusted diversification solve the problem/ }, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText('Three strategies')).toBeTruthy();
    expect(screen.getByText(/Partnership Strength with USA 0.90/)).toBeTruthy();
    cleanup();
    resetYFCache();
    renderAt('/yards-fences/methodology');
    expect(await screen.findByText('Thresholds and why they exist', {}, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getAllByText(/OSI metrics describe observed economic/).length).toBeGreaterThan(0);
  });

  it('shows the module’s "No data" screen when the bundle is missing', async () => {
    const f = globalThis.fetch;
    globalThis.fetch = vi.fn(async () => new Response('not found', { status: 404 })) as unknown as typeof fetch;
    try {
      renderAt('/yards-fences');
      expect(await screen.findByText(/No Yards & Fences data/, {}, { timeout: 20_000 })).toBeTruthy();
      expect(screen.getByText(/89_export_yards_fences_data\.R/)).toBeTruthy();
    } finally {
      globalThis.fetch = f;
    }
  });
});
