// The three headline charts on the fixture bundle: the Country posture matrix (marks, borders, Watch in
// the tooltip only, the drawer with partners, the Secure rationale and restraint, trade freshness), the
// Sector scatter (quadrant labels, Secure at true coordinates, click opens the profile) and the AIB
// division matrix (holders, gaps, the three header metrics).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { AibDivisionHero } from '../AibDivisionHero.tsx';
import { CountryPostureHero } from '../CountryPostureHero.tsx';
import { SectorPostureHero } from '../SectorPostureHero.tsx';
import { setPostureStore } from '../store.ts';
import { fixtureStore } from './fixture.ts';

function at(path: string, element: React.ReactNode) {
  const router = createMemoryRouter(
    [
      { path: '/sector', element },
      { path: '/countries/:iso3', element: <p>profile page</p> },
      { path: '*', element },
    ],
    { initialEntries: [path] },
  );
  render(<RouterProvider router={router} />);
  return router;
}

// jsdom's AbortSignal is not undici's: the router's navigation Request drops the signal (as the AIB view tests do).
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
beforeEach(() => setPostureStore(fixtureStore()));
afterEach(() => {
  cleanup();
  setPostureStore(null);
});

describe('the Country posture matrix', () => {
  it('draws every technology x six-stage node with its posture, mark and alignment', async () => {
    at('/', <CountryPostureHero iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    const cells = within(m).getAllByRole('button');
    expect(cells).toHaveLength(12);
    expect(within(m).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Technology', 'ExtExtraction', 'RefRefining', 'ComComponents', 'AsmAssembly', 'DepDeployment', 'RecRecovery']);
    const sec = within(m).getByRole('button', { name: /^Batteries Refining: Secure; Coercive chokepoint/ });
    expect(sec.textContent).toContain('K');
    expect(sec.getAttribute('aria-label')).toContain('policy under-prioritized');
    const part = within(m).getByRole('button', { name: /^Batteries Components: Compete · Partner/ });
    expect(part.getAttribute('data-mode')).toBe('partner');
    const pres = within(m).getByRole('button', { name: /^Solar Assembly: Compete · Preserve/ });
    expect(pres.textContent).toContain('P');
    expect(Number((pres as HTMLElement).style.opacity)).toBeLessThan(1); // weak evidence fades
    // Watch: named in the accessible name and tooltip, never as a mark
    const w = within(m).getByRole('button', { name: /^Solar Extraction: Deploy; Watch/ });
    expect(w.textContent?.trim()).toBe('');
    fireEvent.mouseEnter(w);
    expect(screen.getByRole('tooltip').textContent).toContain('Watch');
  });
  it('states the takeaway, counts and trade freshness above the matrix', async () => {
    at('/', <CountryPostureHero iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^Aland should secure 1 node/);
    expect(screen.getByTestId('trade-freshness').textContent).toBe('Structural trade benchmark: 2022–24 · Current signal through: Aug 2026');
    const counts = screen.getByLabelText('Posture counts');
    expect(counts.textContent).toMatch(/Secure1/);
    expect(counts.textContent).toMatch(/Compete2/);
  });
  it('opens the drawer: partners for a Partner node, the rationale for Secure, restraint for Deploy', async () => {
    at('/', <CountryPostureHero iso3="AAA" name="Aland" />);
    const m = await screen.findByTestId('posture-matrix');
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Components/ }));
    const d = await screen.findByTestId('cell-drawer');
    await waitFor(() => expect(d.textContent).toContain('Trusted partners'));
    expect(d.textContent).toContain('Bland');
    expect(d.textContent).toContain('partner despite capability');
    expect(d.textContent).toMatch(/Data through Aug 2026/);
    fireEvent.click(within(m).getByRole('button', { name: /^Batteries Refining/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Coercive chokepoint'));
    expect(screen.getByTestId('cell-drawer').textContent).toContain('Secure does not mean reshoring');
    expect(screen.getByTestId('cell-drawer').textContent).toContain('Current momentum unavailable');
    fireEvent.click(within(m).getByRole('button', { name: /^Solar Extraction/ }));
    await waitFor(() => expect(screen.getByTestId('cell-drawer').textContent).toContain('Why restraint'));
    fireEvent.keyDown(window, { key: 'Escape' });
    await waitFor(() => expect(screen.queryByTestId('cell-drawer')).toBeNull());
  });
  it('without a momentum window it says why there is no current signal', async () => {
    setPostureStore(fixtureStore({ momentum: false }));
    at('/', <CountryPostureHero iso3="AAA" name="Aland" />);
    expect((await screen.findByTestId('trade-freshness')).textContent).toContain('Current momentum unavailable — incomplete matched-period reporting');
  });
});

describe('the Sector scatter', () => {
  it('places countries by opportunity and need, keeps Secure at its true coordinates, and opens the profile on click', async () => {
    const router = at('/sector?pnode=Batteries|Refining', <SectorPostureHero />);
    const sc = await screen.findByTestId('posture-scatter');
    for (const q of ['CATCH UP', 'PRESERVE', 'PARTNER', 'WATCH · DEPLOY']) expect(sc.textContent).toContain(q);
    const secure = sc.querySelector('path[data-posture="S"]') as SVGPathElement;
    expect(secure).not.toBeNull();
    // need 0.95, opportunity 0.40 -> in the partner quadrant, drawn as an outlined diamond
    expect(secure.getAttribute('fill')).toMatch(/rgb/);
    expect(secure.getAttribute('aria-label')).toMatch(/Aland: Secure, need 95%, opportunity 40%/);
    fireEvent.click(secure);
    await waitFor(() => expect(router.state.location.pathname).toBe('/countries/AAA'));
  });
  it('the node selector writes the URL and the takeaway follows it', async () => {
    const router = at('/sector?pnode=Batteries|Components', <SectorPostureHero />);
    expect((await screen.findByTestId('posture-takeaway')).textContent).toMatch(/^In Batteries components/);
    fireEvent.change(screen.getByLabelText('Technology'), { target: { value: 'Solar' } });
    await waitFor(() => expect(router.state.location.search).toContain('pnode=Solar%7CComponents'));
  });
});

describe('the AIB division matrix', () => {
  it('names holders and gaps and shows the three header metrics', async () => {
    at('/', <AibDivisionHero />);
    const m = await screen.findByTestId('aib-matrix');
    const node = within(m).getByRole('button', { name: /^Batteries Components: primary Bland 20%/ });
    expect(node.getAttribute('data-gap')).toBe('single_point_of_failure');
    expect(within(m).getByRole('button', { name: /^Solar Assembly: no allied holder/ }).getAttribute('data-gap')).toBe('no_trusted_supplier');
    const hero = screen.getByTestId('aib-division-hero');
    expect(hero.textContent).toContain('Core coalition members');
    expect(hero.textContent).toContain('33%');
    expect(hero.textContent).toContain('Unresolved gaps');
    fireEvent.click(node);
    expect(screen.getByTestId('aib-node-detail').textContent).toMatch(/Single point of failure .*also insufficient redundancy/);
  });
});
