// The CM-FDI views on builder A's R synthetic bundle (14 projects; skipped when absent): the profile's
// Investment subsection in a covered Midstream cell (loads sector-fdi; captions with the unit, the
// announcement years and the source; the CC BY 4.0 attribution on every card; "not disclosed", never
// 0; source links that open safely; the basis switch), no sector-fdi load in an uncovered cell, the
// Explore investment layout (the measure picks the lead card; no year select), Country in sector's
// inbound and outbound projects (the EU node counts its members together), and the Sources download.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../../routes.tsx';
import { haveSynthetic, syntheticSource } from '../../__fixtures__/__tests__/synthetic.ts';
import { SectorStore, setSectorStoreForTests } from '../../data/store.ts';

const restore: (() => void)[] = [];
let source: ReturnType<typeof syntheticSource>;
let errors: string[] = [];

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q),
    media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  Element.prototype.scrollIntoView = function () {} as typeof Element.prototype.scrollIntoView;
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
});

afterAll(() => {
  for (const r of restore.reverse()) r();
});

beforeEach(() => {
  source = syntheticSource();
  setSectorStoreForTests(new SectorStore(source));
  errors = [];
  vi.spyOn(console, 'error').mockImplementation((...a: unknown[]) => {
    errors.push(a.map(String).join(' '));
  });
});

afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
  vi.restoreAllMocks();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const fdiLoaded = () => source.calls.some((f) => f.startsWith('sector-fdi'));
const realErrors = () => errors.filter((e) => !/Could not parse CSS stylesheet/.test(e));

describe.skipIf(!haveSynthetic())('CM-FDI views on the R synthetic bundle', () => {
  it('profile: the Investment subsection of Batteries Midstream with captions, attribution, value states and source links', async () => {
    renderAt('/sector/batteries/midstream');
    expect(await screen.findByRole('heading', { level: 3, name: 'Cross-border manufacturing projects (CM-FDI)' }, { timeout: 30_000 })).toBeTruthy();
    const block = await screen.findByTestId('fdi-block', {}, { timeout: 30_000 });
    expect(fdiLoaded()).toBe(true);
    for (const t of ['Where the projects go', 'From headquarters to destination', 'Who invests: the parents’ headquarters', 'Whose parents: projects by the parent’s headquarters group', 'Announcements by year', 'The projects']) {
      expect(within(block).getByRole('heading', { level: 3, name: t })).toBeTruthy();
    }
    // the caption: measure, unit, announcement years, source and edition; the basis
    expect(within(block).getAllByText(/Cross-border clean-tech manufacturing projects · number of projects · 2016-2025 · CM-FDI \(Net Zero Industrial Policy Lab\) 2026\.10\.05/).length).toBeGreaterThan(0);
    expect(within(block).getAllByText(/all 5 projects \(every status and activity\)/).length).toBeGreaterThan(0);
    // the group split states its own destination count; no build-wide "n economies" that ignores the basis
    expect(within(block).getAllByText(/· \d+ destinations? · share of the projects/).length).toBe(1);
    expect(within(block).queryAllByText(/\d+ economies/)).toHaveLength(0);
    // CC BY 4.0 attribution on every card, the licence link opens safely
    const attributions = within(block).getAllByTestId('cmfdi-attribution');
    expect(attributions.length).toBeGreaterThanOrEqual(6);
    for (const a of attributions) expect(a.textContent).toMatch(/P\. Bandara and the Net Zero Industrial Policy Lab.*CC BY 4\.0/);
    const cc = within(attributions[0]).getByRole('link', { name: /creativecommons\.org\/licenses\/by\/4\.0/ });
    expect(cc.getAttribute('rel')).toBe('noopener noreferrer');
    expect(cc.getAttribute('target')).toBe('_blank');
    // an undisclosed value reads "not disclosed" in the table, never 0
    const table = within(block).getByRole('table', { name: /Cross-border projects, Batteries — Midstream/ });
    const rosatom = within(table).getByText('Rosatom').closest('tr') as HTMLElement;
    expect(within(rosatom).getByText('not disclosed')).toBeTruthy();
    expect(within(rosatom).queryByText(/^0$/)).toBeNull();
    // source links per field, http(s) only, opening safely
    const catl = within(table).getByText('CATL').closest('tr') as HTMLElement;
    const links = within(catl).getAllByRole('link');
    expect(links.length).toBeGreaterThan(0);
    for (const l of links) {
      expect(l.getAttribute('href')).toMatch(/^https:\/\/example\.org\//);
      expect(l.getAttribute('rel')).toBe('noopener noreferrer');
    }
    // the basis switch: 5 projects in all, 4 used by the indices (the closed Hong Kong plant is out)
    const tiles = within(block).getByRole('list', { name: 'Key figures of the cross-border projects' });
    expect(within(tiles).getAllByRole('listitem')[0].textContent).toMatch(/Projects5/);
    fireEvent.click(within(block).getByRole('radio', { name: /Used by the indices \(4\)/ }));
    await waitFor(() => expect(within(tiles).getAllByRole('listitem')[0].textContent).toMatch(/Projects4/));
    expect(within(block).queryByText('HK Battery Co')).toBeNull();
    expect(realErrors()).toEqual([]);
  });

  it('profile: a cell without CM-FDI data never loads sector-fdi', async () => {
    renderAt('/sector/solar/downstream');
    expect(await screen.findByRole('heading', { level: 1, name: /Solar — Downstream/ }, { timeout: 30_000 })).toBeTruthy();
    await new Promise((r) => setTimeout(r, 100));
    expect(screen.queryByText('Cross-border manufacturing projects (CM-FDI)')).toBeNull();
    expect(fdiLoaded()).toBe(false);
  });

  it('Explore: the investment layout leads with the measure (parent groups first), with no year select', async () => {
    renderAt('/sector/explore/investment?m=investment.fdi_parent_share&tech=Wind&stage=Midstream');
    const block = await screen.findByTestId('fdi-block', {}, { timeout: 30_000 });
    expect(screen.getByText(/Project-level data: .*Not a country total/)).toBeTruthy();
    expect(screen.queryByRole('combobox', { name: 'Year' })).toBeNull();
    const titles = within(block).getAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    expect(titles.indexOf('Whose parents: projects by the parent’s headquarters group')).toBeLessThan(titles.indexOf('Where the projects go'));
    expect(screen.getByRole('heading', { name: /Notes on foreign-invested projects by parent's headquarters group/ })).toBeTruthy();
    expect(screen.getByText(/see Methods for how it enters/)).toBeTruthy();
    expect(realErrors()).toEqual([]);
  });

  it('Explore: disclosed capital leads with the value view', async () => {
    renderAt('/sector/explore/investment?m=investment.fdi_value&tech=Solar&stage=Midstream');
    const block = await screen.findByTestId('fdi-block', {}, { timeout: 30_000 });
    const radio = within(block).getByRole('radio', { name: 'Disclosed capital (US$ m)' }) as HTMLInputElement;
    expect(radio.checked).toBe(true);
    expect(within(block).getAllByText(/Announced capital investment of cross-border projects · US dollars, millions/).length).toBeGreaterThan(0);
  });

  it('Country in sector: inbound and outbound projects of an economy and of the EU node', async () => {
    renderAt('/sector/batteries/midstream/DEU');
    const sec = await screen.findByTestId('fdi-country', {}, { timeout: 30_000 });
    const tiles = within(sec).getAllByRole('listitem');
    expect(tiles[0].textContent).toMatch(/Inbound projects2/);
    expect(tiles[3].textContent).toMatch(/Outbound projects0/);
    expect(within(sec).getByRole('heading', { level: 3, name: /^Who invests in / })).toBeTruthy();
    const table = within(sec).getByRole('table', { name: /Cross-border projects/ });
    expect(within(table).getAllByText('Inbound').length).toBe(2);
    expect(within(table).getByText('Rosatom')).toBeTruthy();
    cleanup();
    renderAt('/sector/batteries/midstream/EUU');
    const eu = await screen.findByTestId('fdi-country', {}, { timeout: 30_000 });
    expect(within(eu).getAllByRole('listitem')[0].textContent).toMatch(/Inbound projects3/);
    expect(within(eu).getByText(/projects between two EU members are left out/)).toBeTruthy();
    expect(realErrors()).toEqual([]);
  });

  it('Sources: the CM-FDI projects download with its licence', async () => {
    renderAt('/sector/sources');
    expect(await screen.findByRole('heading', { name: 'Cross-border projects, CM-FDI (.csv)' }, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Download the projects' })).toBeTruthy();
    expect(screen.getAllByText(/Licensed CC BY 4\.0: cite as Bandara, P\. and the Net Zero Industrial Policy Lab/).length).toBe(1);
  });
});
