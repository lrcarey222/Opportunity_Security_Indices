// Development & finance on the R synthetic bundle (skipped when absent): the WDI series as a map and
// ranking with its own unit, the aid flows layout (who funds whom; the aid-sector select; one year or
// the total), the investment positions layout (conduit centres flagged and the switch that leaves them
// out), the economy page's aid and investment partners for the stage, and the four sources listed.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
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

const devLoaded = () => source.calls.some((f) => f.startsWith('sector-dev'));
const realErrors = () => errors.filter((e) => !/Could not parse CSS stylesheet/.test(e));

describe.skipIf(!haveSynthetic())('Development & finance on the R synthetic bundle', () => {
  it('Explore: a World Bank series on the map and ranking, without loading sector-dev', async () => {
    renderAt('/sector/explore/development?m=development.wdi&item=ny-gdp-pcap-kd');
    expect(await screen.findByRole('combobox', { name: 'Series' }, { timeout: 30_000 })).toBeTruthy();
    expect((await screen.findAllByText(/GDP per capita \(constant 2015 US\$\)/, {}, { timeout: 30_000 })).length).toBeGreaterThan(0);
    // a mean of the years reported is labelled as such, never as one year
    expect((await screen.findAllByRole('heading', { name: /GDP per capita \(constant 2015 US\$\), 2022–2023 average$/ }, { timeout: 30_000 })).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/average of the years reported, 2022–2023/).length).toBeGreaterThan(0);
    expect(screen.queryByText(/latest per economy/)).toBeNull();
    expect(devLoaded()).toBe(false);
    expect(realErrors()).toEqual([]);
  });

  it('Explore: who funds whom, by aid sector, one year or the total', async () => {
    renderAt('/sector/explore/development?m=development.oda&item=energy&y=2024');
    expect(await screen.findByRole('heading', { name: /^Who funds whom: aid for energy, 2024/ }, { timeout: 30_000 })).toBeTruthy();
    expect(devLoaded()).toBe(true);
    expect(screen.getByText(/Bilateral flows: official development assistance/)).toBeTruthy();
    expect(screen.getByRole('heading', { name: /^Largest donors, 2024/ })).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: '2022-2024 total' }));
    expect(await screen.findByRole('heading', { name: /^Who funds whom: aid for energy, 2022-2024/ })).toBeTruthy();
    expect(realErrors()).toEqual([]);
  });

  it('Explore: who invests where, conduit centres flagged and optionally left out', async () => {
    renderAt('/sector/explore/development?m=development.fdi_out');
    expect(await screen.findByRole('heading', { name: /^Who invests where: direct-investment positions/ }, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/2 positions involve a conduit financial centre/)).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Leave out conduit centres' }));
    expect(screen.queryByText(/^\d+ positions involve a conduit financial centre \(pass-through/)).toBeNull();
    expect(realErrors()).toEqual([]);
  });

  it('Country in sector: aid for the stage and investment partners', async () => {
    renderAt('/sector/solar/deployment/IDN');
    const sec = (await screen.findByRole('heading', { level: 2, name: 'Aid and investment partners' }, { timeout: 30_000 })).closest('section') as HTMLElement;
    expect(await within(sec).findByRole('heading', { level: 3, name: /^Aid for energy to Indonesia, 2022-2024/ }, { timeout: 30_000 })).toBeTruthy();
    expect(within(sec).getByRole('heading', { level: 3, name: 'Who invests in Indonesia' })).toBeTruthy();
    expect(realErrors()).toEqual([]);
  });

  it('Sources: the four development sources with their licences', async () => {
    renderAt('/sector/sources');
    const table = await screen.findByRole('table', { name: /^Sources/ }, { timeout: 30_000 });
    const search = within(table.closest('section') as HTMLElement).getByPlaceholderText('Search');
    for (const s of ['World Bank WDI', 'World Bank Doing Business', 'OECD Creditor Reporting System', 'IMF Direct Investment Positions']) {
      fireEvent.change(search, { target: { value: s } });
      expect((await screen.findAllByText(s)).length).toBeGreaterThan(0);
    }
  });
});
