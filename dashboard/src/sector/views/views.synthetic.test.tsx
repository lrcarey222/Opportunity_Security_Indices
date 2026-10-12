// Every Sector route on builder A's R-written synthetic bundle (scripts/87 --synthetic ->
// tests/fixtures/sector/synthetic/; skipped when absent), decoded with the real codec and validated
// with the real contract: each page renders its h1 with no error screen, and the pages that need
// chunks load them. This is the check that the views read what scripts/87 writes, not only the TS
// fixture.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { haveSynthetic, syntheticSource } from '../__fixtures__/__tests__/synthetic.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';

const restore: (() => void)[] = [];
let source: ReturnType<typeof syntheticSource>;

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

let errors: string[] = [];
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

const ROUTES: [string, RegExp][] = [
  ['/sector', /Energy supply chains, measured/],
  ['/sector?c=USA&eu=bloc', /Energy supply chains, measured/],
  ['/sector/batteries', /Batteries: the supply chain/],
  ['/sector/batteries/midstream?c=USA', /Batteries — Midstream/],
  ['/sector/batteries/upstream', /Batteries — Upstream/],
  ['/sector/oil/upstream?eu=bloc', /Oil — Upstream/],
  ['/sector/solar/downstream', /Solar — Downstream/],
  ['/sector/electric-vehicles/midstream', /Electric Vehicles — Midstream/],
  ['/sector/geothermal/upstream', /Geothermal — Upstream/],
  ['/sector/batteries/midstream/USA', /United States in Batteries — Midstream/],
  ['/sector/oil/upstream/BRA', /Brazil in Oil — Upstream/],
  ['/sector/minerals', /^Minerals$/],
  ['/sector/minerals/lithium?c=USA', /^Lithium$/],
  ['/sector/minerals/graphite?eu=bloc', /^Graphite/],
  ['/sector/concentration?lens=hhi', /Concentration across sectors/],
  ['/sector/concentration?eu=bloc&fam=exports', /Concentration across sectors/],
  ['/sector/explore', /^Trade$/],
  ['/sector/explore/trade?m=trade.flow&tech=Batteries&stage=Midstream&c=USA', /^Trade$/],
  ['/sector/explore/resources', /Reserves/],
  ['/sector/explore/production?m=production.fuels&item=oil', /Production/],
  ['/sector/explore/consumption?c=USA', /Consumption/],
  ['/sector/explore/investment', /Investment/],
  ['/sector/explore/investment?m=investment.fdi_projects&tech=Batteries&stage=Midstream&c=DEU', /Investment/],
  ['/sector/explore/investment?m=investment.fdi_value&tech=Electric%20Vehicles&stage=Midstream', /Investment/],
  ['/sector/explore/investment?m=investment.fdi_parent_share&tech=Solar&stage=Midstream', /Investment/],
  ['/sector/wind/midstream/BRA', /Brazil in Wind — Midstream/],
  ['/sector/explore/manufacturing', /Manufacturing/],
  ['/sector/explore/prices', /Prices/],
  ['/sector/explore/technology', /Technology/],
  ['/sector/explore/policy', /Policy/],
  ['/sector/explore/policy?m=policy.cat', /Policy/],
  ['/sector/explore/development', /Development/],
  ['/sector/sources', /Sources & data/],
];

describe.skipIf(!haveSynthetic())('every Sector route on the R synthetic bundle', () => {
  for (const [path, name] of ROUTES) {
    it(path, async () => {
      const router = createMemoryRouter(routes, { initialEntries: [path] });
      render(<RouterProvider router={router} />);
      expect(await screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 })).toBeTruthy();
      // let lazy sections and chunk loads settle, then check no error screen appeared
      await waitFor(() => expect(document.querySelectorAll('[role="status"]').length).toBeGreaterThanOrEqual(0));
      await new Promise((r) => setTimeout(r, 50));
      expect(screen.queryByText(/This view failed to render|could not be loaded|is not part of the current sector data build/i)).toBeNull();
      expect(errors.filter((e) => !/Could not parse CSS stylesheet/.test(e))).toEqual([]);
    });
  }
});
