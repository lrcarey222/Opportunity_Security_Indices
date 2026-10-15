// The 2026-10-11/12 Sector edits on the TS fixture (spec_country.md C1.5, C3.6, C5.1-C5.8): Upstream trade
// baskets (Solar and Semiconductors Upstream), the export-feasibility card, the HHI explainer, trade by
// sub-component (profile and Explore; the `sub` key), the three-colour legends, unique landmark names,
// the bridge link to Country Insights, and no bare acronym on any Sector route (C3.4).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { buildSectorFixture, fixtureSource } from '../__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';

const restore: (() => void)[] = [];
let source: ReturnType<typeof fixtureSource>;

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
  source = fixtureSource(buildSectorFixture());
  setSectorStoreForTests(new SectorStore(source));
  try {
    localStorage.clear();
  } catch {
    // no storage: the explainer stays open
  }
});

afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const main = () => document.querySelector('main') ?? document.body;

describe('Sector edits 2026-10 (C5)', () => {
  it('Batteries — Midstream: the HHI explainer, sub-components, the basket composition and export feasibility', async () => {
    const r = renderAt('/sector/batteries/midstream');
    expect(await h1('Batteries — Midstream')).toBeTruthy();
    const explainer = await screen.findByTestId('hhi-explainer', {}, { timeout: 20_000 });
    expect(explainer.textContent).toMatch(/Ten equal exporters give 0\.10; four give 0\.25/);
    expect(explainer.textContent).toMatch(/Here the index is \d\.\d\d, which behaves like \d+\.\d equal exporters/);
    expect(explainer.textContent).toMatch(/merger rules describe a market below 0.10 as "concentration concerns unlikely"/);
    expect(explainer.hasAttribute('open')).toBe(true);
    expect(await screen.findByRole('heading', { name: 'What the basket is made of' }, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'How close is each economy to exporting these goods?' })).toBeTruthy();
    // whole basket first; "Sub-component" picks the largest group and re-scopes the trade cards
    expect(screen.getByRole('heading', { name: 'Exporters and importers' })).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Sub-component' }));
    await waitFor(() => expect(r.state.location.search).toMatch(/sub=Cells%20and%20packs/));
    expect(await screen.findByRole('heading', { name: 'Exporters and importers (Cells and packs)' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByText(/Bilateral flows are shown for the whole basket/)).toBeTruthy();
    expect(screen.getByRole('heading', { name: /Rank the countries: exports \(Cells and packs\)/ })).toBeTruthy();
    fireEvent.change(screen.getByRole('combobox', { name: 'Sub-component' }), { target: { value: 'Materials' } });
    await waitFor(() => expect(r.state.location.search).toMatch(/sub=Materials/));
    expect(await screen.findByRole('heading', { name: 'Exporters and importers (Materials)' }, { timeout: 10_000 })).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Whole basket' }));
    await waitFor(() => expect(r.state.location.search).not.toMatch(/sub=/));
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('an unknown sub-component in the link shows the whole basket; the explainer is collapsed after the first visit', async () => {
    renderAt('/sector/batteries/midstream?sub=Nope');
    expect(await h1('Batteries — Midstream')).toBeTruthy();
    expect(await screen.findByRole('heading', { name: 'Exporters and importers' }, { timeout: 20_000 })).toBeTruthy();
    expect((screen.getByRole('radio', { name: 'Whole basket' }) as HTMLInputElement).checked).toBe(true);
    cleanup();
    renderAt('/sector/batteries/midstream');
    expect(await h1('Batteries — Midstream')).toBeTruthy();
    const explainer = await screen.findByTestId('hhi-explainer', {}, { timeout: 20_000 });
    expect(explainer.hasAttribute('open')).toBe(false);
  });

  it('Solar — Upstream: a trade basket with one group of codes and its basket note (C5.1)', async () => {
    renderAt('/sector/solar/upstream');
    expect(await h1('Solar — Upstream')).toBeTruthy();
    expect(await screen.findByText('One group of codes: the whole basket.', {}, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText(/Solar Upstream trade is silicon metal only/)).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Exporters and importers' })).toBeTruthy();
    expect(screen.queryByRole('radio', { name: 'Sub-component' })).toBeNull();
    expect(screen.getByRole('heading', { name: 'How close is each economy to exporting these goods?' })).toBeTruthy();
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('Semiconductors — Upstream renders with its trade basket', async () => {
    renderAt('/sector/semiconductors/upstream');
    expect(await h1('Semiconductors — Upstream')).toBeTruthy();
    expect(await screen.findByRole('heading', { name: 'Exporters and importers' }, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText(/gallium, germanium and indium/i, { selector: 'li' })).toBeTruthy();
  });

  it('Explore trade: three-colour map legend, and the sub-component view loads the trade chunk on demand', async () => {
    const r = renderAt('/sector/explore/trade?tech=Batteries&stage=Midstream');
    expect(await h1('Trade')).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 2, name: /^Results: Exports of the sector’s traded goods/ }, { timeout: 20_000 })).toBeTruthy();
    const legend = (await screen.findAllByTestId('tri-legend'))[0];
    expect(legend.textContent).toMatch(/White: median of the \d+ economies shown/);
    expect(source.calls.some((c) => c.startsWith('sector-trade-batteries'))).toBe(false);
    fireEvent.click(screen.getByRole('radio', { name: 'Sub-component' }));
    await waitFor(() => expect(source.calls.some((c) => c.startsWith('sector-trade-batteries'))).toBe(true));
    await waitFor(() => expect(r.state.location.search).toMatch(/sub=Cells%20and%20packs/), { timeout: 10_000 });
    expect(await screen.findByRole('heading', { name: /\(Cells and packs\), 2024$/ }, { timeout: 10_000 })).toBeTruthy();
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('Explore: the measure select never offers the sub-component vars; a link naming one opens its whole-basket measure (C5.4)', async () => {
    renderAt('/sector/explore/trade?m=trade.sub_imports&tech=Batteries&stage=Midstream');
    expect(await screen.findByRole('heading', { level: 2, name: /^Results: Imports of the sector’s traded goods/ }, { timeout: 20_000 })).toBeTruthy();
    const measure = screen.getByRole('combobox', { name: 'Measure' }) as HTMLSelectElement;
    const ids = [...measure.options].map((o) => o.value);
    expect(ids).toContain('trade.imports');
    expect(ids.some((x) => x.startsWith('trade.sub_'))).toBe(false);
  });

  it('Explore net balance: production assumed 0 is drawn as a value with the dagger and the note (C5.5)', async () => {
    renderAt('/sector/explore/consumption?m=consumption.balance&item=gas&c=VNM');
    expect(await screen.findByRole('heading', { level: 2, name: /^Results: Net balance/ }, { timeout: 20_000 })).toBeTruthy();
    await waitFor(() => expect(main().textContent).toMatch(/Viet Nam \(assumed 0\)/), { timeout: 20_000 });
    expect(main().textContent).toMatch(/−0\.4 EJ †/);
    expect(main().textContent).toMatch(/† Production not reported separately; assumed 0/);
    expect(main().textContent).not.toMatch(/production as…/);
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('Concentration: the general explainer at the top, lens labels spelled out', async () => {
    renderAt('/sector/concentration');
    expect(await h1('Concentration across sectors')).toBeTruthy();
    const explainer = await screen.findByTestId('hhi-explainer', {}, { timeout: 20_000 });
    expect(explainer.textContent).toMatch(/A sector with an index of 0\.25 behaves like four equal holders/);
    expect(screen.getByRole('radio', { name: 'Concentration (HHI)' })).toBeTruthy();
    expect(screen.getByRole('radio', { name: 'Largest holder' })).toBeTruthy();
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('Country in sector links to Country Insights; the SectorBar highlight does too', async () => {
    renderAt('/sector/batteries/midstream/USA');
    expect(await h1('United States in Batteries — Midstream')).toBeTruthy();
    const links = screen.getAllByRole('link', { name: 'United States in Country Insights →' });
    expect(links.length).toBeGreaterThan(0);
    for (const a of links) expect(a.getAttribute('href')).toBe('/countries/USA');
  });

  it('Sources: unique card names inside the sections, the glossary link, the CM-FDI line in the ZIP description', async () => {
    renderAt('/sector/sources');
    expect(await h1('Sources & data')).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Variable dictionary' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Every measure: unit, years, source and caveats' })).toBeTruthy();
    expect(screen.getByRole('link', { name: /Glossary: every term/ }).getAttribute('href')).toBe('/glossary');
    // no two landmarks share a name
    const names = [...document.querySelectorAll('section[aria-labelledby]')].map((s) => document.getElementById(s.getAttribute('aria-labelledby') ?? '')?.textContent?.trim() ?? '');
    expect(new Set(names).size).toBe(names.length);
    expect(bareAcronyms(main())).toEqual([]);
  });

  it.each(['/sector/batteries/midstream', '/sector/oil/upstream', '/sector/explore/trade?tech=Batteries&stage=Midstream', '/sector/explore/prices'])('every chart card on %s says how to read it (C3.6)', async (path) => {
    renderAt(path);
    await screen.findByRole('heading', { level: 1 }, { timeout: 30_000 });
    await waitFor(() => expect(document.querySelectorAll('main section.card > figure').length).toBeGreaterThan(1), { timeout: 20_000 });
    await waitFor(() => expect(main().textContent ?? '').not.toMatch(/Loading /), { timeout: 20_000 });
    // a chart card's reading line is the HowToRead span under its measure caption
    const without = [...main().querySelectorAll('section.card')]
      .filter((card) => card.querySelector(':scope > figure') && !card.querySelector('span.mt-1.block.text-paper-dim'))
      .map((card) => card.querySelector('h2, h3, h4')?.textContent?.trim());
    expect(without).toEqual([]);
  });

  it.each(['/sector', '/sector/batteries', '/sector/minerals', '/sector/explore/policy', '/sector/explore/technology', '/sector/oil/upstream'])('no bare acronym on %s (C3.4)', async (path) => {
    renderAt(path);
    await screen.findByRole('heading', { level: 1 }, { timeout: 30_000 });
    await waitFor(() => expect(document.querySelectorAll('figure, [data-testid="sector-bar"]').length).toBeGreaterThan(0), { timeout: 20_000 });
    expect(bareAcronyms(main())).toEqual([]);
  });
});
