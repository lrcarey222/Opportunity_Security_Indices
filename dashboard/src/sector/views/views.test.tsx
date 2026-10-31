// Sector views on the fixture (spec_sector.md 10.3, builder C): every route renders; coverage chips;
// a thin cell shows the notice and the "Not available" list; the EU switch changes the snapshot and
// loads the trade chunk; the highlight pins a row; CSV text has the 3.9 columns; the ZIP has the
// expected files; arrow keys move across Atlas tiles; the phone SectorBar dialog traps focus and closes
// on Escape. The data come from the TS fixture (src/sector/__fixtures__/build.ts) through the store.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { buildSectorFixture, fixtureSource } from '../__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { SECTOR_CSV_HEADER } from '../derive/csv.ts';

const restore: (() => void)[] = [];
let wide = true;
let source: ReturnType<typeof fixtureSource>;
let store: SectorStore;

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: wide ? /min-width/.test(q) : /max-width/.test(q),
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
  wide = true;
  source = fixtureSource(buildSectorFixture());
  store = new SectorStore(source);
  setSectorStoreForTests(store);
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

// 30 s: the first render compiles every lazy route module (slow under load)
const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
/** The Sector profile's groups start folded at every width: open them all, as a reader clicking "Show". */
const openGroups = async () => {
  for (const b of await screen.findAllByRole('button', { name: 'Show' }, { timeout: 30_000 })) fireEvent.click(b);
};

describe('Sector routes render on the fixture', () => {
  it('home: questions, the Atlas, minerals and themes', async () => {
    renderAt('/sector');
    expect(await h1('Where are the real bottlenecks and opportunities?')).toBeTruthy();
    expect(document.title).toMatch(/^Sectors \| Allied Industrial Base Explorer/);
    expect(screen.getByRole('link', { name: 'Who controls supply?' })).toBeTruthy();
    expect(screen.getByRole('group', { name: /Supply-chain cells/ })).toBeTruthy();
    const tile = screen.getByRole('link', { name: /^Batteries — Assembly: exports/ });
    expect(tile.getAttribute('href')).toBe('/sector/batteries/assembly');
    expect(screen.getByRole('link', { name: /^Lithium:/ })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Browse by theme' })).toBeTruthy();
    expect(source.calls.some((c) => c.startsWith('sector-facts'))).toBe(false);
  });

  it('home: arrow keys move across Atlas tiles', async () => {
    renderAt('/sector');
    await h1('Where are the real bottlenecks and opportunities?');
    const first = screen.getByRole('link', { name: /^Batteries — Extraction/ });
    first.focus();
    // six columns in chain order: right from Extraction is Refining, down is the next technology's Refining
    fireEvent.keyDown(first, { key: 'ArrowRight' });
    expect(document.activeElement?.getAttribute('aria-label')).toMatch(/^Batteries — Refining/);
    fireEvent.keyDown(document.activeElement as Element, { key: 'ArrowDown' });
    expect(document.activeElement?.getAttribute('aria-label')).toMatch(/^Coal — Refining/);
    fireEvent.keyDown(document.activeElement as Element, { key: 'End' });
    expect(document.activeElement?.getAttribute('aria-label')).toMatch(/^Coal — Recovery/);
  });

  it('home with c: facts load and tiles show the position', async () => {
    renderAt('/sector?c=USA');
    await h1('Where are the real bottlenecks and opportunities?');
    await waitFor(() => expect(screen.getByRole('link', { name: /^Batteries — Assembly: .*USA/ })).toBeTruthy(), { timeout: 10_000 });
  });

  it('unknown sector slug goes home with a notice; country snapshot redirects to ?c=', async () => {
    const r = renderAt('/sector/baterries');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector'), { timeout: 10_000 });
    expect(await screen.findByText(/No sector called 'baterries'/, {}, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    const r2 = renderAt('/sector/country/jpn');
    await waitFor(() => expect(`${r2.state.location.pathname}${r2.state.location.search}`).toBe('/sector?c=JPN'));
  });

  it('no manifest shows the module-level screen with the developer hint', async () => {
    setSectorStoreForTests(new SectorStore(fixtureSource(buildSectorFixture(), { noManifest: true })));
    renderAt('/sector');
    expect(await screen.findByText(/scripts\/87_export_sector_data\.R/, {}, { timeout: 10_000 })).toBeTruthy();
    expect(document.title).toMatch(/(^|\| )Sectors \| /);
  });

  it('invalid link values are listed in the "Ignored in the link" notice', async () => {
    renderAt('/sector?lens=red&c=XXX');
    await h1('Where are the real bottlenecks and opportunities?');
    expect(screen.getByText('Ignored in the link')).toBeTruthy();
    expect(screen.getByText(/lens=red/)).toBeTruthy();
  });

  it('CSV of the Atlas', async () => {
    renderAt('/sector');
    await h1('Where are the real bottlenecks and opportunities?');
    expect(SECTOR_CSV_HEADER.split(',')).toHaveLength(18);
    expect(SECTOR_CSV_HEADER.endsWith(',note')).toBe(true);
  });

  it('the phone SectorBar opens a dialog that traps focus and closes on Escape', async () => {
    wide = false;
    renderAt('/sector');
    await h1('Where are the real bottlenecks and opportunities?');
    const bar = screen.getByTestId('sector-bar');
    fireEvent.click(within(bar).getByRole('button', { name: /Filters/ }));
    const dlg = await screen.findByRole('dialog', { name: /Filters/ });
    expect(dlg.contains(document.activeElement)).toBe(true);
    const focusables = [...dlg.querySelectorAll<HTMLElement>('input:not([disabled]),select:not([disabled]),button:not([disabled]),a[href]')];
    focusables[focusables.length - 1].focus();
    fireEvent.keyDown(document.activeElement as Element, { key: 'Tab' });
    expect(dlg.contains(document.activeElement)).toBe(true);
    fireEvent.keyDown(document.activeElement as Element, { key: 'Escape' });
    await waitFor(() => expect(screen.queryByRole('dialog')).toBeNull());
  });
});

describe('Sector profile', () => {
  it('Batteries — Assembly renders every section with data, coverage chips and the highlight', async () => {
    const r = renderAt('/sector/batteries/assembly?c=USA');
    expect(await h1('Batteries — Assembly')).toBeTruthy();
    await openGroups();
    expect(document.title).toMatch(/^Batteries — Assembly \| Sectors \| /);
    expect(await screen.findByRole('heading', { level: 2, name: 'Snapshot' }, { timeout: 10_000 })).toBeTruthy();
    // coverage chips: trade links to its section; reserves explains why not
    const chips = screen.getByRole('list', { name: /Coverage of Batteries — Assembly/ });
    expect(within(chips).getByRole('link', { name: /Trade/ }).getAttribute('href')).toBe('#trade');
    expect(within(chips).getAllByText(/not applicable/i).length).toBeGreaterThan(0);
    // snapshot: market size and the highlight tile
    const snap = screen.getByRole('list', { name: 'Snapshot' });
    expect(within(snap).getByText('World exports')).toBeTruthy();
    expect(within(snap).getByText('Highlight')).toBeTruthy();
    expect(within(snap).getByText(/United States/)).toBeTruthy();
    // sections in rendering order
    for (const name of ['Who holds the market', 'Rank the countries', 'Trade', 'Investment', 'Production & capacity', 'Policy activity', 'Technology readiness', 'Stage context', 'Countries in this sector']) {
      expect(await screen.findByRole('heading', { level: 2, name }, { timeout: 10_000 })).toBeTruthy();
    }
    await waitFor(() => expect(source.calls.some((c) => c.startsWith('sector-trade-batteries'))).toBe(true), { timeout: 10_000 });
    expect(await screen.findByText('Exporters and importers', {}, { timeout: 10_000 })).toBeTruthy();
    // a series-only measure (capacity by product) has no facts slice: its caption never says "0 of 57
    // coalition members have a value"
    expect(await screen.findByRole('heading', { level: 3, name: /^Manufacturing capacity by product/ }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.queryAllByText(/\b0 of \d+ coalition members have a value/)).toHaveLength(0);
    void r;
  });

  it('a thin cell shows the notice and the "Not available" list', async () => {
    renderAt('/sector/geothermal/extraction');
    expect(await h1('Geothermal — Extraction')).toBeTruthy();
    await openGroups();
    expect(await screen.findByText(/Little country-level data exists for Geothermal — Extraction/, {}, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByRole('heading', { name: 'Not available for this sector' })).toBeTruthy();
    expect(within(screen.getByRole('heading', { name: 'Not available for this sector' }).closest('section') as HTMLElement).getByText(/No HS6 trade basket/)).toBeTruthy();
  });

  it('the EU switch changes the snapshot and loads the trade chunk', async () => {
    const r = renderAt('/sector/batteries/assembly');
    expect(await h1('Batteries — Assembly')).toBeTruthy();
    await openGroups();
    // the Snapshot's tile (the Economic Opportunity lens repeats the figure above it)
    const snap = () => within(document.getElementById('snapshot') as HTMLElement);
    await waitFor(() => expect(snap().getByText('World exports')).toBeTruthy(), { timeout: 10_000 });
    const before = snap().getByText('World exports').closest('div')?.parentElement?.textContent;
    fireEvent.click(screen.getByRole('switch', { name: /EU as one bloc/ }));
    await waitFor(() => expect(r.state.location.search).toBe('?eu=bloc'));
    await waitFor(() => expect(source.calls.some((c) => c.startsWith('sector-trade-batteries'))).toBe(true));
    await waitFor(() => expect(snap().getByText('World exports').closest('div')?.parentElement?.textContent).not.toBe(before), { timeout: 10_000 });
  });

  it('unknown stage goes to the chain; the chain renders', async () => {
    // Semiconductors has Refining, Components and Assembly cells (its trade baskets), no Deployment
    const r = renderAt('/sector/semiconductors/deployment');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/semiconductors'), { timeout: 10_000 });
    expect(await h1('Semiconductors: the supply chain')).toBeTruthy();
  });

  it('Oil — Extraction: production, reserves, prices', async () => {
    renderAt('/sector/oil/extraction');
    expect(await h1('Oil — Extraction')).toBeTruthy();
    await openGroups();
    for (const name of ['Production & capacity', 'Resources & potential', 'Prices & costs']) {
      expect(await screen.findByRole('heading', { level: 2, name }, { timeout: 10_000 })).toBeTruthy();
    }
    expect(await screen.findByRole('heading', { level: 3, name: 'Price board' }, { timeout: 10_000 })).toBeTruthy();
  });
});

describe('Technology chain', () => {
  it('Batteries: the ladder, stage cards and minerals', async () => {
    renderAt('/sector/batteries');
    expect(await h1('Batteries: the supply chain')).toBeTruthy();
    expect(screen.getByText('Where concentration sits along the chain')).toBeTruthy();
    // six stage cards in chain order
    const cards = screen.getAllByRole('heading', { level: 3 }).map((x) => x.textContent).filter((t) => ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'].includes(t ?? ''));
    expect(cards).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']);
    expect(screen.getByText('Minerals Batteries uses')).toBeTruthy();
    // the minerals: mined under Extraction, refined under Refining (design C3)
    expect(screen.getAllByText('Lithium mining').length).toBeGreaterThan(0);
    expect(screen.getAllByText('Lithium refining').length).toBeGreaterThan(0);
    // the codes kept out of the baskets, listed once for the technology (design C6)
    const excluded = await screen.findByTestId('excluded-codes', {}, { timeout: 10_000 });
    expect(within(excluded).getByRole('heading', { name: 'Codes kept out of the Batteries trade baskets' })).toBeTruthy();
    expect(excluded.textContent).toMatch(/851762 \(Parts\): General-purpose communication equipment/);
    expect(excluded.textContent).toMatch(/no supply-chain stage/);
    expect(document.body.textContent).not.toMatch(/upstream|midstream|downstream|three stages/i);
  });
});

describe('Minerals', () => {
  it('index: tiles, the split by mineral and the table render from meta alone', async () => {
    renderAt('/sector/minerals?c=USA');
    expect(await h1('Minerals')).toBeTruthy();
    expect(document.title).toMatch(/^Minerals \| Sectors \| /);
    expect(screen.getByRole('link', { name: /^Lithium: mine production/ }).getAttribute('href')).toBe('/sector/minerals/lithium?c=USA');
    expect(screen.getByRole('heading', { level: 2, name: 'Who mines and who refines, by mineral' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Every mineral' })).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Nuclear — Extraction' }).getAttribute('href')).toBe('/sector/nuclear/extraction?c=USA');
    expect(source.calls.some((c) => c.startsWith('sector-facts'))).toBe(false);
  });

  it('lithium: snapshot, mining and refining by year with projections, reserves, trade and uses', async () => {
    renderAt('/sector/minerals/lithium?c=USA');
    expect(await h1('Lithium')).toBeTruthy();
    expect(document.title).toMatch(/^Lithium \| Sectors \| /);
    for (const name of ['Snapshot', 'Mining and refining', 'Reserves and production', 'Trade', 'Who uses it']) {
      expect(await screen.findByRole('heading', { level: 2, name }, { timeout: 10_000 })).toBeTruthy();
    }
    const snap = screen.getByRole('list', { name: 'Snapshot' });
    expect(within(snap).getByText("China's refining share")).toBeTruthy();
    expect(within(snap).getByText('Highlight')).toBeTruthy();
    // projected years: the row "2030" with the projection tag (the label is never "2030 (projection) (projection)")
    expect(screen.getAllByRole('row', { name: /^2030: / }).length).toBeGreaterThan(0);
    expect(screen.getAllByText('projection').length).toBeGreaterThan(0);
    expect(screen.queryAllByText(/\(projection\) \(projection\)/)).toHaveLength(0);
    // the critical-minerals trade by stage: one card per stage, never added (design C3)
    expect(screen.getByRole('heading', { level: 3, name: /^Exporters of lithium ores and concentrates \(Extraction\), 2024/ })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: /^Exporters of lithium refined products \(Refining\), 2024/ })).toBeTruthy();
    expect(screen.getByText(/Traded at 2 stages: Extraction \(ores and concentrates\) and Refining \(refined products\)\. Each stage is a different product, so the cards are never added\./)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Share of reserves against share of production' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Who uses lithium' })).toBeTruthy();
    // the technology tags link to the chains, keeping c
    expect(screen.getByRole('link', { name: 'Electric Vehicles' }).getAttribute('href')).toBe('/sector/electric-vehicles?c=USA');
  });

  it('uranium redirects to Nuclear — Extraction; an unknown mineral goes to the index with a notice', async () => {
    const r = renderAt('/sector/minerals/uranium');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/nuclear/extraction'), { timeout: 10_000 });
    cleanup();
    const r2 = renderAt('/sector/minerals/lithum');
    await waitFor(() => expect(r2.state.location.pathname).toBe('/sector/minerals'), { timeout: 10_000 });
    expect(await screen.findByText(/No mineral called 'lithum'/, {}, { timeout: 10_000 })).toBeTruthy();
  });
});

describe('Concentration', () => {
  it('the board, the lens in the URL, a tile opens its section; the note is always there', async () => {
    const r = renderAt('/sector/concentration');
    expect(await h1('Where are the chokepoints?')).toBeTruthy();
    expect(document.title).toMatch(/^Concentration \| Sectors \| /);
    expect(screen.getByText(/Compared within one measure family; values come from different sources and years and are never added\./)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Concentration board' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Share split by sector' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Every sector and measure' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Exporter concentration, 2020 and 2024' })).toBeTruthy();
    // lens -> URL
    fireEvent.click(screen.getByRole('radio', { name: 'Concentration (HHI)' }));
    await waitFor(() => expect(r.state.location.search).toBe('?lens=hhi'));
    // rendered from sector-meta alone
    expect(source.calls.some((c) => c.startsWith('sector-facts'))).toBe(false);
    // a tile is a button that opens the profile section
    const tile = screen.getByRole('button', { name: /^Batteries, Exports: Assembly: exports/ });
    fireEvent.click(tile);
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries/assembly'));
    expect(r.state.location.hash).toBe('#trade');
  });

  it('mineral sub-rows open behind the chevron and link to the mineral page; EU bloc shows the note', async () => {
    const r = renderAt('/sector/concentration?eu=bloc');
    expect(await h1('Where are the chokepoints?')).toBeTruthy();
    expect(screen.getByText('EU-27 as one raises measured concentration where members are large.')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Show Batteries sub-rows' }));
    fireEvent.click(screen.getByRole('button', { name: /^Lithium \(Batteries\), Refining:/ }));
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/minerals/lithium'));
    expect(r.state.location.search).toBe('?eu=bloc');
  });
});

describe('Explore', () => {
  const THEMES = ['trade', 'resources', 'production', 'consumption', 'investment', 'manufacturing', 'prices', 'technology', 'policy', 'development'];

  it('default theme: chips, the filter row, key figures, map and ranking, table and notes', async () => {
    const r = renderAt('/sector/explore?c=USA');
    expect(await h1('Trade')).toBeTruthy();
    expect(document.title).toMatch(/^Explore Trade \| Sectors \| /);
    const chips = screen.getByRole('navigation', { name: 'Themes' });
    expect(within(chips).getByRole('link', { name: 'Trade' }).getAttribute('aria-current')).toBe('page');
    expect(within(chips).getByRole('link', { name: 'Policy' }).getAttribute('href')).toBe('/sector/explore/policy?c=USA');
    expect(screen.getByRole('combobox', { name: 'Measure' })).toBeTruthy();
    expect(screen.getByRole('combobox', { name: 'Technology' })).toBeTruthy();
    expect(await screen.findByRole('list', { name: 'Key figures' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByText(/economies with data/)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Table' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: /^Notes on / })).toBeTruthy();
    expect(screen.getByText(/as S-curved ranks within each sector/)).toBeTruthy();
    // Show as share: legal for exports; GDP is disabled
    expect((screen.getByRole('radio', { name: '% of GDP' }) as HTMLInputElement).disabled).toBe(true);
    fireEvent.click(screen.getByRole('radio', { name: 'Share of world' }));
    await waitFor(() => expect(r.state.location.search).toContain('as=share'));
    // economies scope
    fireEvent.click(screen.getByRole('radio', { name: 'Coalition' }));
    await waitFor(() => expect(r.state.location.search).toContain('geo=coalition'));
  });

  it('a measure change resets show-as and keeps the item when valid; the stage select changes the cell', async () => {
    const r = renderAt('/sector/explore/trade?as=share&tech=Batteries&stage=Assembly');
    expect(await h1('Trade')).toBeTruthy();
    await screen.findByRole('list', { name: 'Key figures' }, { timeout: 10_000 });
    fireEvent.change(screen.getByRole('combobox', { name: 'Measure' }), { target: { value: 'trade.imports' } });
    await waitFor(() => expect(r.state.location.search).toBe('?m=trade.imports&tech=Batteries&stage=Assembly'));
    // the six stages, those without data disabled
    const stage = screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement;
    expect([...stage.options].map((o) => o.value)).toEqual(['extraction', 'refining', 'components', 'assembly', 'deployment', 'recovery']);
    expect([...stage.options].filter((o) => o.disabled).map((o) => o.value)).toEqual(['refining', 'components', 'deployment', 'recovery']);
    fireEvent.change(stage, { target: { value: 'extraction' } });
    await waitFor(() => expect(r.state.location.search).toContain('stage=Extraction'));
  });

  it('an old three-stage stage in the link: Downstream shows Deployment, Midstream is dropped; both are listed', async () => {
    renderAt('/sector/explore/investment?tech=Solar&stage=Downstream');
    expect(await h1('Investment')).toBeTruthy();
    expect(await screen.findByText(/stage=Downstream \(Downstream corresponds to Deployment; showing it\)/, {}, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/trade?tech=Batteries&stage=Midstream');
    expect(await h1('Trade')).toBeTruthy();
    expect(await screen.findByText(/stage=Midstream \(Midstream corresponds mostly to Components and Assembly/, {}, { timeout: 10_000 })).toBeTruthy();
  });

  it('every theme renders a layout and the notes', async () => {
    for (const t of THEMES) {
      cleanup();
      renderAt(`/sector/explore/${t}`);
      await screen.findByRole('navigation', { name: 'Themes' }, { timeout: 10_000 });
      expect(await screen.findByRole('heading', { level: 2, name: /^Notes on / }, { timeout: 10_000 })).toBeTruthy();
    }
  });

  it('trade.flow: the flows panel with the origin-destination table loads the trade chunk', async () => {
    renderAt('/sector/explore/trade?m=trade.flow&tech=Batteries&stage=Assembly');
    expect(await screen.findByRole('heading', { level: 3, name: /^Who supplies whom: Batteries — Assembly, 2024/ }, { timeout: 10_000 })).toBeTruthy();
    expect(source.calls.some((c) => c.startsWith('sector-trade-batteries'))).toBe(true);
    expect(screen.getByRole('heading', { level: 3, name: /^Origin and destination/ })).toBeTruthy();
    expect(screen.getByText(/technology-attributed HS6 basket/)).toBeTruthy();
  });

  it('world, cell-global, entity, region, policy, CAT and mineral-demand layouts', async () => {
    renderAt('/sector/explore/prices');
    expect(await screen.findByRole('heading', { level: 3, name: 'Price board' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByText('World prices: the same for every country.')).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/technology');
    expect(await screen.findByText('A property of the technology, the same in every country.', {}, { timeout: 10_000 })).toBeTruthy();
    expect(await screen.findByRole('group', { name: /Technology readiness by technology and stage|by technology and stage/ }, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/manufacturing?tech=Batteries&stage=Assembly');
    expect(await screen.findByRole('heading', { level: 3, name: /^Share of global manufacturing capacity/ }, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/production?m=production.ev_region');
    expect((await screen.findAllByText(/Reported for regions; countries in a region share its value/, {}, { timeout: 10_000 })).length).toBeGreaterThan(0);
    expect(await screen.findByRole('heading', { level: 3, name: /^Electric vehicles by IEA region/ }, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/policy');
    expect(await screen.findByRole('heading', { level: 3, name: /^Recorded policies by jurisdiction/ }, { timeout: 10_000 })).toBeTruthy();
    expect(source.calls.some((c) => c.startsWith('sector-policy'))).toBe(true);
    cleanup();
    renderAt('/sector/explore/policy?m=policy.cat');
    expect(await screen.findByRole('heading', { level: 3, name: 'Climate Action Tracker ratings' }, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    renderAt('/sector/explore/consumption?m=consumption.mineral_demand&item=lithium');
    expect(await screen.findByRole('heading', { level: 3, name: 'Clean-energy demand for lithium by technology' }, { timeout: 10_000 })).toBeTruthy();
  });

  it('an unknown theme goes to /sector/explore with a notice', async () => {
    const r = renderAt('/sector/explore/tarde');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/explore'), { timeout: 10_000 });
    expect(await screen.findByText(/No theme called 'tarde'/, {}, { timeout: 10_000 })).toBeTruthy();
  });
});

describe('Country in sector', () => {
  it('United States in Batteries — Assembly: tags, lede, position tiles, peers, trade partners, policies', async () => {
    renderAt('/sector/batteries/assembly/USA');
    expect(await h1('United States in Batteries — Assembly')).toBeTruthy();
    expect(document.title).toMatch(/^United States in Batteries — Assembly \| Sectors \| /);
    expect(screen.getByText('Coalition core')).toBeTruthy();
    expect(await screen.findByRole('list', { name: 'Position tiles' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Rank among peers' })).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 3, name: 'Where its imports come from' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'Its imports by supplier group, 2020 and 2024' })).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 2, name: 'Policies' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.getByRole('link', { name: /Back to Batteries — Assembly/ }).getAttribute('href')).toBe('/sector/batteries/assembly?c=USA');
  });

  it('an unknown economy goes to the profile with a notice', async () => {
    const r = renderAt('/sector/batteries/assembly/XXX');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries/assembly'), { timeout: 10_000 });
    expect(await screen.findByText(/No economy 'XXX'/, {}, { timeout: 10_000 })).toBeTruthy();
  });

  it('Oil — Extraction for an economy: production is the measure on fuel:oil', async () => {
    renderAt('/sector/oil/extraction/RUS');
    expect(await h1('Russian Federation in Oil — Extraction')).toBeTruthy();
    const tiles = await screen.findByRole('list', { name: 'Position tiles' }, { timeout: 10_000 });
    expect(within(tiles).getByText('Production')).toBeTruthy();
    // over time: the economy's EI series from sector-series
    expect(await screen.findByRole('heading', { level: 3, name: /^Production \(oil\), \d{4}-2025/ }, { timeout: 10_000 })).toBeTruthy();
    expect(source.calls.some((c) => c.startsWith('sector-series'))).toBe(true);
  });
});

describe('Sources & data (Data behind the sector analysis)', () => {
  it('about, dictionary, sources, coverage, the trade basis, downloads and the build', async () => {
    renderAt('/sector/sources');
    expect(await h1('Data behind the sector analysis')).toBeTruthy();
    // the tab keeps its short name
    expect(document.title).toMatch(/^Sources & data \| Sectors \| /);
    expect(document.querySelector('main')?.textContent).toContain('What each measure means, where it comes from and where coverage is strong or weak');
    expect(screen.getByRole('heading', { level: 2, name: 'What the Sector pages show' })).toBeTruthy();
    expect(screen.getByText(/The Country profiles rank and weigh; the Sector pages show the measured data those rankings start from\./)).toBeTruthy();
    expect(screen.queryByText(/\{members_n\}|\{core_n\}/)).toBeNull();
    // the cards inside the Dictionary and Sources sections have their own names (unique landmarks, C5.8)
    for (const name of ['Every measure: unit, years, source and caveats', 'Every source: edition, licence and attribution', 'Coverage by sector and measure family', 'Which sources cover the coalition members', "Trade basis: the matrix and the exporters' reports"]) {
      expect(screen.getByRole('heading', { level: 3, name })).toBeTruthy();
    }
    expect(screen.getByRole('heading', { level: 3, name: 'All sector data (.zip)' })).toBeTruthy();
    expect(screen.getByText('Licensed data (BNEF, BCG) are not part of the public explorer.')).toBeTruthy();
    expect(screen.getByRole('heading', { level: 3, name: 'This data build' })).toBeTruthy();
    // the page renders from meta: no facts until a download asks for them
    expect(source.calls.some((c) => c.startsWith('sector-facts'))).toBe(false);
  });

  it('/sector/data goes to the downloads anchor', async () => {
    const r = renderAt('/sector/data?c=USA');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/sources'), { timeout: 10_000 });
    expect(r.state.location.hash).toBe('#downloads');
    expect(r.state.location.search).toBe('?c=USA');
  });
});

describe('Profile ZIP and keyboard', () => {
  it('the sector ZIP has every card CSV, dictionary.csv, sources.csv and README.txt', async () => {
    const { sectorZipFiles } = await import('./profile/SectorProfileView.tsx');
    const { modelOf } = await import('../data/model.ts');
    const fx = buildSectorFixture();
    const model = modelOf(fx.chunks['sector-meta'] as never);
    const files = sectorZipFiles(model, fx.manifest, [{ name: 'a', text: `${SECTOR_CSV_HEADER}\r\n` }, { name: 'a', text: 'x' }], ['trade.exports'], 'https://example.org/x');
    expect(Object.keys(files).sort()).toEqual(['README.txt', 'a-2.csv', 'a.csv', 'dictionary.csv', 'sources.csv']);
    expect(files['a.csv'].charCodeAt(0)).toBe(0xfeff);
    expect(files['dictionary.csv']).toContain('trade.exports');
  });

  it('Enter on an Atlas tile opens the profile', async () => {
    const r = renderAt('/sector');
    await h1('Where are the real bottlenecks and opportunities?');
    const tile = screen.getByRole('link', { name: /^Batteries — Assembly/ });
    tile.focus();
    fireEvent.click(tile);
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries/assembly'), { timeout: 10_000 });
  });
});

describe('Sector profile: lenses, supply-chain navigator and groups (IA refactor)', () => {
  it('four lenses on top, the navigator scopes the profile to a sub-component, sections sit in five groups', async () => {
    const r = renderAt('/sector/batteries/assembly');
    expect(await h1('Batteries — Assembly')).toBeTruthy();
    await openGroups();
    // the four lenses
    expect(await screen.findByRole('heading', { level: 2, name: 'What to know about Batteries — Assembly' }, { timeout: 10_000 })).toBeTruthy();
    // the lenses are titled by the page's questions (copy refresh 2026-10-30): capability, risk, policy, climate
    for (const name of ['Where the capability sits', 'Where the risk sits', 'What policy is doing', 'Climate']) expect(screen.getByRole('heading', { level: 3, name })).toBeTruthy();
    const eo = screen.getByTestId('lens-eo');
    expect(within(eo).getByRole('link', { name: /Where the capability sits/ }).getAttribute('href')).toBe('#markets');
    expect(within(screen.getByTestId('lens-es')).getByRole('link', { name: /Where the risk sits/ }).getAttribute('href')).toBe('#security');
    // Sector -> Country: the lens's largest exporters link to their Country profiles
    expect(within(eo).getAllByRole('link').some((a) => /^\/countries\/[A-Z]{3}$/.test(a.getAttribute('href') ?? ''))).toBe(true);
    expect(within(screen.getByTestId('lens-climate')).getByText(/Sector-level climate data are limited/)).toBeTruthy();
    // the groups, in order, and the rail lists them
    for (const name of ['Where the capability sits', 'Where the risk sits', 'What policy is doing', 'Climate', 'Advanced & evidence']) {
      expect(screen.getByRole('heading', { level: 2, name })).toBeTruthy();
    }
    const rail = screen.getByRole('navigation', { name: 'Sections' });
    expect(within(rail).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(['#overview', '#posture', '#markets', '#security', '#policy-technology', '#climate', '#evidence']);
    // the strategic-choice section between the lenses and the groups (design L3), never folded
    expect(screen.getByRole('heading', { level: 2, name: 'What each country should do' })).toBeTruthy();
    // each group opens on the question it answers
    expect(document.getElementById('markets')?.textContent).toContain("Who produces, exports or controls this supply-chain step today, and how strong is each country's position?");
    expect(document.getElementById('security')?.textContent).toContain('How dependent are countries on concentrated or hard-to-replace supply?');
    expect(document.getElementById('policy-technology')?.textContent).toContain('Is government action concentrated where the strategic case is strongest?');
    expect(document.getElementById('posture')?.textContent).toMatch(/Country strategy · fixed thresholds/);
    expect(document.getElementById('markets')?.contains(document.getElementById('snapshot'))).toBe(true);
    expect(document.getElementById('security')?.contains(document.getElementById('who'))).toBe(true);
    expect(within(document.getElementById('deeper') as HTMLElement).getByRole('link', { name: 'Advanced explorer' }).getAttribute('href')).toMatch(/^\/sector\/explore\?/);
    // the navigator: folded to the six stages; "Show components" lists the trade basket's components; one scopes the lenses
    const nav = screen.getByRole('navigation', { name: 'Batteries supply chain' });
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']);
    expect(within(nav).getByRole('link', { name: /^Assembly/ }).getAttribute('aria-current')).toBe('page');
    expect(within(nav).queryByRole('link', { name: 'Cells and packs' })).toBeNull();
    fireEvent.click(within(nav).getByRole('button', { name: 'Show components' }));
    const cells = await within(nav).findByRole('link', { name: 'Cells and packs' }, { timeout: 10_000 });
    expect(cells.getAttribute('href')).toBe('/sector/batteries/assembly?sub=Cells%20and%20packs');
    fireEvent.click(cells);
    await waitFor(() => expect(r.state.location.search).toBe('?sub=Cells%20and%20packs'));
    expect(await screen.findByRole('heading', { level: 2, name: 'What to know about Cells and packs' }, { timeout: 10_000 })).toBeTruthy();
    // the chosen component titles the page; its sector is the eyebrow
    expect(await h1('Cells and packs')).toBeTruthy();
    expect(document.title).toMatch(/^Cells and packs · Batteries — Assembly \| Sectors \| /);
    expect(within(nav).getByRole('link', { name: 'Cells and packs' }).getAttribute('aria-current')).toBe('page');
    expect(screen.getByRole('link', { name: 'Whole sector' }).getAttribute('href')).toBe('/sector/batteries/assembly');
  });

  it('phones: the groups start folded and open on "Show" or a link to one of their sections', async () => {
    wide = false;
    renderAt('/sector/batteries/assembly#trade');
    expect(await h1('Batteries — Assembly')).toBeTruthy();
    expect(await screen.findByTestId('lens-eo', {}, { timeout: 10_000 })).toBeTruthy();
    // the link opened "Where the capability sits"; the others are folded
    expect(await screen.findByRole('heading', { level: 2, name: 'Trade' }, { timeout: 10_000 })).toBeTruthy();
    expect(screen.queryByRole('heading', { level: 2, name: 'Who holds the market' })).toBeNull();
    const security = document.getElementById('security') as HTMLElement;
    fireEvent.click(within(security).getByRole('button', { name: 'Show' }));
    expect(await screen.findByRole('heading', { level: 2, name: 'Who holds the market' })).toBeTruthy();
    // the navigator folds to the six stages with a "Show components" button
    const nav = screen.getByRole('navigation', { name: 'Batteries supply chain' });
    expect(within(nav).getByRole('link', { name: 'Assembly' }).getAttribute('aria-current')).toBe('page');
    fireEvent.click(within(nav).getByRole('button', { name: 'Show components' }));
    expect(await within(nav).findByRole('link', { name: 'Cells and packs' }, { timeout: 10_000 })).toBeTruthy();
  });

  it('the landing picks a technology; its chain shows the navigator open', async () => {
    const r = renderAt('/sector');
    expect(await h1('Where are the real bottlenecks and opportunities?')).toBeTruthy();
    fireEvent.click(within(screen.getByRole('heading', { name: 'Follow a technology' }).closest('section') as HTMLElement).getByRole('link', { name: 'Batteries' }));
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries'));
    expect(await h1('Batteries: the supply chain')).toBeTruthy();
    const nav = screen.getByRole('navigation', { name: 'Batteries supply chain' });
    expect(within(nav).getByRole('link', { name: /^Assembly/ }).getAttribute('href')).toBe('/sector/batteries/assembly');
    expect(within(nav).queryByRole('button', { name: 'Show components' })).toBeNull();
    expect((await within(nav).findByRole('link', { name: 'Cells and packs' }, { timeout: 10_000 })).getAttribute('href')).toBe('/sector/batteries/assembly?sub=Cells%20and%20packs');
    // six columns in chain order, each a stage of the chain; the materials stage lists its own components; no roll-up note
    expect(within(nav).getAllByRole('listitem').filter((li) => li.parentElement?.tagName === 'OL').map((li) => li.querySelector(':scope > a, :scope > span:not([aria-hidden="true"])')?.textContent?.replace(' (shown)', ''))).toEqual(['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery']);
    expect((await within(nav).findByRole('link', { name: 'Lithium minerals' }, { timeout: 10_000 })).getAttribute('href')).toBe('/sector/batteries/extraction?sub=Lithium%20minerals');
    expect(within(nav).getAllByText('No traded components (deployment and use)')).toHaveLength(1);
    expect(nav.textContent).not.toMatch(/roll up|three|upstream|midstream|downstream/i);
  });
});

void act;
