// "Show a variable" under the fence tables of the Country page (2026-10-30; src/country/views/profile/VariableChart.tsx,
// src/sector/views/explore/EmbeddedExplore.tsx, the table src/posture/variableCharts.ts): on the R-written Country
// fixture with the posture fixture's Aland served as the United States, and the Sector fixture store (as the Sector
// view tests mock it). Each picker lists its analysis' variables, the table's columns first; no Sector data load
// until a variable with a chart is picked (the Country profile's first view does not grow); a pick mounts the
// embedded explorer on the right measure, node (tech + stage) and economy (c), on the variable's first chart, with
// "Open in Sectors" to the full explore page; the chart follows the selected row and says when the Sector data
// have nothing for the node; the reader can pick another chart; a variable with no Sector chart says so and why.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { setPostureStore } from '../../posture/store.ts';
import { buildSectorFixture, fixtureSource as sectorSource } from '../../sector/__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../../sector/data/store.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { fixtureSource, haveFixture } from './fixture.ts';

const restore: (() => void)[] = [];
let sector: ReturnType<typeof sectorSource>;

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  const mm = window.matchMedia;
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q),
    media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  restore.push(() => (window.matchMedia = mm));
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  Element.prototype.scrollIntoView = function () {} as typeof Element.prototype.scrollIntoView;
  if (!('ResizeObserver' in globalThis)) {
    (globalThis as { ResizeObserver?: unknown }).ResizeObserver = class {
      observe() {}
      unobserve() {}
      disconnect() {}
    };
    restore.push(() => delete (globalThis as { ResizeObserver?: unknown }).ResizeObserver);
  }
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
  setCountryStoreForTests(new CountryStore(fixtureSource()));
  setPostureStore(fixtureStore({ economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
  setEngineClient(new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/'));
  sector = sectorSource(buildSectorFixture());
  setSectorStoreForTests(new SectorStore(sector));
});

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
  setPostureStore(null);
  setSectorStoreForTests(null);
  setEngineClient(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

async function usa(path = '/countries/USA') {
  const r = renderAt(path);
  await screen.findByRole('heading', { level: 1, name: 'United States' }, { timeout: 30_000 });
  await screen.findByRole('region', { name: 'Country Strategy Map' }, { timeout: 30_000 });
  return r;
}

const sec = (id: string) => document.getElementById(id) as HTMLElement;
const heads = (el: HTMLElement) => [...el.querySelectorAll('thead th')].map((th) => th.textContent);
const rowOf = (el: HTMLElement, node: string) => [...el.querySelectorAll('tbody tr')].find((tr) => tr.children[0]?.textContent === node) as HTMLElement;
/** The Sector chunks fetched so far (the manifest and the chunk files). */
const sectorCalls = () => [...sector.calls];
/** A picker's select and its options by group ("" for the lead option). */
function picker(id: string) {
  const box = screen.getByTestId(`${id}-variables`);
  const select = within(box).getByLabelText('Show a variable') as HTMLSelectElement;
  const group = (label: string) => [...select.querySelectorAll(`optgroup[label="${label}"] option`)].map((o) => o.textContent);
  return { box, select, group, pick: (value: string) => fireEvent.change(select, { target: { value } }) };
}
async function openFold(id: string): Promise<HTMLElement> {
  const d = document.getElementById(`fold-${id}`) as HTMLElement;
  fireEvent.click(d.querySelector('summary') as HTMLElement);
  await waitFor(() => expect(document.getElementById(`fold-${id}`)?.hasAttribute('open')).toBe(true));
  return document.getElementById(`fold-${id}`) as HTMLElement;
}
/** Columns of a fence table that are not variables: the step, the layer, the choice and the explaining texts. */
const NOT_VARIABLES = new Set(['Supply-chain step', 'Technology', 'Layer', 'Choice', 'Basis', 'Evidence', 'Security rationale', 'Why Watch']);

describe.skipIf(!haveFixture())('Country page: Show a variable (the Sector chart of a fence variable)', () => {
  it('each picker lists its analysis’ variables, the table’s columns first; no Sector data load before a pick', async () => {
    await usa();
    // the Medium fence's Prize, Competitive Position and Accelerate tables
    const prize = picker('medium-prize');
    const pt = within(sec('medium-prize')).getByTestId('medium-prize-table');
    expect(prize.group('In the table')).toEqual(heads(pt).filter((h) => !NOT_VARIABLES.has(h ?? '')));
    expect(prize.group('In the table')).toEqual(['Strategic Industrial Value', 'Market prize', 'Industrial commons', 'Future option', 'World demand growth', 'Home-market growth', 'Investment momentum', 'Market outlook', 'Supply-chain vulnerability']);
    const pos = picker('medium-position');
    expect(pos.group('In the table')).toEqual(heads(within(sec('medium-position')).getByTestId('medium-position-table')).filter((h) => !NOT_VARIABLES.has(h ?? '')));
    expect(pos.group('Behind the analysis')).toEqual(['Export feasibility (pathway)', 'Operating capacity', 'Committed and announced pipeline', 'Input cost', 'Resource base']);
    const acc = picker('medium-accelerate');
    expect(acc.group('In the table')).toEqual(heads(within(sec('medium-accelerate')).getByTestId('medium-accelerate-table')).filter((h) => !NOT_VARIABLES.has(h ?? '')));
    // the High fence's drop-down: the need variables
    const high = await openFold('high-all');
    expect(within(high).getByTestId('high-variables')).toBeTruthy();
    expect(picker('high').group('In the table')).toEqual(heads(within(high).getByTestId('high-table')).filter((h) => !NOT_VARIABLES.has(h ?? '')));
    expect(picker('high').group('In the table')).toEqual(['Supply-chain vulnerability', 'Import dependence', 'Supplier concentration', 'Risky supplier exposure', 'Lack of trusted alternatives']);
    // the Low fence's drop-downs: the same variables
    const low = await openFold('low-supply-all');
    expect(picker('low-prize').group('In the table')).toEqual(heads(within(low).getByTestId('low-prize-table')).filter((h) => !NOT_VARIABLES.has(h ?? '')));
    expect(picker('low-position').group('In the table')).toEqual(pos.group('In the table'));
    await openFold('low-deploy-all');
    expect(picker('low-deploy').group('In the table')).toEqual(acc.group('In the table'));
    // the node line: the table's first row until a row is selected
    expect(within(prize.box).getByTestId('variable-chart-node').textContent).toBe('For Batteries · Final Assembly & Equipment, the first row; select a row to show another node.');
    // nothing of the Sector data has been fetched: the first view does not grow
    expect(sectorCalls()).toEqual([]);
  }, 120_000);

  it('Investment momentum mounts the explore map of investment.annual for the node and the economy, with Open in Sectors', async () => {
    await usa();
    const prize = picker('medium-prize');
    prize.pick('investment');
    const ex = await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    expect([ex.getAttribute('data-m'), ex.getAttribute('data-tech'), ex.getAttribute('data-stage'), ex.getAttribute('data-c'), ex.getAttribute('data-part')]).toEqual([
      'investment.annual', 'Batteries', 'Assembly', 'USA', 'map',
    ]);
    expect(within(ex).getByTestId('variable-chart-open').getAttribute('href')).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Batteries&stage=Assembly');
    expect(within(ex).getByTestId('variable-chart-basis').textContent).toMatch(/^Same source as the variable\. Annual clean-technology investment \(Clean Investment Monitor\)/);
    // the map card, the economy highlighted; only the map (the ranking is another chart)
    await waitFor(() => expect(within(ex).getByLabelText(/^Map of Annual investment/)).toBeTruthy(), { timeout: 30_000 });
    expect(within(ex).queryByLabelText(/Annual investment, ranked/)).toBeNull();
    // the Sector data loaded on the pick: the manifest, sector-meta, sector-facts
    expect(sectorCalls()[0]).toBe('manifest.json');
    expect(sectorCalls().some((c) => c.startsWith('sector-meta'))).toBe(true);
    expect(sectorCalls().some((c) => c.startsWith('sector-facts'))).toBe(true);
    // another chart of the layout: the ranking, then the table
    fireEvent.change(within(ex).getByLabelText('Chart'), { target: { value: 'bars' } });
    await waitFor(() => expect(within(ex).getByLabelText(/Annual investment, ranked/)).toBeTruthy());
    expect(within(ex).queryByLabelText(/^Map of Annual investment/)).toBeNull();
    fireEvent.change(within(ex).getByLabelText('Chart'), { target: { value: 'table' } });
    await waitFor(() => expect(ex.getAttribute('data-part')).toBe('table'));
    expect(within(ex).getByRole('table')).toBeTruthy();
    // the page's own URL never changed
    expect(window.location.search).toBe('');
    // another variable starts on its own measure, item and chart: Home-market growth reads the deployment at home
    prize.pick('home');
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-item-note').textContent).toMatch(/^The Sector data have no annual investment for Batteries · Deployment & Use; showing /), { timeout: 30_000 });
    const home = within(prize.box).getByTestId('variable-chart-explore');
    expect([home.getAttribute('data-m'), home.getAttribute('data-stage'), home.getAttribute('data-part')]).toEqual(['investment.annual', 'Deployment', 'map']);
    expect(within(home).getByTestId('variable-chart-basis').textContent).toMatch(/^A related Sector measure, not the variable’s own input\. Deployment investment/);
  }, 120_000);

  it('the chart follows the selected row, and says when the Sector data have nothing for its node', async () => {
    await usa();
    const prize = picker('medium-prize');
    prize.pick('investment');
    await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    // select Solar Final Assembly in the table (the drawer opens; the chart moves to the node)
    fireEvent.click(rowOf(within(sec('medium-prize')).getByTestId('medium-prize-table'), 'Solar · Final Assembly & Equipment'));
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-node').textContent).toBe('For Solar · Final Assembly & Equipment, the selected row.'), { timeout: 30_000 });
    const ex = await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    // the fixture has no Solar Assembly investment: the note says so and names what is shown instead
    const note = within(ex).getByTestId('variable-chart-item-note');
    expect(note.getAttribute('data-covered')).toBe('no');
    expect(note.textContent).toMatch(/^The Sector data have no annual investment for Solar · Final Assembly & Equipment; showing Solar — Deployment\.$/);
    expect(`${ex.getAttribute('data-tech')}|${ex.getAttribute('data-stage')}`).toBe('Solar|Deployment');
    // the Open in Sectors link opens what is shown
    expect(within(ex).getByTestId('variable-chart-open').getAttribute('href')).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Solar&stage=Assembly');
  }, 120_000);

  it('High fence: Supplier concentration opens the economy’s import partners (trade.flow) for the node', async () => {
    await usa();
    const fold = await openFold('high-all');
    const high = picker('high');
    high.pick('concentration');
    // the top node, Batteries Refining, has no bilateral flows in the fixture: said, and the shown basket named
    const first = await within(high.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    expect(within(first).getByTestId('variable-chart-item-note').textContent).toMatch(/^The Sector data have no bilateral flow for Batteries · Refining & Processing; showing /);
    // select Solar Refining: its import partners, the economy highlighted
    fireEvent.click(rowOf(within(fold).getByTestId('high-table'), 'Solar · Refining & Processing'));
    await waitFor(() => expect(within(high.box).getByTestId('variable-chart-explore').getAttribute('data-tech')).toBe('Solar'), { timeout: 30_000 });
    const ex = within(high.box).getByTestId('variable-chart-explore');
    expect([ex.getAttribute('data-m'), ex.getAttribute('data-tech'), ex.getAttribute('data-stage'), ex.getAttribute('data-c'), ex.getAttribute('data-part')]).toEqual([
      'trade.flow', 'Solar', 'Refining', 'USA', 'partners',
    ]);
    expect(within(ex).queryByTestId('variable-chart-item-note')).toBeNull();
    expect(await within(ex).findByText(/: where its imports come from, 2024$/, {}, { timeout: 30_000 })).toBeTruthy();
    expect(within(ex).queryByRole('heading', { name: /^Who supplies whom/ })).toBeNull();
    expect(within(ex).getByTestId('variable-chart-open').getAttribute('href')).toBe('/sector/explore/trade?c=USA&m=trade.flow&tech=Solar&stage=Refining');
    // the measures of import dependence: a select of the two trade measures at a Refining node (the fixture's labels)
    high.pick('import');
    const ex2 = await waitFor(() => {
      const e = within(high.box).getByTestId('variable-chart-explore');
      expect(e.getAttribute('data-m')).toBe('trade.net');
      return e;
    }, { timeout: 30_000 });
    const measure = within(high.box).getByLabelText('Sector measure') as HTMLSelectElement;
    expect([...measure.options].map((o) => o.textContent)).toEqual(['Net trade (exports − imports)', 'Imports of the sector’s traded goods']);
    fireEvent.change(measure, { target: { value: '1' } });
    await waitFor(() => expect(within(high.box).getByTestId('variable-chart-explore').getAttribute('data-m')).toBe('trade.imports'));
    expect(ex2).toBeTruthy();
  }, 120_000);

  it('a variable without a Sector chart says so and why, and loads no Sector data', async () => {
    await usa();
    const prize = picker('medium-prize');
    prize.pick('commons');
    expect(within(prize.box).getByTestId('variable-chart-none').textContent).toBe(
      'No Sector chart for this variable. A node attribute of the Yards & Fences evidence, assessed node by node; the Sector data have no measure of it.',
    );
    prize.pick('opportunity');
    expect(within(prize.box).getByTestId('variable-chart-none').textContent).toMatch(/^No Sector chart for this variable\. A composite score: the market prize, industrial commons and future option value\./);
    const pos = picker('medium-position');
    pos.pick('momentum');
    expect(within(pos.box).getByTestId('variable-chart-none').textContent).toMatch(/monthly trade .* which the Sector data do not carry/);
    // a variable whose measures cover other technologies: the Target / climate gap of Batteries Deployment
    const acc = picker('medium-accelerate');
    acc.pick('gap');
    expect(within(acc.box).getByTestId('variable-chart-none').textContent).toBe(
      'No Sector chart for this variable at Batteries · Deployment & Use. The Sector measures that chart it cover other stages or technologies.',
    );
    expect(within(acc.box).queryByTestId('variable-chart-explore')).toBeNull();
    expect(sectorCalls()).toEqual([]);
  }, 120_000);
});
