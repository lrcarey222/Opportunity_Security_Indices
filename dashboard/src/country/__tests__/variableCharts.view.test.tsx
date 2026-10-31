// "Show a variable" under the fence tables of the Country page (2026-10-30; indicators, not composites, 2026-10-31;
// src/country/views/profile/VariableChart.tsx, src/sector/views/explore/EmbeddedExplore.tsx, the table
// src/posture/variableCharts.ts): on the R-written Country fixture with the posture fixture's Aland served as the
// United States, and the Sector fixture store (as the Sector view tests mock it). Each picker offers the indicators
// with a chart at the step, grouped by the family they feed; no composite or family score is an option; one line
// names the families it cannot offer and where to see them; no Sector data load until an indicator is picked (the
// Country profile's first view does not grow); a pick mounts the embedded explorer on that one indicator (no
// second measure select), on the right measure, step (tech + stage) and economy (c), with "Open in Sectors"; the
// chart follows the selected row and says when the Sector data have nothing for the step; a pick the new row does
// not offer is said in one line and the picker stays usable; the reader can pick another chart.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { setPostureStore } from '../../posture/store.ts';
import { familyGroup, fenceFamilies, variableOptions, type VariableAnalysis } from '../../posture/variableCharts.ts';
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
const heads = (el: HTMLElement) => [...el.querySelectorAll('thead th')].map((th) => th.textContent ?? '');
const rowOf = (el: HTMLElement, node: string) => [...el.querySelectorAll('tbody tr')].find((tr) => tr.children[0]?.textContent === node) as HTMLElement;
/** The Sector chunks fetched so far (the manifest and the chunk files). */
const sectorCalls = () => [...sector.calls];
/** A picker's select, its groups (optgroup label and option texts) and its step (tech + stage of data-node). */
function picker(id: string) {
  const box = screen.getByTestId(`${id}-variables`);
  const select = within(box).getByLabelText('Show a variable') as HTMLSelectElement;
  const groups = () => [...select.querySelectorAll('optgroup')].map((g) => [g.getAttribute('label'), [...g.querySelectorAll('option')].map((o) => o.textContent)] as [string, string[]]);
  const options = () => [...select.querySelectorAll('optgroup option')].map((o) => ({ value: (o as HTMLOptionElement).value, text: o.textContent ?? '' }));
  const step = () => {
    const [tech, stage] = (within(box).getByTestId('variable-chart-node').getAttribute('data-node') ?? '').split('|');
    return { tech, stage };
  };
  const chartless = () => within(box).queryByTestId('variable-chart-chartless')?.textContent ?? null;
  return { box, select, groups, options, step, chartless, pick: (value: string) => fireEvent.change(select, { target: { value } }) };
}
async function openFold(id: string): Promise<HTMLElement> {
  const d = document.getElementById(`fold-${id}`) as HTMLElement;
  fireEvent.click(d.querySelector('summary') as HTMLElement);
  await waitFor(() => expect(document.getElementById(`fold-${id}`)?.hasAttribute('open')).toBe(true));
  return document.getElementById(`fold-${id}`) as HTMLElement;
}
/** The composite scores: never an option. */
const COMPOSITES = ['Supply-chain vulnerability', 'Strategic Industrial Value', 'Industrial Opportunity', 'Market prize', 'Competitive Position', 'Deployment need'];

/**
 * A picker offers exactly the indicators of its families with a chart at its step, grouped by family in the
 * composite's order; no option is a composite or one of the table's column headers (the family scores).
 */
function expectIndicators(id: string, analysis: VariableAnalysis, table: HTMLElement) {
  const p = picker(id);
  const families = fenceFamilies(analysis, analysis === 'prize' ? { siv: true, prizeLabel: 'Strategic Industrial Value' } : {});
  const want: [string, string[]][] = [];
  for (const o of variableOptions(families, p.step())) {
    const last = want[want.length - 1];
    if (last && last[0] === o.group) last[1].push(o.label);
    else want.push([o.group, [o.label]]);
  }
  expect(p.groups(), id).toEqual(want);
  expect(p.options().length, id).toBeGreaterThan(0);
  const cols = new Set(heads(table));
  for (const o of p.options()) {
    expect(cols.has(o.text), `${id}: "${o.text}" is a column of the table`).toBe(false);
    expect(COMPOSITES, id).not.toContain(o.text);
    expect(o.value, id).toMatch(/^[a-z-]+:\d+$/);
  }
  // every group names a family of the composite
  for (const [g] of p.groups()) expect(families.map(familyGroup), id).toContain(g);
  return p;
}

describe.skipIf(!haveFixture())('Country page: Show a variable (the Sector chart of an indicator)', () => {
  it('each picker offers indicators grouped by family, never a composite or family score; no Sector data load before a pick', async () => {
    await usa();
    // the Medium fence's Prize: at its first row, Batteries Final Assembly, the market prize's parts that chart there
    const prize = expectIndicators('medium-prize', 'prize', within(sec('medium-prize')).getByTestId('medium-prize-table'));
    expect(prize.groups()).toEqual([
      ['Home-market growth (market prize)', ['Deployment investment']],
      ['Investment momentum (market prize)', ['Annual clean-technology investment']],
    ]);
    // the families it cannot offer, and where to see them
    expect(prize.chartless()).toBe('No Sector chart for World demand growth (none at this step), Industrial commons and Future option: select a row to see them in its details.');
    // Competitive Position: trade momentum in the monthly trade card
    const pos = expectIndicators('medium-position', 'position', within(sec('medium-position')).getByTestId('medium-position-table'));
    expect(pos.chartless()).toMatch(/Trade momentum.*: select a row to see (it|them) in its details; for trade momentum, see also the monthly trade card in the Position section\.$/);
    expect(pos.groups().map(([g]) => g)).toEqual(expect.arrayContaining(['Export position', 'Current investment']));
    // Accelerate: Batteries at Deployment & Use, the variable renewables and deployment investment
    const acc = expectIndicators('medium-accelerate', 'deployment', within(sec('medium-accelerate')).getByTestId('medium-accelerate-table'));
    expect(acc.step()).toEqual({ tech: 'Batteries', stage: 'Deployment' });
    expect(acc.groups()).toEqual([
      ['Demand adequacy', ['Solar and wind generation']],
      ['System need', ['Solar and wind generation']],
      ['Deployment trend', ['Deployment investment']],
    ]);
    expect(acc.chartless()).toBe('No Sector chart for Target / climate gap (none at this step) and Climate-aligned (alternative): select a row to see them in its details.');
    // the High fence's drop-down: the four need families at Batteries Refining
    const high = await openFold('high-all');
    const hp = expectIndicators('high', 'need', within(high).getByTestId('high-table'));
    expect(hp.groups()).toEqual([
      ['Import dependence', ['Net trade in the step’s goods', 'Imports of the step’s goods']],
      ['Supplier concentration', ['Who supplies the imports']],
      ['Risky supplier exposure', ['Imports from China and the opposite bloc']],
      ['Lack of trusted alternatives', ['World exports by supplier group', 'Refined production, share of world']],
    ]);
    expect(hp.chartless()).toBeNull();
    // the Low fence's drop-downs: the same families, at their own first rows
    const low = await openFold('low-supply-all');
    expectIndicators('low-prize', 'prize', within(low).getByTestId('low-prize-table'));
    expectIndicators('low-position', 'position', within(low).getByTestId('low-position-table'));
    const lowDeploy = await openFold('low-deploy-all');
    expectIndicators('low-deploy', 'deployment', within(lowDeploy).getByTestId('low-deploy-table'));
    // the step line: the table's first row until a row is selected
    expect(within(prize.box).getByTestId('variable-chart-node').textContent).toBe('For Batteries · Final Assembly & Equipment, the first row; select a row to show another step.');
    // nothing of the Sector data has been fetched: the first view does not grow
    expect(sectorCalls()).toEqual([]);
  }, 120_000);

  it('a pick renders one indicator: Annual clean-technology investment opens the explore map of investment.annual, with Open in Sectors', async () => {
    await usa();
    const prize = picker('medium-prize');
    prize.pick('investment:0');
    const ex = await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    expect([ex.getAttribute('data-m'), ex.getAttribute('data-tech'), ex.getAttribute('data-stage'), ex.getAttribute('data-c'), ex.getAttribute('data-part')]).toEqual([
      'investment.annual', 'Batteries', 'Assembly', 'USA', 'map',
    ]);
    // one indicator: one explorer, no second measure select
    expect(within(prize.box).getAllByTestId('variable-chart-explore')).toHaveLength(1);
    expect(within(prize.box).queryByLabelText('Sector measure')).toBeNull();
    expect(prize.select.value).toBe('investment:0');
    expect(within(ex).getByTestId('variable-chart-open').getAttribute('href')).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Batteries&stage=Assembly');
    expect(within(ex).getByTestId('variable-chart-basis').textContent).toMatch(/^Same source as Investment momentum\. Annual clean-technology investment \(Clean Investment Monitor\)/);
    // the map card, the economy highlighted; only the map (the ranking is another chart)
    await waitFor(() => expect(within(ex).getByLabelText(/^Map of Annual investment/)).toBeTruthy(), { timeout: 30_000 });
    expect(within(ex).queryByLabelText(/Annual investment, ranked/)).toBeNull();
    // the Sector data loaded on the pick: the manifest, sector-meta, sector-facts
    expect(sectorCalls()[0]).toBe('manifest.json');
    expect(sectorCalls().some((c) => c.startsWith('sector-meta'))).toBe(true);
    expect(sectorCalls().some((c) => c.startsWith('sector-facts'))).toBe(true);
    // the Chart control stays: the ranking, then the table
    fireEvent.change(within(ex).getByLabelText('Chart'), { target: { value: 'bars' } });
    await waitFor(() => expect(within(ex).getByLabelText(/Annual investment, ranked/)).toBeTruthy());
    expect(within(ex).queryByLabelText(/^Map of Annual investment/)).toBeNull();
    fireEvent.change(within(ex).getByLabelText('Chart'), { target: { value: 'table' } });
    await waitFor(() => expect(ex.getAttribute('data-part')).toBe('table'));
    expect(within(ex).getByRole('table')).toBeTruthy();
    // the page's own URL never changed
    expect(window.location.search).toBe('');
    // another indicator starts on its own measure, item and chart: Deployment investment reads the deployment at home
    prize.pick('home:5');
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-item-note').textContent).toMatch(/^The Sector data have no annual investment for Batteries · Deployment & Use; showing /), { timeout: 30_000 });
    const home = within(prize.box).getByTestId('variable-chart-explore');
    expect([home.getAttribute('data-m'), home.getAttribute('data-stage'), home.getAttribute('data-part')]).toEqual(['investment.annual', 'Deployment', 'map']);
    expect(within(home).getByTestId('variable-chart-basis').textContent).toMatch(/^A related Sector measure, not the data Home-market growth reads\. Deployment investment/);
    expect(within(prize.box).getAllByTestId('variable-chart-explore')).toHaveLength(1);
  }, 120_000);

  it('the chart follows the selected row; a pick the new row does not offer is said in one line and the picker stays usable', async () => {
    await usa();
    const prize = picker('medium-prize');
    prize.pick('investment:0');
    await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    // select Solar Final Assembly in the table (the drawer opens; the chart moves to the step)
    const table = within(sec('medium-prize')).getByTestId('medium-prize-table');
    fireEvent.click(rowOf(table, 'Solar · Final Assembly & Equipment'));
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-node').textContent).toBe('For Solar · Final Assembly & Equipment, the selected row.'), { timeout: 30_000 });
    const ex = await within(prize.box).findByTestId('variable-chart-explore', {}, { timeout: 30_000 });
    // the fixture has no Solar Assembly investment: the note says so and names what is shown instead
    const note = within(ex).getByTestId('variable-chart-item-note');
    expect(note.getAttribute('data-covered')).toBe('no');
    expect(note.textContent).toMatch(/^The Sector data have no annual investment for Solar · Final Assembly & Equipment; showing Solar — Deployment\.$/);
    expect(`${ex.getAttribute('data-tech')}|${ex.getAttribute('data-stage')}`).toBe('Solar|Deployment');
    // the Open in Sectors link opens what is shown
    expect(within(ex).getByTestId('variable-chart-open').getAttribute('href')).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Solar&stage=Assembly');
    // the Solar step offers its own indicators: world demand growth and the technology's generation at home
    expect(prize.groups()[0]).toEqual(['World demand growth (market prize)', ['World energy supply outlook']]);
    expect(prize.chartless()).toBe('No Sector chart for Industrial commons and Future option: the selected row’s details show them.');
    prize.pick('home:0');
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-explore').getAttribute('data-m')).toBe('production.generation'), { timeout: 30_000 });
    // back to the Batteries row: electricity generation has no chart for Batteries, said in one line, nothing shown
    fireEvent.click(rowOf(table, 'Batteries · Final Assembly & Equipment'));
    const stale = await within(prize.box).findByTestId('variable-chart-stale', {}, { timeout: 30_000 });
    expect(stale.textContent).toBe('Electricity generation from the technology, under Home-market growth (market prize), has no Sector chart for Batteries · Final Assembly & Equipment; choose another variable.');
    expect(within(prize.box).queryByTestId('variable-chart-explore')).toBeNull();
    expect(prize.select.value).toBe('');
    // the picker stays usable: another pick shows its chart and the line goes
    prize.pick('investment:0');
    await waitFor(() => expect(within(prize.box).getByTestId('variable-chart-explore').getAttribute('data-m')).toBe('investment.annual'), { timeout: 30_000 });
    expect(within(prize.box).queryByTestId('variable-chart-stale')).toBeNull();
  }, 120_000);

  it('High fence: Who supplies the imports opens the economy’s import partners (trade.flow) for the step; each trade indicator is its own option', async () => {
    await usa();
    const fold = await openFold('high-all');
    const high = picker('high');
    high.pick('concentration:0');
    // the top step, Batteries Refining, has no bilateral flows in the fixture: said, and the shown basket named
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
    // import dependence at a Refining step: its two trade indicators are two options of its group, each its own chart
    expect(high.groups()[0]).toEqual(['Import dependence', ['Net trade in the step’s goods', 'Imports of the step’s goods']]);
    high.pick('import:0');
    await waitFor(() => expect(within(high.box).getByTestId('variable-chart-explore').getAttribute('data-m')).toBe('trade.net'), { timeout: 30_000 });
    expect(within(high.box).queryByLabelText('Sector measure')).toBeNull();
    high.pick('import:1');
    await waitFor(() => expect(within(high.box).getByTestId('variable-chart-explore').getAttribute('data-m')).toBe('trade.imports'), { timeout: 30_000 });
    expect(within(high.box).getAllByTestId('variable-chart-explore')).toHaveLength(1);
  }, 120_000);

  it('the chartless families are named, never offered, and load no Sector data', async () => {
    await usa();
    const prize = picker('medium-prize');
    const texts = prize.options().map((o) => o.text);
    for (const t of ['Industrial commons', 'Future option', 'World demand growth', 'Strategic Industrial Value', 'Market prize']) expect(texts).not.toContain(t);
    // the composite and family ids of the old picker are not values any more
    for (const v of ['commons', 'option', 'opportunity', 'market', 'need', 'investment', 'node-prize']) expect(prize.options().map((o) => o.value)).not.toContain(v);
    const pos = picker('medium-position');
    expect(pos.options().map((o) => o.text)).not.toContain('Trade momentum');
    expect(pos.groups().map(([g]) => g)).not.toContain('Trade momentum');
    const acc = picker('medium-accelerate');
    expect(acc.groups().map(([g]) => g)).not.toContain('Target / climate gap');
    expect(acc.groups().map(([g]) => g)).not.toContain('Climate-aligned (alternative)');
    expect(within(acc.box).queryByTestId('variable-chart-explore')).toBeNull();
    expect(sectorCalls()).toEqual([]);
  }, 120_000);
});
