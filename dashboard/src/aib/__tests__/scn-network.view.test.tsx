// Page work B (2026-10-19), the views: the Country roles page, a compact's partnership network and the
// investor -> host flows card follow the scenario toggle, with today's network (the Baseline file) as the
// reference. Decoupling: the headline network, the every-shock totals labelled as its supply shocks, "What
// changes from today" (who supplies whom; roles gained or lost, the removed supplier). Managed
// Interdependence: its scenario file (roles, compacts, flows), the capped suppliers, the toggle carried on
// every link; the loading, failed and missing states; an older bundle without the scenarios.

import { ROLES_PAGE_TITLE } from '../views/AibRolesView.tsx';
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { countryNamesOf, type AibDoc } from '../contract.ts';
import { NO_FILTER } from '../derive/partnerships.ts';
import { resetLastCompact } from '../nav.ts';
import { InvestmentFlowsCard } from '../views/InvestmentFlowsCard.tsx';
import { fixtureSource, readDoc, readScenarioDoc, stripScenarios, SYNTHETIC_DIR, type FixtureSourceOpts } from './fixture.ts';

const restore: (() => void)[] = [];
let source: AibSource & { calls: string[] };

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

// the synthetic fixture always (exact expectations), whatever AIB_DATA_DIR says
function useSource(edit?: (d: AibDoc) => void, opts?: FixtureSourceOpts): void {
  source = fixtureSource(SYNTHETIC_DIR, edit, opts);
  setAibStoreForTests(new AibStore(source));
}

beforeEach(() => useSource());

afterEach(() => {
  cleanup();
  setAibStoreForTests(null);
  resetLastLocations();
  resetLastCompact();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const main = () => document.querySelector('main') as HTMLElement;
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);
const region = (name: string | RegExp) => screen.findByRole('region', { name }, { timeout: 15_000 });

const doc = readDoc(SYNTHETIC_DIR);
const mi = readScenarioDoc('managed', SYNTHETIC_DIR)!;
const names = countryNamesOf(doc);

describe('the Country roles page under the toggle', () => {
  it('Decoupling: the headline network, its every-shock totals labelled; what changes from today: who supplies whom and the roles gained or lost', async () => {
    renderAt('/aib/roles?scn=decoupling');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    const note = within(main()).getAllByTestId('modelled-note')[0].textContent ?? '';
    expect(note).toContain('Zembla supply removed; supply assumption: reallocation');
    expect(note).not.toContain('every');
    expect(screen.getByTestId('every-shock-note').textContent).toBe(
      "The every-shock totals sum Major-supplier disruption's three supply shocks (Zembla supply removed; Zembla supply halved; Largest supplier of the sub-sector removed), and the diversification scenario, which removes no supplier. They appear only in the table and in each role's details; every role is read on Major-supplier disruption alone.",
    );
    expect(within(main()).getByText('AIB analysis · Major-supplier disruption')).toBeTruthy();
    // EEE hosts new capacity only under another shock: no host role under Decoupling
    const eee = document.getElementById('role-EEE') as HTMLElement;
    expect(eee.querySelectorAll('td')[2].textContent).toBe('·');
    // Avalon buys from three economies under Decoupling (four across every shock, in the cell's words)
    const aaaBuyer = (document.getElementById('role-AAA') as HTMLElement).querySelectorAll('td')[4];
    expect(aaaBuyer.textContent).toContain('3 suppliers');
    expect(aaaBuyer.querySelector('[title]')!.getAttribute('title')).toMatch(/with 3 suppliers; across Major-supplier disruption's three supply shocks and the diversification scenario: .*, 4 suppliers \(/);
    // today against the scenario
    const today = await screen.findByTestId('roles-today');
    expect(within(today).getByText('Today · observed')).toBeTruthy();
    const pairs = await region(/^Who supplies whom: today and under Major-supplier disruption/);
    expect(within(pairs).getByRole('list', { name: 'Largest supply relationships today' })).toBeTruthy();
    const scn = within(pairs).getByRole('list', { name: 'Largest relationships under Major-supplier disruption' });
    expect(within(scn).getAllByRole('listitem')[0].textContent).toMatch(/^Carpania → Avalon/);
    expect(scn.textContent).toContain('not a tie today');
    // one economy
    fireEvent.change(within(pairs).getByRole('combobox', { name: 'Economy' }), { target: { value: 'DDD' } });
    await waitFor(() => expect(within(within(pairs).getByRole('list', { name: 'Largest relationships under Major-supplier disruption' })).getAllByRole('listitem').every((li) => li.textContent!.includes('Dorvalia'))).toBe(true));
    const roles = await region('Roles gained or lost');
    const lost = within(roles).getByRole('region', { name: 'Roles lost or reduced under Major-supplier disruption' });
    expect(within(lost).getByText(/Stop supplying the allied market/)).toBeTruthy();
    expect(within(lost).getAllByRole('link', { name: 'Zembla' })[0].getAttribute('href')).toBe('/countries/ZZZ?scn=decoupling#aib-role');
    const gained = within(roles).getByRole('region', { name: 'Roles gained under Major-supplier disruption' });
    expect(within(gained).getByText(/New suppliers/).parentElement!.textContent).toContain('Dorvalia');
    noAcronyms();
    // three cards with a table twin and a CSV
    const tables = within(main()).getAllByRole('button', { name: 'Table' });
    expect(tables).toHaveLength(3);
    for (const b of tables) fireEvent.click(b);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(3);
    noAcronyms();
  });

  it('Managed Interdependence: the roles from its file, no every-shock totals, the capped suppliers; links carry the toggle', async () => {
    renderAt('/aib/roles');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    await waitFor(() => expect(document.getElementById('role-FFF')).toBeTruthy(), { timeout: 15_000 });
    const note = within(main()).getAllByTestId('modelled-note')[0].textContent ?? '';
    expect(note).toContain('no supplier removed; supply above the concentration cap shifts');
    expect(note).not.toContain('every-shock');
    expect(screen.queryByTestId('every-shock-note')).toBeNull();
    expect(within(main()).getByText('AIB analysis · Managed Interdependence')).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'How roles are read' }).nextElementSibling!.textContent).toContain('removes no supplier');
    // the matrix: a compact link and a Country profile link carry the toggle
    const ccc = document.getElementById('role-CCC') as HTMLElement;
    expect(within(ccc).getAllByRole('link').map((a) => a.getAttribute('href'))).toContain('/countries/CCC#aib-role');
    const fff = document.getElementById('role-FFF') as HTMLElement;
    expect(within(fff).getAllByRole('link').map((a) => a.getAttribute('href'))).toContain('/compacts/AIB-C03');
    const roles = await region('Roles gained or lost');
    const lost = within(roles).getByRole('region', { name: 'Roles lost or reduced under Managed Interdependence' });
    expect(within(lost).getByText(/Supply less: held to the cap/).parentElement!.textContent).toMatch(/Zembla in 2 steps/);
    expect(within(lost).queryByText(/Stop supplying/)).toBeNull();
    // the matrix's table: no every-shock columns under Managed Interdependence
    const matrix = screen.getByRole('region', { name: 'Roles by economy' });
    fireEvent.click(within(matrix).getByRole('button', { name: 'Table' }));
    const heads = within(matrix).getAllByRole('columnheader').map((h) => h.textContent ?? '');
    expect(heads.some((h) => h.startsWith('Resource / Supplier: Managed Interdependence'))).toBe(true);
    expect(heads.some((h) => /supply shocks/.test(h))).toBe(false);
    noAcronyms();
  });

  it('a broken Managed Interdependence file: an error with Retry in place of the roles', async () => {
    useSource(undefined, { editScenario: (d, id) => void (id === 'managed' && ((d as unknown as Record<string, unknown>).schema = 'x')) });
    renderAt('/aib/roles');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    expect(await screen.findByText('The Managed Interdependence results could not be loaded', undefined, { timeout: 15_000 })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Retry' })).toBeTruthy();
    expect(screen.queryByRole('region', { name: 'Roles by economy' })).toBeNull();
  });

  it('a bundle whose manifest names no scenario file: the missing states, never a crash', async () => {
    useSource(undefined, { noScenarioFiles: true });
    renderAt('/aib/roles');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    expect(await screen.findByText(/This build has no Managed Interdependence results, so the country roles cannot be shown/, undefined, { timeout: 15_000 })).toBeTruthy();
    cleanup();
    renderAt('/aib/roles?scn=decoupling');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    expect(await screen.findByText(/This build has no Today's network results, so today's network cannot be shown/, undefined, { timeout: 15_000 })).toBeTruthy();
    expect(screen.getByRole('region', { name: 'Roles by economy' })).toBeTruthy();
  });

  it('an older bundle without the scenarios: no comparison with today, the facts tag, the headline words', async () => {
    useSource((d) => stripScenarios(d), { noScenarioFiles: true });
    renderAt('/aib/roles');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    expect(screen.queryByTestId('roles-today')).toBeNull();
    expect(within(main()).getByText('Observed data and analysis')).toBeTruthy();
    expect(within(main()).getAllByTestId('modelled-note')[0].textContent).toContain('"every shock" sums');
    expect(screen.queryByTestId('every-shock-note')).toBeNull();
    expect(source.calls.filter((f) => f.startsWith('scenario-'))).toEqual([]);
  });
});

describe('a compact\'s partnership network under the toggle', () => {
  it('Decoupling: today\'s suppliers beside the added ones, the removed supplier marked', async () => {
    renderAt('/compacts/AIB-C01?scn=decoupling');
    await h1(/./);
    const trade = await region('Trade relationships');
    const cmp = await within(trade).findByTestId('suppliers-compare');
    const todayList = within(cmp).getByRole('region', { name: /^Largest suppliers today/ });
    expect(todayList.textContent).toContain('Zembla');
    expect(todayList.textContent).toContain('its supply is removed under Major-supplier disruption');
    expect(within(cmp).getByRole('region', { name: /^Largest added suppliers under Major-supplier disruption/ })).toBeTruthy();
    noAcronyms();
  });

  it('Managed Interdependence: the compact as its file solves it, the capped supplier, its flows', async () => {
    renderAt('/compacts/AIB-C01');
    await h1(/./);
    const c = mi.compacts.find((x) => x.id === 'AIB-C01')!;
    const trade = await region('Trade relationships');
    await waitFor(() => expect(trade.textContent).toContain('under Managed Interdependence'), { timeout: 15_000 });
    expect(screen.getByText(`${c.trade.length} trade relationships, 2 investment flows`)).toBeTruthy();
    const cmp = within(trade).getByTestId('suppliers-compare');
    expect(within(cmp).getByRole('region', { name: /^Largest suppliers today/ }).textContent).toContain('held to its cap under Managed Interdependence in 1 step');
    fireEvent.click(within(trade).getByRole('button', { name: 'Table' }));
    expect(within(trade).getAllByRole('row').length - 1).toBe(c.trade.length);
    const inv = screen.getByRole('region', { name: 'Who invests where' });
    expect(within(inv).getByText('Managed Interdependence')).toBeTruthy();
    expect(within(within(inv).getByTestId('flow-list')).getAllByRole('listitem')).toHaveLength(2);
    expect(within(inv).getByTestId('modelled-note').textContent).toContain('no supplier removed; supply above the concentration cap shifts');
    noAcronyms();
  });
});

describe('the investor -> host flows card follows the toggle wherever it is embedded', () => {
  function Card() {
    return (
      <InvestmentFlowsCard doc={doc} meta={doc.meta} shockLabel="Zembla supply removed" filter={{ ...NO_FILTER }} scope="every economy" name={(iso) => names[iso] ?? iso} nodeLabel={(id) => id} csvName="test" />
    );
  }
  const renderCard = (path: string) => {
    const router = createMemoryRouter([{ path: '/x', element: <Card /> }], { initialEntries: [path] });
    render(<RouterProvider router={router} />);
  };

  it('the URL chooses: Decoupling the main document\'s flows, Managed Interdependence its file\'s (after loading)', async () => {
    renderCard('/x?scn=decoupling');
    const best = doc.investmentFlows!.pairs.filter((p) => p.rank === 1);
    const card = await region('Who invests where');
    expect(within(within(card).getByTestId('flow-list')).getAllByRole('listitem')).toHaveLength(Math.min(8, best.length));
    expect(within(card).getByText('Major-supplier disruption')).toBeTruthy();
    cleanup();
    renderCard('/x');
    const card2 = await region('Who invests where');
    await waitFor(() => expect(within(card2).getByTestId('flow-list')).toBeTruthy(), { timeout: 15_000 });
    expect(within(within(card2).getByTestId('flow-list')).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      expect.stringMatching(/^Avalon → Carpania/),
      expect.stringMatching(/^Borduria → Carpania/),
    ]);
    expect(card2.textContent).toContain('no such new capacity');
  });

  it('a failed file: the error and Retry in the card', async () => {
    useSource(undefined, { editScenario: (d, id) => void (id === 'managed' && ((d as unknown as Record<string, unknown>).schema = 'x')) });
    renderCard('/x');
    const card = await region('Who invests where');
    expect(await within(card).findByText('The Managed Interdependence results could not be loaded', undefined, { timeout: 15_000 })).toBeTruthy();
    expect(within(card).getByRole('button', { name: 'Retry' })).toBeTruthy();
  });
});
