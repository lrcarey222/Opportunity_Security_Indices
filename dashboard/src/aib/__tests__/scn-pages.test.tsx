// Page work A of the scenario toggle (2026-10-19), in the app, on the synthetic fixture: the Overview leads
// with the three scenarios side by side (the toggled one current, a table twin) and its class tiles,
// lists and question cards follow the toggle; the Managed Interdependence explainer prints the toggle's
// numbers (the same as the Overview's); the Sectoral Compacts landing orders its cards by the scenario's
// supply, gives each a three-way mini bar and a table twin, solves the coalition line from the scenario's
// file, and groups the nodes by their class under it; a compact's page follows the toggle (the coalition,
// tiers and roles of the scenario's file, the reason when there is no coalition, the file's failure), shows
// the compact and every node under the three scenarios and its suppliers today; an older bundle keeps the
// Decoupling-only pages.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import type { AibDoc, AibManagedScenarioDoc } from '../contract.ts';
import { diversifyTotals } from '../derive/diversify.ts';
import { compactTitle, fmtUsd } from '../derive/format.ts';
import { overviewRows } from '../derive/scenarioPages.ts';
import { nodeScenario } from '../derive/scenarios.ts';
import { resetLastCompact } from '../nav.ts';
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

/** Always the synthetic fixture (these tests pin its figures). */
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
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);
const current = (el: HTMLElement) => [...el.querySelectorAll('[data-scenario][aria-current="true"]')].map((x) => x.getAttribute('data-scenario'));
const order = (el: HTMLElement) => [...el.querySelectorAll('li[data-scenario]')].map((x) => x.getAttribute('data-scenario'));

const doc = readDoc(SYNTHETIC_DIR);
const md = readScenarioDoc('managed', SYNTHETIC_DIR)!;
const titleOf = (c: AibDoc['compacts'][number], d: AibDoc = doc) => compactTitle(c, d.nodes);
const names = doc.countries as Record<string, string>;
const three = overviewRows(doc.nodes, doc.meta);

describe('the Overview leads with the three scenarios', () => {
  it('Decoupling: the three side by side in order, Decoupling current, values in text and a table twin; the rest as before', async () => {
    renderAt('/aib');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    const region = screen.getByRole('region', { name: 'Three scenarios compared' });
    // it comes first
    expect(screen.getAllByRole('heading', { level: 2 })[0].textContent).toBe('Three scenarios compared');
    expect(within(region).getByTestId('overview-scenario-words').textContent).toMatch(/^BaselineTrade as it is today \(2024\), before any shock\.Managed Interdependence/);
    const cov = within(region).getByRole('figure', { name: 'Coverage of the allied demand under each scenario' });
    expect(order(cov)).toEqual(['baseline', 'managed', 'decoupling']);
    expect(current(cov)).toEqual(['decoupling']);
    for (const name of ['Allied demand affected under each scenario', 'Left for new capacity under each scenario', 'Lost or shifted supply by class under each scenario']) {
      expect(current(within(region).getByRole('figure', { name }))).toEqual(['decoupling']);
    }
    const gap = within(region).getByRole('figure', { name: 'Left for new capacity under each scenario' });
    expect(gap.querySelector('[data-scenario="managed"]')!.textContent).toContain(fmtUsd(three[1].residualUsd));
    expect(gap.querySelector('[data-scenario="decoupling"]')!.textContent).toContain(fmtUsd(three[2].residualUsd));
    fireEvent.click(within(region).getByRole('button', { name: 'Table' }));
    const table = within(region).getByRole('table');
    expect(within(table).getAllByRole('row')).toHaveLength(4);
    expect(within(region).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    expect(screen.getByTestId('overview-run').textContent).toMatch(/^Shown: Decoupling \(/);
    expect(screen.queryByTestId('overview-unchanged')).toBeNull();
    noAcronyms();
  });

  it('Managed Interdependence: current in every comparison; the class tiles, the lists and the question cards follow it', async () => {
    renderAt('/aib?scn=managed');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    const region = screen.getByRole('region', { name: 'Three scenarios compared' });
    expect(current(within(region).getByRole('figure', { name: 'Coverage of the allied demand under each scenario' }))).toEqual(['managed']);
    expect(screen.getByTestId('overview-run').textContent).toMatch(/^Shown: Managed Interdependence \(/);
    // the tiles count the nodes by their class under the scenario
    const rows = doc.nodes.map((n) => nodeScenario(n, 'managed'));
    const tiles = within(screen.getByRole('list', { name: 'Nodes by treatment' })).getAllByRole('listitem');
    ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION'].forEach((cls, i) => {
      const n = rows.filter((r) => r.ran && r.cls === cls).length;
      expect(tiles[i].textContent, cls).toContain(`${n} ${n === 1 ? 'node' : 'nodes'}`);
    });
    const unchanged = rows.filter((r) => !r.ran).length;
    expect(screen.getByTestId('overview-unchanged').textContent).toMatch(new RegExp(`^${unchanged} nodes are unchanged under Managed Interdependence`));
    const shift = screen.getByRole('list', { name: 'Nodes with the largest share of supply to shift' });
    expect(within(shift).getAllByRole('listitem')).toHaveLength(rows.filter((r) => r.ran).length);
    // every link carries the choice
    const hrefs = within(screen.getByRole('region', { name: 'Who coordinates, and how' })).getAllByRole('link').map((a) => a.getAttribute('href'));
    expect(hrefs).toEqual(['/aib/partnerships?scn=managed', '/compacts?scn=managed', '/aib/roles?scn=managed']);
    expect(within(screen.getByTestId('diversify-pointer')).getByRole('link').getAttribute('href')).toBe('/aib/diversification?scn=managed');
    // the question cards count the scenario's relationships once its file is loaded
    await waitFor(() => expect(source.calls.some((f) => f.startsWith('scenario-managed'))).toBe(true));
    const nTrade = new Set(md.compacts.flatMap((c) => c.trade.filter((e) => (e.usd ?? 0) > 0 && e.from !== e.to).map((e) => `${e.from}|${e.to}|${e.relationship}`))).size;
    await waitFor(() => expect(screen.getByRole('region', { name: 'Who coordinates, and how' }).textContent).toContain(`${nTrade} coordinated trade relationship`));
    noAcronyms();
  });

  it("the Managed Interdependence explainer prints the toggle's numbers: the same as the Overview's", async () => {
    const t = diversifyTotals(doc.nodes);
    const mi = three[1];
    expect(fmtUsd(mi.lostUsd)).toBe(fmtUsd(t.shiftUsd));
    renderAt('/aib/diversification?scn=managed');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    const tiles = within(screen.getByRole('list', { name: 'Managed Interdependence totals' })).getAllByRole('listitem');
    expect(tiles[1].textContent).toContain(fmtUsd(mi.lostUsd));
    expect(tiles[2].textContent).toContain(fmtUsd(mi.restoredUsd));
    expect(tiles[3].textContent).toContain(fmtUsd(mi.residualUsd));
    const words = screen.getByTestId('explainer-toggle-numbers').textContent ?? '';
    for (const v of [mi.lostUsd, mi.affectedDemandUsd, mi.restoredUsd, mi.residualUsd]) expect(words).toContain(fmtUsd(v));
    expect(current(screen.getByTestId('explainer-three-way'))).toEqual(['managed']);
    expect(screen.queryByTestId('explainer-note')).toBeNull();
    noAcronyms();
    cleanup();
    // under Decoupling it still explains Managed Interdependence, and says so
    renderAt('/aib/diversification');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    expect(screen.getByTestId('explainer-note').textContent).toMatch(/explains Managed Interdependence whichever scenario is chosen above/);
  });
});

describe('the Sectoral Compacts landing follows the toggle', () => {
  it('cards ordered by the supply the scenario shifts, a three-way mini bar each, the coalition from its file, a table twin of every compact', async () => {
    const last = doc.compacts[doc.compacts.length - 1];
    useSource((d) => {
      d.compacts.find((c) => c.id === last.id)!.managed!.lostUsd = 1e12;
    });
    renderAt('/compacts?scn=managed');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(screen.getByTestId('run-settings').textContent).toMatch(/^Shown: Managed Interdependence \(no supplier removed; caps above the benchmark\); supply assumption: reallocation, production-constrained supply; trade in 2024\./);
    // no shock semantics under Managed Interdependence: it removes no supplier; coverage is served after the cap
    expect(screen.getByTestId('run-settings').textContent).not.toMatch(/semantics|after the shock/);
    const region = screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` });
    const cards = within(region).getAllByRole('article');
    expect(cards[0].getAttribute('data-compact')).toBe(last.id);
    for (const card of cards) {
      const mini = within(card).getByTestId('scenario-mini');
      expect(order(mini)).toEqual(['baseline', 'managed', 'decoupling']);
      expect(current(mini)).toEqual(['managed']);
    }
    // the coalition line of the first card, once the scenario's file is loaded
    const m = md.compacts.find((c) => c.id === last.id)!;
    await waitFor(() => expect(within(cards[0]).getByText(new RegExp(`· ${m.coalition!.members.length} members`))).toBeTruthy());
    expect(within(cards[0]).getByText(/^Managed Interdependence shifts /)).toBeTruthy();
    // the table twin: every compact under the three scenarios
    const card = within(region).getByRole('region', { name: 'Every compact under the three scenarios' });
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    const table = within(card).getByRole('table');
    expect(within(table).getAllByRole('row')).toHaveLength(doc.compacts.length + 1);
    // (the sorted column's header ends with its sort mark): sorted by the supply the scenario shifts
    const shifted = within(table).getAllByRole('columnheader').find((h) => (h.textContent ?? '').startsWith('Shifted: Managed Interdependence'));
    expect(shifted?.getAttribute('aria-sort')).toBe('descending');
    noAcronyms();
  });

  it('the nodes by their class under the scenario, the unchanged ones apart; the map places only the nodes it runs on', async () => {
    renderAt('/compacts?scn=managed#nodes');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const nodes = screen.getByRole('region', { name: 'How every sub-sector is classified' });
    const group = nodes.querySelector('[data-group="UNCHANGED"]') as HTMLElement;
    expect(within(group).getByRole('heading', { level: 4 }).textContent).toMatch(/^Unchanged under Managed Interdependence \(\d+\)$/);
    const unchanged = doc.nodes.filter((n) => n.inStrategicScope !== false && !nodeScenario(n, 'managed').ran);
    expect(within(group).getAllByRole('listitem').filter((li) => li.className.includes('hairline'))).toHaveLength(unchanged.length);
    const map = screen.getByRole('region', { name: 'Strategic map of every node' });
    expect(map.textContent).toContain(`${unchanged.length} nodes unchanged under Managed Interdependence are not placed`);
    noAcronyms();
  });

  it('under Decoupling the cards keep the export order; the node chips of the nodes outside a compact show their three scenarios', async () => {
    renderAt('/compacts#nodes');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const cards = within(screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` })).getAllByRole('article');
    expect(cards.map((c) => c.getAttribute('data-compact'))).toEqual(doc.compacts.map((c) => c.id));
    for (const card of cards) expect(current(within(card).getByTestId('scenario-mini'))).toEqual(['decoupling']);
    const outside = doc.nodes.filter((n) => !n.compact && n.inStrategicScope !== false && (n.variants !== undefined || n.growth !== undefined || n.diversify !== undefined));
    expect(screen.getAllByTestId('node-scenarios')).toHaveLength(outside.length);
  });
});


describe("a compact's page follows the toggle", () => {
  const c = doc.compacts[0];
  const m = md.compacts.find((x) => x.id === c.id)!;
  const nodesOf = (x: AibDoc['compacts'][number]) => x.nodes.map((id) => doc.nodes.find((n) => n.id === id)!);

  it('Managed Interdependence: the coalition, tiers and roles of its file; the compact and each node under the three scenarios; suppliers today', async () => {
    renderAt(`/compacts/${c.id}?scn=managed`);
    expect(await h1(titleOf(c))).toBeTruthy();
    // (the card is drawn anew once the scenario's file is loaded: query it again)
    const coalitionCard = () => screen.getByRole('region', { name: 'The coalition and its tiers' });
    await waitFor(() => expect(within(coalitionCard()).getByText(new RegExp(`^Exact minimum coalition: ${m.coalition!.members.length} economies\\.`))).toBeTruthy(), { timeout: 10_000 });
    expect(coalitionCard().querySelector('[data-scenario-tag="managed"]')).toBeTruthy();
    // the roles of the scenario's coalition; the Country profile links carry the choice
    const roles = screen.getByRole('region', { name: 'Economies and their roles' });
    expect(within(roles).getAllByRole('rowheader')).toHaveLength(new Set(m.roles.map((r) => r.iso3)).size);
    expect(within(roles).getAllByRole('link').every((a) => /^\/countries\/[A-Z]{3}\?scn=managed#aib-role$/.test(a.getAttribute('href') ?? ''))).toBe(true);
    // the lede and the case for coordination under the scenario
    expect(screen.getByText(/Under Managed Interdependence \(no supplier is removed; suppliers above a cap in the allied market lose the excess\), .* has to shift\.$/)).toBeTruthy();
    expect(screen.getByTestId('why-answer').textContent).toMatch(/^Managed Interdependence shifts \d+% of the compact's demand away from capped suppliers/);
    expect(within(screen.getByRole('region', { name: 'The case for coordination' })).getByText('Share shifted')).toBeTruthy();
    // the compact under the three scenarios
    const three = screen.getByRole('region', { name: 'Demand served under each scenario' });
    expect(order(three)).toEqual(['baseline', 'managed', 'decoupling']);
    expect(current(three)).toEqual(['managed']);
    // the external dependency: no supplier removed, the capped ones named; the suppliers today from compact.baseline
    const capped = [...new Set(nodesOf(c).filter((n) => nodeScenario(n, 'managed').ran).flatMap((n) => n.diversify!.capped))].map((x) => names[x]);
    expect(screen.getByTestId('external-dependency').textContent).toBe(`Managed Interdependence removes no supplier; it caps ${capped.join(' and ')} in the allied market`);
    const today = screen.getByTestId('suppliers-today').textContent ?? '';
    for (const [iso] of c.baseline!.suppliers) expect(today).toContain(names[iso]);
    // the steps start from the cap
    const steps = screen.getByRole('region', { name: 'Demand served at each step' });
    fireEvent.click(within(steps).getByRole('button', { name: 'Table' }));
    expect(within(within(steps).getByRole('table')).getAllByRole('row')[1].textContent).toMatch(/^Cap \(static\)/);
    // each node: its three scenarios, the toggled one current; a node the scenario leaves unchanged says why
    const card = screen.getByRole('region', { name: "Each node's coverage and treatment" });
    for (const n of nodesOf(c)) {
      const li = card.querySelector(`li[data-node="${n.id}"]`) as HTMLElement;
      const rows = within(li).getByTestId('node-scenarios');
      expect(order(rows), n.id).toEqual(['baseline', 'managed', 'decoupling']);
      expect(current(rows), n.id).toEqual(['managed']);
      const r = nodeScenario(n, 'managed');
      if (!r.ran) {
        expect(within(li).getByText('Unchanged')).toBeTruthy();
        expect(within(li).getByTestId('node-figures').textContent).toMatch(/^Under Managed Interdependence: unchanged: /);
      } else expect(within(li).getByTestId('node-figures').textContent).toMatch(/^Share shifted /);
    }
    // the frontier is Decoupling's
    expect(screen.getByRole('region', { name: 'Specialization and resilience by supplier limit' }).querySelector('[data-scenario-tag="decoupling"]')).toBeTruthy();
    noAcronyms();
  });

  it('Decoupling: the compact and every node under the three scenarios, Decoupling current; the node blocks leave out what the rows show', async () => {
    renderAt(`/compacts/${c.id}`);
    expect(await h1(titleOf(c))).toBeTruthy();
    expect(current(screen.getByRole('region', { name: 'Demand served under each scenario' }))).toEqual(['decoupling']);
    expect(screen.getAllByTestId('node-scenarios')).toHaveLength(c.nodes.length);
    for (const b of screen.getAllByTestId('diversify-block')) expect(b.textContent).not.toMatch(/With the cap holding/);
    expect(screen.getByTestId('external-dependency').textContent).toMatch(/^The supplier the headline shock removes: /);
    // the scenario's file is never fetched under Decoupling
    expect(source.calls.filter((f) => f.startsWith('scenario-managed'))).toEqual([]);
  });

  it('a compact the scenario has no coalition for: the reason, wherever the coalition would be', async () => {
    const id = doc.compacts[doc.compacts.length - 1].id;
    const reason = 'no node of this compact is more concentrated than its benchmark';
    useSource(undefined, {
      editScenario: (d) => {
        if (d.scenario !== 'managed') return;
        const x = (d as AibManagedScenarioDoc).compacts.find((y) => y.id === id)!;
        Object.assign(x, { coalition: null, metrics: null, coalitionReason: reason, roles: [], path: [], trade: [], investment: [] });
      },
    });
    renderAt(`/compacts/${id}?scn=managed`);
    expect(await h1(titleOf(doc.compacts.find((x) => x.id === id)!))).toBeTruthy();
    const no = await screen.findAllByTestId('scenario-no-coalition', {}, { timeout: 10_000 });
    expect(no).toHaveLength(4);
    for (const x of no) expect(x.textContent).toBe(`No coalition under Managed Interdependence: ${reason}`);
    noAcronyms();
  });

  it("the scenario's file fails: a notice with Retry, the comparisons still drawn", async () => {
    useSource(undefined, { editScenario: (d) => void ((d as unknown as Record<string, unknown>).schema = 'x') });
    renderAt(`/compacts/${c.id}?scn=managed`);
    expect(await h1(titleOf(c))).toBeTruthy();
    const notices = await screen.findAllByText('Managed Interdependence could not be loaded', {}, { timeout: 10_000 });
    // the coalition, the roles, the coverage steps and the coalition path, each with its Retry
    expect(notices.length).toBe(4);
    for (const n of notices) expect(within(n.closest('[role="alert"]') as HTMLElement).getByRole('button', { name: 'Retry' })).toBeTruthy();
    expect(current(screen.getByRole('region', { name: 'Demand served under each scenario' }))).toEqual(['managed']);
  });
});

describe('an older bundle without the scenarios keeps the Decoupling pages', () => {
  it('no comparison on the Overview, the coverage strip on the cards, the node track and the full node block on a compact', async () => {
    useSource((d) => void stripScenarios(d), { noScenarioFiles: true });
    renderAt('/aib');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    expect(screen.queryByRole('region', { name: 'Three scenarios compared' })).toBeNull();
    expect(screen.getByTestId('overview-run').textContent).toMatch(/^Headline: /);
    cleanup();
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const card = within(screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` })).getAllByRole('article')[0];
    expect(within(card).getByRole('list', { name: `${titleOf(doc.compacts[0])}: demand served at each step` })).toBeTruthy();
    expect(screen.queryAllByTestId('scenario-mini')).toEqual([]);
    expect(screen.queryByRole('region', { name: 'Every compact under the three scenarios' })).toBeNull();
    cleanup();
    const c = doc.compacts[0];
    renderAt(`/compacts/${c.id}`);
    expect(await h1(titleOf(c))).toBeTruthy();
    expect(screen.queryByRole('region', { name: 'Demand served under each scenario' })).toBeNull();
    expect(screen.queryAllByTestId('node-scenarios')).toEqual([]);
    const ran = screen.getAllByTestId('diversify-block').find((b) => b.getAttribute('data-status') === 'diversified to the benchmark')!;
    expect(ran.textContent).toMatch(/With the cap holding/);
    expect(within(ran).getByTestId('demand-split')).toBeTruthy();
    noAcronyms();
  });
});
