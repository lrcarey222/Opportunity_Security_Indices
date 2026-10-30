// The Country profile's Partnerships section on the three lenses (2026-10-19; aib_scenarios_spec.md s.5 C
// and s.6), in the app, on the synthetic Country and AIB fixtures (the AIB economy AAA renamed USA in the
// main document and both scenario files): the four kinds (URL ptab), each with the cross-lens summary and
// three tagged panels; Today from the fact sheet (trade ties) and the ties chunk, OSI recommends from the
// fact sheet's partner scores, the AIB model under the scenario toggle (URL scn, the toggle in the panel):
// its coordinated trade, investment flows, BUILD nodes and compact coalitions follow Managed
// Interdependence from its file; the Role in the Allied Industrial Base follows the same key; a lens with
// no data at the focus says so in one line.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import type { AibDoc, AibScenarioDoc } from '../../aib/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { coalitionMembersOf } from '../../aib/contract.ts';
import { fmtUsd } from '../../aib/derive/format.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, readDoc, readScenarioDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { aibTradeOf } from '../derive/framing.ts';
import { fixtureSource, haveFixture, readSheet } from './fixture.ts';

/** The AIB economy AAA renamed USA (a fact-sheet economy), in place. */
function renamed<T extends AibDoc | AibScenarioDoc>(d: T): void {
  Object.assign(d, JSON.parse(JSON.stringify(d).replaceAll('"AAA"', '"USA"')) as T);
}

const restore: (() => void)[] = [];
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
  setAibStoreForTests(new AibStore(aibSource(SYNTHETIC_DIR, renamed, { editScenario: (d) => renamed(d) })));
  // a baseline visit never needs the engine
  setEngineClient(new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/'));
});

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
  setAibStoreForTests(null);
  setEngineClient(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}
const T = { timeout: 30_000 };
// since 2026-10-29 (design 5b) the section is generic partnership material under Allied Strategy's "Explore all
// partnership analysis" (id allied-partnerships, an h3); the #partnerships links open that disclosure
const section = async () => {
  await screen.findByRole('heading', { level: 3, name: 'Partnerships' }, T);
  const part = document.getElementById('allied-partnerships')!;
  expect(part.closest('section#allied')).toBeTruthy();
  return part;
};
const lens = (part: HTMLElement, id: 'today' | 'osi' | 'aib') => within(part).getByTestId(`lens-${id}`);
const tags = (part: HTMLElement) => within(part).getAllByTestId(/^lens-(today|osi|aib)$/).map((p) => p.querySelector('[data-lens-tag]')?.textContent);
const search = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search);

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country profile Partnerships: the three lenses', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  renamed(doc);
  const managed = readScenarioDoc('managed', SYNTHETIC_DIR)!;
  renamed(managed);
  const sheet = readSheet('USA');
  const seats = (cs: readonly { members: readonly string[] | null }[]) => cs.filter((c) => (c.members ?? []).includes('USA')).length;
  const decSeats = seats(doc.compacts.map((c) => ({ members: coalitionMembersOf(c) })));
  const miSeats = seats(managed.compacts.map((c) => ({ members: c.coalition?.members ?? null })));

  it('Trade under Decoupling: four kinds, the summary, then Today (fact sheet ties), OSI recommends (Partnership Opportunity) and the AIB model with its toggle', async () => {
    renderAt('/countries/USA?scn=decoupling#partnerships');
    const part = await section();
    const kinds = within(within(part).getByRole('radiogroup', { name: 'Kind of partnership' })).getAllByRole('radio');
    expect(kinds.map((r) => r.parentElement?.textContent)).toEqual(['Trade', 'Investment', 'Development', 'Geopolitical']);
    expect(tags(part)).toEqual(['Today · observed', 'OSI recommends · NEIS defaults', 'AIB model · Decoupling']);
    expect(within(part).getByRole('heading', { level: 3, name: 'AIB model: Decoupling' })).toBeTruthy();
    // Today: its largest trade partners in the fact sheet's tie order
    const first = sheet.partners.rows.find((r) => r.iso3 === sheet.partners.tie_order.filter((x) => x !== 'USA')[0])!;
    const today = within(lens(part, 'today')).getByTestId('trade-today');
    expect(within(today).getAllByRole('listitem')[0].textContent).toContain(first.name);
    expect(within(lens(part, 'osi')).getByTestId('po-top')).toBeTruthy();
    // the AIB model: the compacts' coordinated trade, and the toggle in the panel
    const t = aibTradeOf(doc.compacts, 'USA');
    const aib = await within(lens(part, 'aib')).findByTestId('aib-trade', {}, T);
    expect(aib.textContent).toContain(`In the compacts' coordinated trade the United States has ${t.trade.n} relationships worth ${fmtUsd(t.trade.usd)}`);
    const toggle = within(lens(part, 'aib')).getByRole('radiogroup', { name: 'Scenario' });
    expect((within(toggle).getByRole('radio', { name: 'Decoupling' }) as HTMLInputElement).checked).toBe(true);
    // the summary compares the three lenses
    const sum = within(part).getByTestId('cross-lens-summary');
    expect(within(sum).getByRole('heading', { level: 3 }).textContent).toBe('Where the lenses agree: trade, the United States');
    expect(within(sum).getByTestId('cross-lens-line').textContent).toMatch(/^The three lenses name \d+ partners: /);
    expect(bareAcronyms(part)).toEqual([]);
  }, 90_000);

  it('Managed Interdependence by default (no scn): the AIB lens and the role section read its file; the toggle in the panel writes scn and keeps the place', async () => {
    const r = renderAt('/countries/USA#partnerships');
    const part = await section();
    expect(await within(part).findByRole('heading', { level: 3, name: 'AIB model: Managed Interdependence' }, T)).toBeTruthy();
    expect(tags(part)[2]).toBe('AIB model · Managed Interdependence');
    const t = aibTradeOf(managed.compacts, 'USA');
    const aib = await within(lens(part, 'aib')).findByTestId('aib-trade', {}, T);
    expect(aib.textContent).toContain(`Under Managed Interdependence, in the compacts' coordinated trade the United States has ${t.trade.n} relationship`);
    const role = document.getElementById('allied-aib-role')!;
    expect((await within(role).findByTestId('aib-role-scenario', {}, T)).textContent).toMatch(/^Shown under Managed Interdependence/);
    // the role cells read the scenario as /aib/roles does: under its own name, no every-shock total
    const cells = await within(role).findByTestId('aib-roles', {}, T);
    expect(cells.textContent).not.toMatch(/under the headline shock|diversification scenario|after the shock/);
    // links into the AIB section carry the scenario
    const c01 = await within(role).findAllByRole('link', { name: managed.compacts[0].id }, T);
    expect(c01[0].getAttribute('href')).toBe(`/compacts/${managed.compacts[0].id}`);
    expect(within(role).getByRole('link', { name: /See all country roles/ }).getAttribute('href')).toBe('/aib/roles?economy=USA#role-USA');
    // to Decoupling from the panel's toggle: scn=decoupling written, the section kept
    fireEvent.click(within(within(lens(part, 'aib')).getByRole('radiogroup', { name: 'Scenario' })).getByRole('radio', { name: 'Decoupling' }));
    await waitFor(() => expect(search(r).get('scn')).toBe('decoupling'));
    expect(r.state.location.hash).toBe('#partnerships');
    expect(await within(part).findByRole('heading', { level: 3, name: 'AIB model: Decoupling' }, T)).toBeTruthy();
    expect(within(role).getByTestId('aib-role-scenario').textContent).toMatch(/^Shown under Decoupling/);
    await waitFor(() => expect(within(role).getByTestId('aib-roles').textContent).not.toMatch(/under the headline shock/));
  }, 90_000);

  it('Investment under Managed Interdependence: the flows of its file; Today without the ties chunk says so in one line', async () => {
    renderAt('/countries/USA?ptab=investment#partnerships');
    const part = await section();
    const best = managed.investmentFlows!.pairs.filter((p) => p.rank === 1 && p.investor === 'USA');
    const usd = best.reduce((s, p) => s + (p.usd ?? 0), 0);
    const inv = await within(lens(part, 'aib')).findByTestId('aib-as-investor', {}, T);
    expect(inv.textContent).toContain(`New capacity at ${best.length} host${best.length === 1 ? '' : 's'} serves ${fmtUsd(usd)} a year`);
    expect(within(lens(part, 'osi')).getByTestId('fs-top')).toBeTruthy();
    // the test build has no Strategic ties chunk: the summary leaves Today out and says why
    expect((await within(part).findByTestId('cross-lens-absent-today', {}, T)).textContent).toMatch(/^Today: .*; left out of the comparison\.$/);
  }, 90_000);

  it('Development under Managed Interdependence: aid says in one line it is not in this build; the BUILD nodes are the scenario\'s', async () => {
    renderAt('/countries/USA?ptab=development#partnerships');
    const part = await section();
    expect(await within(lens(part, 'today')).findByTestId('lens-empty', {}, T)).toBeTruthy();
    const card = await within(lens(part, 'aib')).findByTestId('aib-build', {}, T);
    const build = doc.nodes.filter((n) => n.diversify?.status === 'diversified to the benchmark' && n.diversify.cls === 'BUILD');
    if (build.length) {
      expect(card.textContent).toContain('under Managed Interdependence');
      expect(card.textContent).toContain(build[0].label);
    }
    expect(within(lens(part, 'osi')).getByTestId('dp-top')).toBeTruthy();
  }, 90_000);

  it('Geopolitical: alignment today, the allied coalition, and the compact coalitions following the toggle', async () => {
    const r = renderAt('/countries/USA?ptab=geopolitical&scn=decoupling#partnerships');
    const part = await section();
    expect((within(within(part).getByRole('radiogroup', { name: 'Kind of partnership' })).getByRole('radio', { name: 'Geopolitical' }) as HTMLInputElement).checked).toBe(true);
    expect(within(lens(part, 'today')).getByTestId('geo-today').textContent).toMatch(/In the UN General Assembly it votes/);
    expect(within(lens(part, 'osi')).getByTestId('geo-osi').textContent).toMatch(/The indices treat \d+ core members? and \d+ partners? as the allied coalition/);
    const co = await within(lens(part, 'aib')).findByTestId('aib-coalitions', {}, T);
    expect(co.textContent).toContain(`The United States sits in the coalition of ${decSeats} compact${decSeats === 1 ? '' : 's'} under Decoupling`);
    fireEvent.click(within(within(lens(part, 'aib')).getByRole('radiogroup', { name: 'Scenario' })).getByRole('radio', { name: 'Managed Interdependence' }));
    await waitFor(() => expect(search(r).get('scn')).toBeNull());
    expect(search(r).get('ptab')).toBe('geopolitical');
    const mi = await waitFor(() => {
      const el = within(lens(part, 'aib')).getByTestId('aib-coalitions');
      expect(el.textContent).toContain('under Managed Interdependence');
      return el;
    }, T);
    expect(mi.textContent).toContain(miSeats ? `sits in the coalition of ${miSeats} compact${miSeats === 1 ? '' : 's'} under Managed Interdependence` : 'sits in no compact coalition under Managed Interdependence');
    expect(bareAcronyms(part)).toEqual([]);
  }, 90_000);
});
