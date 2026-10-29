// /aib/partnerships on the three lenses (2026-10-19) with the synthetic fixture: the four kinds; each kind's
// cross-lens summary and its three tagged panels (Today, OSI recommends, AIB model with the toggled
// scenario); Today's trade from the Baseline scenario file; the AIB lens under Managed Interdependence from
// its file (trade relationships, investment flows, coalitions); a lens with no data at the focus says so in
// one line; the filters (economy, OSI technology, stage, node) stay; no bare acronym.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { CountryStore, setCountryStoreForTests } from '../../country/data/store.ts';
import { fixtureSource as countrySource, haveFixture as haveCountry } from '../../country/__tests__/fixture.ts';
import { AibStore, setAibStoreForTests } from '../data/store.ts';
import { fmtUsd } from '../derive/format.ts';
import { resetLastCompact } from '../nav.ts';
import { fixtureSource, haveFixture, readDoc, readScenarioDoc, SYNTHETIC_DIR } from './fixture.ts';

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
  setAibStoreForTests(new AibStore(fixtureSource(SYNTHETIC_DIR)));
  setEngineClient(new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/'));
});

afterEach(() => {
  cleanup();
  setAibStoreForTests(null);
  setCountryStoreForTests(null);
  setEngineClient(null);
  resetLastLocations();
  resetLastCompact();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}
const h1 = async (name: string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const lens = (id: 'today' | 'osi' | 'aib') => screen.getByTestId(`lens-${id}`);
const summary = () => screen.getByTestId('cross-lens-summary');
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('/aib/partnerships: the three lenses', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  const managed = readScenarioDoc('managed', SYNTHETIC_DIR)!;
  const baseline = readScenarioDoc('baseline', SYNTHETIC_DIR)!;
  const name = (x: string) => doc.countries[x] ?? managed.countries[x] ?? x;

  it("Trade under Decoupling: four kinds, the summary, then Today (the Baseline file's pairs and suppliers), OSI recommends and AIB model", async () => {
    renderAt('/aib/partnerships?scn=decoupling');
    expect(await h1('Partnerships')).toBeTruthy();
    const kinds = within(screen.getByRole('radiogroup', { name: 'Partnership type' })).getAllByRole('radio');
    expect(kinds.map((r) => r.parentElement?.textContent)).toEqual(['Trade', 'Investment', 'Development', 'Geopolitical']);
    // the filters stay
    for (const f of ['Economy', 'Technology', 'Stage', 'Node']) expect(screen.getByRole('combobox', { name: f })).toBeTruthy();
    expect(screen.getAllByTestId(/^lens-(today|osi|aib)$/).map((p) => p.querySelector('[data-lens-tag]')?.textContent)).toEqual([
      'Today · observed',
      'OSI recommends · NEIS defaults',
      'AIB model · Decoupling',
    ]);
    expect(screen.getByRole('heading', { level: 2, name: 'AIB model: Decoupling' })).toBeTruthy();
    // Today: who supplies whom, from the Baseline file (lazily loaded)
    const top = [...baseline.network!.edges].sort((a, b) => b.tradeUsd - a.tradeUsd)[0];
    const pairs = await within(lens('today')).findByRole('list', { name: 'Who supplies whom today, the largest pairs' }, { timeout: 15_000 });
    expect(within(pairs).getAllByRole('listitem')[0].textContent).toContain(`${name(top.from)} → ${name(top.to)}`);
    expect(within(pairs).getAllByRole('listitem')[0].textContent).toContain(fmtUsd(top.tradeUsd));
    expect(within(lens('today')).getByRole('region', { name: 'Largest suppliers today' })).toBeTruthy();
    // a name links to the economy's Partnerships on its Country profile (the scenario carried: scn=decoupling)
    const sup = within(lens('today')).getByRole('list', { name: 'Largest suppliers to the allied market today' });
    expect(within(sup).getAllByRole('link')[0].getAttribute('href')).toMatch(/^\/countries\/[A-Z]{3}\?scn=decoupling#partnerships$/);
    // the AIB lens keeps the coordinated trade and the specialized suppliers
    expect(within(lens('aib')).getByRole('region', { name: 'Coordinated trade relationships' })).toBeTruthy();
    expect(within(lens('aib')).getByRole('region', { name: 'Specialized suppliers' })).toBeTruthy();
    expect(within(lens('osi')).getByRole('region', { name: 'Partnership Opportunity' })).toBeTruthy();
    // the summary compares the lenses with data here; the OSI rankings are not in the test build: left out, said so
    expect(within(summary()).getByRole('heading').textContent).toBe('Where the lenses agree: trade, all economies and sub-sectors');
    await waitFor(() => expect(within(summary()).getByTestId('cross-lens-absent-osi').textContent).toMatch(/^OSI recommends: .*; left out of the comparison\.$/));
    expect(within(summary()).getByTestId('cross-lens-line').textContent).toMatch(/^Today and AIB model name \d+ economies/);
    noAcronyms();
  });

  it('Trade under Managed Interdependence: the AIB lens reads the scenario file (its relationships), says what it changes, and links to a Country profile carry the scenario', async () => {
    renderAt('/aib/partnerships');
    expect(await h1('Partnerships')).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 2, name: 'AIB model: Managed Interdependence' })).toBeTruthy();
    const aib = await waitFor(() => {
      const el = lens('aib');
      expect(within(el).getByRole('region', { name: 'Coordinated trade relationships' })).toBeTruthy();
      return el;
    });
    expect(within(aib).getByTestId('managed-note').textContent).toMatch(/changes only the sub-sectors more concentrated than comparable goods/);
    const mi = managed.compacts.flatMap((c) => c.trade).sort((a, b) => (b.usd ?? 0) - (a.usd ?? 0))[0];
    const fig = within(aib).getByRole('region', { name: 'Coordinated trade relationships' }).querySelector('figcaption')!;
    expect(fig.textContent).toContain(`${name(mi.from)} to ${name(mi.to)} ${fmtUsd(mi.usd)}`);
    expect(fig.textContent).toContain('Managed Interdependence');
    // the Decoupling-only pairs are gone
    const decOnly = doc.compacts.flatMap((c) => c.trade).find((e) => !managed.compacts.some((c) => c.trade.some((x) => x.from === e.from && x.to === e.to)));
    if (decOnly) expect(fig.textContent).not.toContain(`${name(decOnly.from)} to ${name(decOnly.to)} `);
    // the Country profile's AIB lens follows the same key: the default is never written on the links
    const sup = within(lens('today')).getByRole('list', { name: 'Largest suppliers to the allied market today' });
    expect(within(sup).getAllByRole('link')[0].getAttribute('href')).toMatch(/^\/countries\/[A-Z]{3}#partnerships$/);
    const sumLinks = within(summary()).queryAllByRole('link');
    for (const a of sumLinks) expect(a.getAttribute('href')).toMatch(/^[^?]*#partnerships$/);
    noAcronyms();
  });

  it('Investment: without an economy the Today lens says what to choose; under Managed Interdependence the flows are the scenario file\'s', async () => {
    const r = renderAt('/aib/partnerships?mode=investment');
    expect(await h1('Partnerships')).toBeTruthy();
    expect(within(lens('today')).getByTestId('lens-empty').textContent).toBe('Choose an economy to see its direct-investment positions (IMF) and factory projects.');
    expect(within(summary()).getByTestId('cross-lens-absent-today').textContent).toMatch(/choose an economy/);
    const card = await within(lens('aib')).findByRole('region', { name: 'Who invests where' }, { timeout: 15_000 });
    const best = managed.investmentFlows!.pairs.filter((p) => p.rank === 1);
    await waitFor(() => expect(within(within(card).getByTestId('flow-list')).getAllByRole('listitem')).toHaveLength(best.length));
    expect(within(lens('aib')).getByRole('region', { name: 'Where new capacity buys most resilience' })).toBeTruthy();
    // an economy: its direct-investment cards load (here the ties chunk is absent: the cards say so)
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: best[0].host } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('country')).toBe(best[0].host));
    expect(new URLSearchParams(r.state.location.search).get('scn')).toBeNull();
    expect(within(lens('today')).queryByTestId('lens-empty')).toBeNull();
    noAcronyms();
  });

  it('Development: the Today lens loads the aid flows on this tab only and says in one line when they are not there; the capacity gaps follow the scenario', async () => {
    renderAt('/aib/partnerships?mode=development');
    expect(await h1('Partnerships')).toBeTruthy();
    // no Sector bundle in the test build: one line, never a blank panel
    expect(await within(lens('today')).findByTestId('lens-empty', {}, { timeout: 15_000 })).toBeTruthy();
    const gaps = await within(lens('aib')).findByRole('region', { name: 'Capacity gaps' }, { timeout: 15_000 });
    const build = doc.nodes.filter((n) => n.diversify?.status === 'diversified to the benchmark' && n.diversify.cls === 'BUILD');
    const list = within(gaps).queryByRole('list', { name: 'Largest capacity gaps' });
    if (build.length) expect(within(list!).getAllByRole('listitem')).toHaveLength(Math.min(8, build.length));
    else expect(list).toBeNull();
    noAcronyms();
  });

  it.skipIf(!haveCountry())('Geopolitical: the coalition tiers (Country roster) under OSI, and the compact coalitions under the scenario with an economy\'s seats', async () => {
    setCountryStoreForTests(new CountryStore(countrySource()));
    // an economy in two Managed Interdependence coalitions
    const counts = new Map<string, number>();
    for (const c of managed.compacts) for (const m of c.coalition?.members ?? []) counts.set(m, (counts.get(m) ?? 0) + 1);
    const iso = [...counts.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))[0][0];
    renderAt(`/aib/partnerships?mode=geopolitical&country=${iso}`);
    expect(await h1('Partnerships')).toBeTruthy();
    expect((within(screen.getByRole('radiogroup', { name: 'Partnership type' })).getByRole('radio', { name: 'Geopolitical' }) as HTMLInputElement).checked).toBe(true);
    const card = await within(lens('aib')).findByRole('region', { name: 'Compact coalitions' }, { timeout: 15_000 });
    expect(card.textContent).toContain(`${name(iso)} sits in the coalition of ${counts.get(iso)} compacts under Managed Interdependence`);
    const osi = await within(lens('osi')).findByTestId('geo-osi', {}, { timeout: 15_000 });
    expect(osi.textContent).toMatch(/core members? and \d+ partners?/);
    noAcronyms();
  });
});
