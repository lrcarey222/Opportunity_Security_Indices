// /aib/partnerships with a pair (2026-10-20; strategic task spec s.19), on the synthetic fixture: the
// Partner filter (URL `partner`, read only with an economy, its economies grouped by whether the model
// writes a thesis for the pair) beside the existing filters and URL keys; the relationship thesis above the
// lenses (complementarity, priority compacts linked to their strategic pages with the scenario carried, the
// AIB logic); the suggestions without a partner; the cross-lens summary's line on where the partner sits;
// a partner that is not valid, or without an economy, is ignored; clearing the economy or choosing the
// partner as the economy drops it; a bundle without the strategic layer shows no thesis. No bare acronym.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { setCountryStoreForTests } from '../../country/data/store.ts';
import { AibStore, setAibStoreForTests } from '../data/store.ts';
import { resetLastCompact } from '../nav.ts';
import { fixtureSource, haveFixture, stripStrategic, SYNTHETIC_DIR } from './fixture.ts';

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
const T = { timeout: 30_000 };
const h1 = async () => screen.findByRole('heading', { level: 1, name: 'Partnerships' }, T);
const search = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search);
const partnerBox = () => screen.getByRole('combobox', { name: 'Partner' }) as HTMLSelectElement;
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('/aib/partnerships: the relationship thesis of a pair', () => {
  it('a pair: the Partner filter beside the others, the thesis above the lenses, and where the partner sits in the summary', async () => {
    renderAt('/aib/partnerships?country=AAA&partner=CCC');
    await h1();
    for (const f of ['Economy', 'Partner', 'Technology', 'Stage', 'Supply-chain step']) expect(screen.getByRole('combobox', { name: f })).toBeTruthy();
    // the Partner filter: the economies with a thesis first, in their own group
    const box = partnerBox();
    expect(box.value).toBe('CCC');
    expect(box.disabled).toBe(false);
    const groups = [...box.querySelectorAll('optgroup')];
    expect(groups.map((g) => g.label)).toEqual(['With a relationship thesis', 'Other economies']);
    expect([...groups[0].querySelectorAll('option')].map((o) => o.value)).toEqual(['BBB', 'CCC', 'DDD', 'EEE']);
    expect([...box.querySelectorAll('option')].some((o) => o.value === 'AAA')).toBe(false);
    // the thesis, above the lenses
    const card = screen.getByTestId('relationship-thesis');
    expect(within(card).getByRole('heading', { level: 2 }).textContent).toBe('Relationship thesis: Avalon and Carpania');
    expect(within(card).getAllByRole('heading', { level: 3 }).map((h) => h.textContent)).toEqual([
      'Complementarity: what each side brings',
      'Priority compacts: where cooperation matters most',
      'AIB logic: what the relationship is about',
    ]);
    const summary = await screen.findByTestId('cross-lens-summary', {}, T);
    expect(card.compareDocumentPosition(summary) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    // Managed Interdependence is the default: the compact link carries no scenario
    expect(within(card).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    expect(within(card).getByRole('link', { name: 'Carpania' }).getAttribute('href')).toBe('/countries/CCC#aib-role');
    await waitFor(() => expect(within(summary).getByTestId('cross-lens-pair').textContent).toMatch(/^Where Carpania sits: /));
    noAcronyms();
  });

  it('choosing a partner writes `partner` and keeps the other keys; the scenario is carried to the compact links; clearing the economy drops the partner', async () => {
    const r = renderAt('/aib/partnerships?country=AAA&mode=investment&scn=decoupling');
    await h1();
    // no partner yet: the suggestions
    const prompt = screen.getByTestId('thesis-prompt');
    expect(prompt.textContent).toMatch(/^Choose a partner to read the strategic case for the pair\. The analysis has 4 for Avalon/);
    expect(within(prompt).getByRole('link', { name: 'Carpania' }).getAttribute('href')).toBe('/aib/partnerships?country=AAA&mode=investment&scn=decoupling&partner=CCC');
    fireEvent.change(partnerBox(), { target: { value: 'BBB' } });
    await waitFor(() => expect(search(r).get('partner')).toBe('BBB'));
    expect(search(r).get('mode')).toBe('investment');
    expect(search(r).get('scn')).toBe('decoupling');
    expect(search(r).get('country')).toBe('AAA');
    const card = screen.getByTestId('relationship-thesis');
    expect(within(card).getByRole('heading', { level: 2 }).textContent).toBe('Relationship thesis: Avalon and Borduria');
    expect(within(card).getByRole('link', { name: 'Wind Power Equipment' }).getAttribute('href')).toBe('/compacts/s/SC-WIND?scn=decoupling');
    // the partner as the economy: the partner goes
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: 'BBB' } });
    await waitFor(() => expect(search(r).get('country')).toBe('BBB'));
    expect(search(r).get('partner')).toBeNull();
    // the economy cleared: no partner, no thesis, the Partner filter waits for an economy
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: '' } });
    await waitFor(() => expect(search(r).get('country')).toBeNull());
    expect(screen.queryByTestId('relationship-thesis')).toBeNull();
    expect(partnerBox().disabled).toBe(true);
    noAcronyms();
  });

  it('a partner without an economy, or not an economy of the page, is ignored', async () => {
    renderAt('/aib/partnerships?partner=CCC');
    await h1();
    expect(screen.queryByTestId('relationship-thesis')).toBeNull();
    expect(partnerBox().value).toBe('');
    cleanup();
    renderAt('/aib/partnerships?country=AAA&partner=QQQ');
    await h1();
    expect(screen.getByTestId('relationship-thesis').getAttribute('data-state')).toBe('choose');
  });

  it('a pair the model writes no thesis for says why', async () => {
    renderAt('/aib/partnerships?country=AAA&partner=ZZZ');
    await h1();
    const card = screen.getByTestId('relationship-thesis');
    expect(card.getAttribute('data-state')).toBe('outside');
    expect(within(card).getByTestId('thesis-headline').textContent).toMatch(/^Relationship theses are written for pairs of allied economies: Zembla is a major supplier outside the allied economies\./);
    expect(within(card).queryByTestId('thesis-logic')).toBeNull();
  });

  it('a bundle without the strategic layer: no thesis, the Partner filter (one list) and the summary line stay', async () => {
    setAibStoreForTests(new AibStore(fixtureSource(SYNTHETIC_DIR, (d) => void stripStrategic(d))));
    renderAt('/aib/partnerships?country=AAA&partner=CCC');
    await h1();
    expect(screen.queryByTestId('relationship-thesis')).toBeNull();
    expect(partnerBox().value).toBe('CCC');
    expect(partnerBox().querySelectorAll('optgroup')).toHaveLength(0);
    const summary = await screen.findByTestId('cross-lens-summary', {}, T);
    await waitFor(() => expect(within(summary).getByTestId('cross-lens-pair')).toBeTruthy());
    noAcronyms();
  });
});
