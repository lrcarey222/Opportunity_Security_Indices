// The Country profile's Partnerships section with the relationship thesis (2026-10-20; strategic task spec
// s.19), in the app, on the synthetic Country and AIB fixtures (the AIB economies AAA, BBB and ZZZ renamed
// USA, JPN and CHN, fact-sheet economies, in the main document and both scenario files): above the three
// lenses, the thesis of the economy and its selected partner (URL `partner`) with a Partner picker over the
// fact sheet's partners (those with a thesis first); the compacts link to their strategic pages and the
// footer to the pair on the AIB Partnerships page, the scenario carried; a partner the model writes no
// thesis for says why; the cross-lens summary says where the partner sits; a bundle without the strategic
// layer shows no thesis. No bare acronym.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import type { AibDoc, AibScenarioDoc } from '../../aib/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, stripStrategic, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { fixtureSource, haveFixture } from './fixture.ts';

const RENAME: Readonly<Record<string, string>> = { AAA: 'USA', BBB: 'JPN', ZZZ: 'CHN' };

/** The AIB economies AAA, BBB and ZZZ renamed USA, JPN and CHN (fact-sheet economies), in place. */
function renamed<T extends AibDoc | AibScenarioDoc>(d: T): void {
  let text = JSON.stringify(d);
  for (const [from, to] of Object.entries(RENAME)) text = text.replaceAll(`"${from}"`, `"${to}"`);
  Object.assign(d, JSON.parse(text) as T);
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
const section = async () => {
  await screen.findByRole('heading', { level: 2, name: 'Partnerships' }, T);
  return document.getElementById('partnerships')!;
};
const search = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search);

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country profile Partnerships: the relationship thesis of the selected partner', () => {
  it('the thesis above the lenses, its Partner picker, its links; the summary says where the partner sits', async () => {
    renderAt('/countries/USA?partner=JPN#partnerships');
    const part = await section();
    const card = await within(part).findByTestId('relationship-thesis', {}, T);
    expect(card.getAttribute('data-state')).toBe('thesis');
    expect(within(card).getByRole('heading', { level: 3 }).textContent).toBe('Relationship thesis: the United States and Japan');
    expect(within(card).getByTestId('thesis-headline').textContent).toBe(
      'The United States and Japan: a trade and market access relationship, with Wind Power Equipment and Grid & Electrolysis Equipment as the priority compacts.',
    );
    expect(within(card).getAllByRole('heading', { level: 4 }).map((h) => h.textContent)).toEqual([
      'Complementarity: what each side brings',
      'Priority compacts: where cooperation matters most',
      'AIB logic: what the relationship is about',
    ]);
    expect(within(within(card).getByTestId('thesis-logic')).getAllByRole('listitem')[1].textContent).toBe(
      'Market access. In Wind Power Equipment, the United States is the demand anchor and Japan supplies it as a scale manufacturer (and in 1 more priority compact).',
    );
    // the picker: the fact sheet's partners, those with a thesis first
    const box = within(card).getByRole('combobox', { name: 'Partner' }) as HTMLSelectElement;
    expect(box.value).toBe('JPN');
    const groups = [...box.querySelectorAll('optgroup')];
    expect(groups.map((g) => g.label)).toEqual(['With a relationship thesis', 'No thesis in the model']);
    expect([...groups[0].querySelectorAll('option')].map((o) => o.value)).toEqual(['JPN']);
    // links: the strategic compact pages and the pair on the Partnerships page (Managed Interdependence: no scn)
    expect(within(card).getByRole('link', { name: 'Wind Power Equipment' }).getAttribute('href')).toBe('/compacts/s/SC-WIND');
    expect(within(card).getByRole('link', { name: /^Every relationship thesis of the United States/ }).getAttribute('href')).toBe('/aib/partnerships?country=USA&partner=JPN');
    // the thesis comes before the lenses; the summary names where Japan sits
    const summary = within(part).getByTestId('cross-lens-summary');
    expect(card.compareDocumentPosition(summary) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    // (the lenses name economies with their own namer: the renamed scenario file calls JPN Borduria)
    await waitFor(() => expect(within(summary).getByTestId('cross-lens-pair').textContent).toMatch(/^Where (Japan|Borduria) sits: /));
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('choosing another partner writes `partner`; one outside the allied economies reads why there is no thesis; the scenario is carried', async () => {
    const r = renderAt('/countries/USA?partner=JPN&scn=decoupling#partnerships');
    const part = await section();
    const card = await within(part).findByTestId('relationship-thesis', {}, T);
    expect(within(card).getByRole('link', { name: 'Wind Power Equipment' }).getAttribute('href')).toBe('/compacts/s/SC-WIND?scn=decoupling');
    fireEvent.change(within(card).getByRole('combobox', { name: 'Partner' }), { target: { value: 'CHN' } });
    await waitFor(() => expect(search(r).get('partner')).toBe('CHN'));
    expect(search(r).get('scn')).toBe('decoupling');
    const now = within(part).getByTestId('relationship-thesis');
    expect(now.getAttribute('data-state')).toBe('outside');
    expect(within(now).getByTestId('thesis-headline').textContent).toMatch(/^The model writes relationship theses for pairs of allied economies: China is a major supplier outside the allied economies\. Both take part in/);
    expect(within(now).queryByTestId('thesis-logic')).toBeNull();
    expect(within(now).getByRole('link', { name: /^Every relationship thesis/ }).getAttribute('href')).toBe('/aib/partnerships?country=USA&partner=CHN&scn=decoupling');
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('a bundle without the strategic layer: no thesis, the lenses stay', async () => {
    setAibStoreForTests(
      new AibStore(
        aibSource(
          SYNTHETIC_DIR,
          (d) => {
            renamed(d);
            stripStrategic(d);
          },
          { editScenario: (d) => renamed(d) },
        ),
      ),
    );
    renderAt('/countries/USA?partner=JPN#partnerships');
    const part = await section();
    await within(part).findByTestId('cross-lens-summary', {}, T);
    // the AIB document has loaded (its lens shows), and still no thesis
    await within(part).findByTestId('aib-trade', {}, T);
    expect(within(part).queryByTestId('relationship-thesis')).toBeNull();
  });
});
