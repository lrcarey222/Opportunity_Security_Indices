// /aib/agreements in the app's routes: a lazy page of the Allied Industrial Base hub (inside AibLayout), its
// "Agreements" nav entry after Partnerships marked current, the scenario toggle kept, and the page drawn from
// the SYNTHETIC agreements bundle.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { CountryStore, setCountryStoreForTests } from '../../country/data/store.ts';
import { fixtureSource as countrySource } from '../../country/__tests__/fixture.ts';
import { AgreementsStore, setAgreementsStoreForTests } from '../data.ts';
import { fixtureSource } from './fixture.ts';

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
  setAgreementsStoreForTests(new AgreementsStore(fixtureSource()));
  setCountryStoreForTests(new CountryStore(countrySource()));
  setAibStoreForTests(new AibStore(aibSource(SYNTHETIC_DIR)));
  setEngineClient(new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/'));
});

afterEach(() => {
  cleanup();
  setAgreementsStoreForTests(null);
  setCountryStoreForTests(null);
  setAibStoreForTests(null);
  setEngineClient(null);
});

const T = { timeout: 30_000 };

describe.skipIf(!haveAib(SYNTHETIC_DIR))('the Agreements route', () => {
  it('/aib/agreements renders the page inside the hub, with its nav entry current after Partnerships', async () => {
    const router = createMemoryRouter(routes, { initialEntries: ['/aib/agreements?country=JPN&scn=decoupling'] });
    render(<RouterProvider router={router} />);
    expect(await screen.findByRole('heading', { level: 1, name: 'Agreements' }, T)).toBeTruthy();
    const nav = screen.getByRole('navigation', { name: 'Allied Industrial Base views' });
    const labels = within(nav).getAllByRole('link').map((a) => a.textContent);
    expect(labels.indexOf('Agreements')).toBe(labels.indexOf('Partnerships') + 1);
    await waitFor(() => expect(within(nav).getByRole('link', { name: 'Agreements' }).getAttribute('aria-current')).toBe('page'));
    expect(await screen.findByTestId('agreements-country', {}, T)).toBeTruthy();
    expect(document.title).toMatch(/^Agreements \| Allied Industrial Base/);
    expect(bareAcronyms(document.body)).toEqual([]);
  });
});
