// The Country profile's Partnerships > Investment tab, Today lens, in the app (synthetic Country and AIB
// fixtures, the AIB economy AAA renamed USA): the IMF direct-investment positions and the cleantech factory
// projects both show whatever sector is selected on the page. The technology choice starts on "All four
// technologies" for a sector outside the four (Nuclear, Gas) and on the sector's own technology for one of
// them, at any stage. The test build has no Strategic ties chunk, so its loader reads the small synthetic
// chunk of the Partners tests (tiesFixture.ts) instead.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import type { AibDoc, AibScenarioDoc } from '../../aib/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { fixtureSource, haveFixture } from './fixture.ts';

vi.mock('../../views/Partners/ties.ts', async (importOriginal) => {
  const real = await importOriginal<typeof import('../../views/Partners/ties.ts')>();
  return {
    ...real,
    // read when called, after this mock is in place (the fixture imports the module itself)
    loadTies: async () => (await import('../../views/Partners/tiesFixture.ts')).syntheticTies(),
  };
});

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

const T = { timeout: 30_000 };
const renderAt = (path: string) => render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [path] })} />);
const IMF = 'Direct investment between the United States and its partners';
const PROJECTS = 'Cleantech factory projects between the United States and its partners';

/** The Investment tab's IMF card and factory-projects card, and the projects card's technology choice. */
async function investmentCards() {
  const projects = await screen.findByRole('region', { name: PROJECTS }, T);
  const imf = screen.getByRole('region', { name: IMF });
  // both in the Partnerships section, the IMF positions first
  const part = document.getElementById('medium-partner')!;
  expect(part.contains(imf) && part.contains(projects)).toBe(true);
  expect(imf.compareDocumentPosition(projects) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  // the ties chunk has loaded: the IMF positions are drawn
  expect(await within(imf).findByRole('list', { name: 'the United States: direct investment by partner' }, T)).toBeTruthy();
  const tech = within(projects).getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement;
  return { imf, projects, tech };
}

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country profile Partnerships, Investment: IMF positions and cleantech factory projects', () => {
  it('a sector outside the four technologies: both cards, the projects of all four technologies together', async () => {
    renderAt('/countries/USA?cell=nuclear-components&ptab=investment#partnerships');
    const { imf, projects, tech } = await investmentCards();
    expect(tech.value).toBe('');
    expect(tech.selectedOptions[0].textContent).toBe('All four technologies');
    const items = within(within(projects).getByRole('list', { name: 'the United States: factory projects in solar, wind, batteries and electric vehicles by partner' })).getAllByRole('listitem');
    expect(items[0].textContent).toMatch(/^Japan.*into the United States: 8 projects, \$1\.4 billion of disclosed capital/);
    expect(within(projects).getByTestId('cmfdi-attribution').textContent).toMatch(/CC BY 4\.0/);
    expect(within(imf).getByText(/the tie is the factory projects instead \(see the factory projects card below\)/)).toBeTruthy();
    expect(bareAcronyms(projects)).toEqual([]);
    cleanup();
    renderAt('/countries/USA?cell=gas-extraction&ptab=investment#partnerships');
    expect((await investmentCards()).tech.value).toBe('');
  }, 90_000);

  it("one of the four technologies, at any stage: the choice starts on the sector's technology", async () => {
    renderAt('/countries/USA?cell=solar-deployment&ptab=investment#partnerships');
    const solar = await investmentCards();
    expect(solar.tech.value).toBe('Solar');
    const items = within(within(solar.projects).getByRole('list', { name: 'the United States: factory projects in solar by partner' })).getAllByRole('listitem');
    expect(items[0].textContent).toMatch(/^Japan.*into the United States: 3 projects, \$500 million of disclosed capital/);
    cleanup();
    // Batteries at a CM-FDI stage: none in this chunk, so the card opens on all four; picking Batteries says so in one plain line
    renderAt('/countries/USA?cell=batteries-assembly&ptab=investment#partnerships');
    const batteries = await investmentCards();
    expect(batteries.tech.value).toBe('');
    fireEvent.change(batteries.tech, { target: { value: 'Batteries' } });
    expect(within(batteries.projects).getByText('No cross-border factory projects in batteries involve the United States, in either direction.')).toBeTruthy();
  }, 90_000);
});
