// App-level navigation (IA refactor 2026-10-16; brief s.3, s.20-21, s.27; plan 2 and 9), on the Sector,
// Country and AIB fixtures through their stores and with no engine client:
//   * every primary route renders under the shared header: an h1, its tab current (About is secondary),
//     its views nav item current, a "<view> | <tab> | Allied Industrial Base Explorer" title, no error
//     boundary, no console error, and no engine (a baseline visit never starts one);
//   * the aliases and old deep links still resolve (query string and hash kept);
//   * the three acceptance workflows at navigation level: Home -> Sectors -> a sector profile with the
//     four lenses; Home -> Countries -> Japan with the profile's sections and the Perspective control;
//     Home -> Allied Industrial Base -> overview -> a compact -> a country's role in its profile.
// The legacy Strategic pages (engine and dial chrome) and the dial behaviour are tested in app.test.tsx.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { peekEngineClient, setEngineClient } from '../../state/EngineProvider.tsx';
import { resetLastLocations } from '../lastLocation.ts';
import { resetPrefetch } from '../prefetch.ts';
import { resetSectorManifestCache } from '../sectorManifest.ts';
import { buildSectorFixture, fixtureSource as sectorSource } from '../../sector/__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../../sector/data/store.ts';
import { CountryStore, setCountryStoreForTests } from '../../country/data/store.ts';
import { fixtureSource as countrySource } from '../../country/__tests__/fixture.ts';
import type { CountryEntry, CountryManifest } from '../../country/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { FIXTURE_DIR as AIB_DIR, fixtureSource as aibSource, readDoc as readAibDoc } from '../../aib/__tests__/fixture.ts';

/** The Country fixture with the AIB fixture's economies added to the roster (without index data): the
 *  synthetic AIB codes (AAA ...) are not in the Country fixture, and a role link must land on a profile. */
function countrySourceWithAib(): ReturnType<typeof countrySource> {
  const src = countrySource();
  const names = readAibDoc(AIB_DIR).countries as Record<string, string>;
  return {
    ...src,
    async manifest() {
      const m = (await src.manifest()) as CountryManifest;
      for (const [iso3, name] of Object.entries(names)) {
        if (m.countries.some((c) => c.iso3 === iso3)) continue;
        const e: CountryEntry = {
          iso3, name, tier: 'none', region: null, eu_member: false, status: 'no_data', file: null, bytes_gz: null,
          n_placed: null, quadrants: null, memo: null, drivers: null, iso_n3: null,
        };
        m.countries.push(e);
        m.counts.economies += 1;
      }
      return m;
    },
  };
}

const restore: (() => void)[] = [];
const errors: string[] = [];

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
  // the data router builds a fetch Request with jsdom's AbortSignal, which Node's Request rejects
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
  // Home's idle prefetch reads the Sector manifest over fetch: answer 404 (a silent failure)
  const fetchSpy = vi.spyOn(globalThis, 'fetch').mockImplementation(async () => new Response('', { status: 404 }));
  restore.push(() => fetchSpy.mockRestore());
  const spy = vi.spyOn(console, 'error').mockImplementation((...args: unknown[]) => {
    const text = args.map((a) => (a instanceof Error ? `${a.name}: ${a.message}` : String(a))).join(' ');
    if (!/not wrapped in act|act\(\.\.\.\)/.test(text)) errors.push(text);
  });
  restore.push(() => spy.mockRestore());
  setEngineClient(null);
});

afterAll(() => {
  for (const r of restore.reverse()) r();
});

beforeEach(() => {
  errors.length = 0;
  setSectorStoreForTests(new SectorStore(sectorSource(buildSectorFixture())));
  setCountryStoreForTests(new CountryStore(countrySource()));
  setAibStoreForTests(new AibStore(aibSource(AIB_DIR)));
});

afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
  setCountryStoreForTests(null);
  setAibStoreForTests(null);
  resetLastLocations();
  resetPrefetch();
  resetSectorManifestCache();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const where = (r: ReturnType<typeof renderAt>) => r.state.location.pathname + r.state.location.search + r.state.location.hash;
const h1 = (name?: RegExp | string) => screen.findByRole('heading', { level: 1, ...(name ? { name } : {}) }, { timeout: 30_000 });
const tabs = () => screen.getByRole('navigation', { name: 'Modules' });
const currentTabs = () => within(tabs()).getAllByRole('link').filter((a) => a.getAttribute('aria-current') === 'true').map((a) => a.textContent);

async function settle(ms = 100) {
  await act(async () => {
    await new Promise((r) => setTimeout(r, ms));
  });
}

/** A link in the page by CSS selector and, optionally, accessible name (aria-label or text). */
const linkIn = (sel: string, name?: RegExp) => () =>
  [...document.querySelectorAll<HTMLAnchorElement>(sel)].find((a) => !name || name.test(a.getAttribute('aria-label') ?? a.textContent?.trim() ?? '')) ?? null;

/** Clicks the link `query` finds until the router reaches `done` (30 s at most). Cheap DOM queries, re-run
 *  each time: under a loaded machine a page can re-render between a query and a click, and a click on the
 *  detached node never reaches React. Never fires events inside a waitFor callback (it would re-run on its
 *  own mutations and starve the timers). */
async function follow(router: ReturnType<typeof renderAt>, query: () => HTMLElement | null, done: (r: ReturnType<typeof renderAt>) => boolean) {
  const end = Date.now() + 30_000;
  while (!done(router) && Date.now() < end) {
    const el = query();
    // one navigation at a time: a click during a pending (lazy) navigation would start another
    if (el && router.state.navigation.state === 'idle') fireEvent.click(el);
    await settle(250);
  }
  expect(done(router)).toBe(true);
}

const COMPACT_ID = readAibDoc(AIB_DIR).compacts[0].id;

describe('the global navigation', () => {
  it('Home, Sectors, Countries, Allied Industrial Base and About (secondary); nothing else', async () => {
    renderAt('/');
    expect(await h1('Explore the electro-industrial system')).toBeTruthy();
    expect(within(tabs()).getAllByRole('link').map((a) => a.textContent)).toEqual(['Sectors', 'Countries', 'Allied Industrial Base', 'About']);
    expect(currentTabs()).toEqual([]);
    expect(screen.getByRole('link', { name: 'Allied Industrial Base Explorer' }).getAttribute('href')).toBe('/');
    expect(screen.queryByText(/Strategic Insights|Country Insights|Sector Insights/)).toBeNull();
    // the three entry cards and their calls to action
    for (const [title, cta, to] of [
      ['Explore a Sector', 'Explore sectors', '/sector'],
      ['Explore a Country', 'Explore countries', '/countries'],
      ['Explore the Allied Industrial Base', 'Explore AIB', '/aib'],
    ]) {
      const card = screen.getByRole('article', { name: title });
      expect(within(card).getByRole('link', { name: cta }).getAttribute('href')).toBe(to);
    }
    await settle();
    expect(peekEngineClient()).toBeNull();
  }, 60_000);
});

// path, tab, the views nav item that is current (null: the module has none for that page)
const PRIMARY: [string, string, string | null][] = [
  ['/sector', 'Sectors', 'All sectors'],
  ['/sector/batteries/midstream', 'Sectors', 'Sector profile'],
  ['/sector/minerals', 'Sectors', 'Minerals'],
  ['/countries', 'Countries', 'All countries'],
  ['/countries/JPN', 'Countries', 'Country profile'],
  ['/aib', 'Allied Industrial Base', 'Overview'],
  ['/aib/partnerships', 'Allied Industrial Base', 'Partnerships'],
  ['/compacts', 'Allied Industrial Base', 'Sectoral Compacts'],
  [`/compacts/${COMPACT_ID}`, 'Allied Industrial Base', 'Sectoral Compacts'],
  ['/aib/roles', 'Allied Industrial Base', 'Country roles'],
  ['/about', 'About', 'Overview'],
  ['/glossary', 'About', 'Glossary'],
];

const VIEWS_NAV: Record<string, string> = {
  Sectors: 'Sector views',
  Countries: 'Country views',
  'Allied Industrial Base': 'Allied Industrial Base views',
  About: 'About pages',
};

describe('every primary route renders (no engine on a baseline visit)', () => {
  for (const [path, tab, item] of PRIMARY) {
    it(path, async () => {
      renderAt(path);
      expect(await h1()).toBeTruthy();
      await waitFor(() => expect(currentTabs()).toEqual([tab]), { timeout: 20_000 });
      if (item) {
        const nav = screen.getByRole('navigation', { name: VIEWS_NAV[tab] });
        await waitFor(() => expect(within(nav).getAllByRole('link').filter((a) => a.getAttribute('aria-current') === 'page').map((a) => a.textContent)).toEqual([item]));
      }
      await settle(300);
      // "<view> | <tab> | app" ("<tab> | app" on a landing); About pages carry no tab: "<view> | app"
      if (tab === 'About') expect(document.title).toMatch(/^[^|]+ \| Allied Industrial Base Explorer \| NEIS Center$/);
      else expect(document.title).toMatch(new RegExp(`(^|\\| )${tab} \\| Allied Industrial Base Explorer`));
      expect(screen.queryByText(/This view failed to render|could not be loaded/i)).toBeNull();
      expect(peekEngineClient()).toBeNull();
      expect(errors).toEqual([]);
    }, 60_000);
  }
});

// old link -> where it lands (query string and hash kept)
const ALIASES: [string, string][] = [
  ['/sectors?c=JPN', '/sector?c=JPN'],
  ['/sector-insights', '/sector'],
  ['/strategic-insights', '/countries'],
  ['/country-insights', '/countries'],
  ['/allied-industrial-base', '/aib'],
  ['/sectoral-compacts', '/compacts'],
  [`/compacts/${COMPACT_ID}/nodes`, `/compacts/${COMPACT_ID}`],
  ['/aib/no-such-view', '/aib'],
];

describe('aliases and old deep links still resolve', () => {
  for (const [from, to] of ALIASES) {
    it(`${from} -> ${to}`, async () => {
      const router = renderAt(from);
      await waitFor(() => expect(where(router)).toBe(to), { timeout: 20_000 });
      expect(await h1()).toBeTruthy();
      await settle(200);
      expect(peekEngineClient()).toBeNull();
    }, 60_000);
  }
});

describe('acceptance workflows (brief s.27), at navigation level', () => {
  const path = (p: string | RegExp) => (r: ReturnType<typeof renderAt>) => (typeof p === 'string' ? r.state.location.pathname === p : p.test(r.state.location.pathname));

  it('Sector: Home -> Sectors -> Solar -> a sub-component -> one profile with the four lenses', async () => {
    const router = renderAt('/');
    await follow(router, linkIn('main article a[href="/sector"]', /^Explore sectors$/), path('/sector'));
    expect(await screen.findByRole('heading', { name: 'Pick a technology' }, { timeout: 30_000 })).toBeTruthy();
    // "Pick a technology" opens the technology's chain (Upstream -> Midstream -> Downstream)
    await follow(router, linkIn('main a[href^="/sector/solar"]', /^Solar$/), path('/sector/solar'));
    await follow(router, linkIn('nav[aria-label="Solar supply chain"] a', /^Silicon metal$/), path('/sector/solar/upstream'));
    expect(new URLSearchParams(router.state.location.search).get('sub')).toBe('Silicon metal');
    for (const lens of ['Economic Opportunity', 'Energy Security', 'Policy', 'Climate']) {
      expect(await screen.findByRole('article', { name: lens }, { timeout: 30_000 })).toBeTruthy();
    }
    expect(currentTabs()).toEqual(['Sectors']);
    await settle(300);
    expect(peekEngineClient()).toBeNull();
    expect(errors).toEqual([]);
  }, 90_000);

  it('Country: Home -> Countries -> Japan: the profile sections and the Perspective control, no engine yet', async () => {
    const router = renderAt('/');
    await follow(router, linkIn('main article a[href="/countries"]', /^Explore countries$/), path('/countries'));
    await follow(router, linkIn('main a[href="/countries/JPN"]'), path('/countries/JPN'));
    expect(await h1(/Japan/)).toBeTruthy();
    expect(await screen.findByRole('button', { name: /^Perspective: NEIS baseline/ }, { timeout: 30_000 })).toBeTruthy();
    for (const id of ['position', 'opportunities', 'sectors', 'policy', 'partnerships', 'aib-role']) {
      await waitFor(() => expect(document.getElementById(id)).toBeTruthy(), { timeout: 30_000 });
    }
    expect(screen.getAllByText(/Observed \/ modelled facts/).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/Preference-adjusted priorities/).length).toBeGreaterThan(0);
    await settle(300);
    expect(peekEngineClient()).toBeNull();
    expect(errors).toEqual([]);
  }, 90_000);

  it('AIB: Home -> Allied Industrial Base -> overview -> Sectoral Compacts -> a compact -> a country role -> its profile', async () => {
    setCountryStoreForTests(new CountryStore(countrySourceWithAib()));
    const router = renderAt('/');
    await follow(router, linkIn('main article a[href="/aib"]', /^Explore AIB$/), path('/aib'));
    expect(await screen.findByRole('list', { name: 'Nodes by treatment' }, { timeout: 30_000 })).toBeTruthy();
    await follow(router, linkIn('nav[aria-label="Allied Industrial Base views"] a', /^Sectoral Compacts$/), path('/compacts'));
    await follow(router, linkIn('main a', /^View compact/), path(/^\/compacts\/[^/]+$/));
    const role = await waitFor(() => {
      const a = linkIn('main a[href^="/countries/"][href$="#aib-role"]')();
      expect(a).toBeTruthy();
      return a as HTMLAnchorElement;
    }, { timeout: 30_000 });
    const to = role.getAttribute('href') as string;
    await follow(router, linkIn(`main a[href="${to}"]`), (r) => r.state.location.pathname + r.state.location.hash === to);
    // the router state changes before React commits the lazy Country route
    await waitFor(() => expect(currentTabs()).toEqual(['Countries']), { timeout: 20_000 });
    const iso3 = to.slice('/countries/'.length, to.indexOf('#'));
    expect(await h1((readAibDoc(AIB_DIR).countries as Record<string, string>)[iso3])).toBeTruthy();
    await settle(300);
    expect(peekEngineClient()).toBeNull();
    expect(errors).toEqual([]);
  }, 90_000);
});
