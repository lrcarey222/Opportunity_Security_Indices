// UI smoke and behaviour tests (jsdom, spec_react.md 10.3 project `ui`): the whole app on the
// committed synthetic bundle (scripts/86, s1) with the engine host running in process instead of a
// Web Worker. Every view renders without an error at the baseline and in a scenario; the dial panel
// writes the URL (keyboard and pointer), the status strip and reset follow.
//
// The shell (spec_sector.md 2, 10.3; spec_country.md A2): Home renders without an engine client and
// without a request before idle; legacy root links redirect; unknown paths go Home with a notice;
// aliases; the module tabs restore the last Strategic URL with its scenario and carry the country to
// Countries; Sector and Country render while the engine never starts; the shared /glossary page; Methods
// and Data in the About layout (engine, one-line perspective, no dial panel).
// No Strategic page (view, header, footer, dial panel, status strip) shows a bare acronym (C3.4).

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { peekEngineClient, setEngineClient, useScenario } from './state/EngineProvider.tsx';
import { EngineScope } from './shell/EngineScope.tsx';
import { routes } from './routes.tsx';
import { resetLastLocations } from './shell/lastLocation.ts';
import { resetBuildInfo } from './shell/buildInfo.ts';
import { resetSectorManifestCache } from './shell/sectorManifest.ts';
import { sectorManifestFixture } from './shell/__tests__/manifestFixture.ts';
import { resetPrefetch } from './shell/prefetch.ts';
import { bareAcronyms } from './components/testing/acronyms.ts';
import { CountryLoadError, CountryStore, setCountryStoreForTests } from './country/data/store.ts';
import { fixtureSource } from './country/__tests__/fixture.ts';
import { EngineClient, type WorkerLike } from './worker/client.ts';
import { EngineHost, type FetchBytes } from './worker/host.ts';
import type { FromWorker, ToWorker } from './worker/protocol.ts';

// (vitest runs from dashboard/; under jsdom import.meta.url is not a file URL)
const dataBase = resolve(process.cwd(), 'src/engine/__fixtures__/synthetic/s1/data') + '/';

const fetchDisk: FetchBytes = async (url) => {
  const b = readFileSync(url);
  const out = new Uint8Array(b.length);
  out.set(b);
  return out;
};
const inflate = async (b: Uint8Array) => new Uint8Array(gunzipSync(b));

/**
 * Node's structuredClone makes typed arrays of Node's realm, while code under jsdom sees jsdom's
 * constructors (so `instanceof Float64Array` fails). A browser's postMessage clones into the page's
 * realm; this does the same for the test.
 */
function intoThisRealm<T>(x: T): T {
  if (ArrayBuffer.isView(x)) {
    const C = (globalThis as unknown as Record<string, new (a: ArrayBufferView) => ArrayBufferView>)[x.constructor.name];
    return (C && !(x instanceof C) ? new C(x) : x) as T;
  }
  if (Array.isArray(x)) return x.map(intoThisRealm) as T;
  if (x && typeof x === 'object' && !(x instanceof Map) && !(x instanceof Set)) {
    const o = x as Record<string, unknown>;
    for (const k of Object.keys(o)) o[k] = intoThisRealm(o[k]);
  }
  return x;
}

/** The module worker's stand-in: the same host, structured clones both ways, async delivery. */
function inProcessWorker(): WorkerLike {
  const w: WorkerLike = {
    onmessage: null,
    postMessage(msg: ToWorker) {
      const m = structuredClone(msg);
      setTimeout(() => void host.handle(m), 0);
    },
  };
  const deliver = (m: FromWorker) => setTimeout(() => w.onmessage?.({ data: intoThisRealm(structuredClone(m)) } as MessageEvent<FromWorker>), 0);
  const host = new EngineHost(deliver, fetchDisk, inflate, { selfCheck: true });
  return w;
}

let client: EngineClient;
const errors: string[] = [];
const restore: (() => void)[] = [];

beforeAll(async () => {
  // jsdom has no layout: give elements a width so the charts render, and a stable matchMedia
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: /min-width: 1024px/.test(q), media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  // the data router builds a fetch Request with jsdom's AbortSignal, which Node's Request rejects;
  // the app has no loaders, so tests drop the signal
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
  const orig = console.error;
  const spy = vi.spyOn(console, 'error').mockImplementation((...args: unknown[]) => {
    const text = args.map((a) => (a instanceof Error ? `${a.name}: ${a.message}` : String(a))).join(' ');
    if (!/not wrapped in act|act\(\.\.\.\)/.test(text)) errors.push(text);
    if (process.env.UI_TEST_VERBOSE) orig(...args);
  });
  restore.push(() => spy.mockRestore());
  client = new EngineClient(inProcessWorker(), dataBase);
  await client.whenReady();
  // StrategicLayout mounts the page's shared client: make it the in-process one
  setEngineClient(client);
}, 60_000);

afterEach(() => {
  cleanup();
  resetLastLocations();
  setCountryStoreForTests(null);
});
afterAll(() => {
  setEngineClient(null);
  client.terminate?.();
  for (const f of restore) f();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

async function settle(ms = 50) {
  await act(async () => {
    await new Promise((r) => setTimeout(r, ms));
  });
}

/** A whole Strategic page (C3.4): the shell's header and footer, and <main> (the view, the status strip
 *  and the dial panel). The shell's header is the first <header>, its footer the last <footer>. */
function pageAcronyms(): string[] {
  const headers = document.querySelectorAll('header');
  const footers = document.querySelectorAll('footer');
  const parts = [headers[0], footers[footers.length - 1], document.querySelector('main')].filter((e): e is HTMLElement => !!e);
  return [...new Set(parts.flatMap((el) => bareAcronyms(el)))];
}

// (the Strategic builder owns the view titles; the old and the new wording both pass)
const VIEWS: [string, RegExp][] = [
  ['/overview', /^Overview$/],
  ['/country', /^Country (scores|profile)/],
  ['/space', /opportunity.security space|security vs opportunity/i],
  ['/strategic', /^Strategic Index$/],
  ['/partners', /^Partners$/],
  ['/methods', /^Methods$/],
  ['/data', /^Data$/],
];

describe('every view renders', () => {
  for (const [path, title] of VIEWS) {
    for (const q of ['', '?h=80&g=95&show=change']) {
      it(`${path}${q}`, async () => {
        errors.length = 0;
        renderAt(path + q);
        expect(await screen.findByRole('heading', { level: 1, name: title }, { timeout: 20_000 })).toBeTruthy();
        // let the run, the lazy chunks and the per-view requests finish
        await waitFor(() => expect(screen.getByRole('main').querySelector('.chart-updating')).toBeNull(), { timeout: 30_000 });
        // the charts drew (Plot and the d3 maps render into the jsdom document); under CPU load the
        // partner layer finishes late, so wait on the figures rather than a fixed pause
        if (!/methods|data|strategic/.test(path)) {
          await waitFor(() => expect(screen.getByRole('main').querySelectorAll('figure svg').length).toBeGreaterThan(0), { timeout: 30_000 });
        }
        await settle(200);
        expect(screen.queryByText(/This view failed to render/)).toBeNull();
        expect(screen.queryByText(/Recomputed values differ from the export/)).toBeNull();
        if (process.env.UI_TEST_VERBOSE) console.log(path, screen.getByRole('main').textContent?.slice(0, 1500));
        expect(screen.getByText(q ? 'Custom perspective' : 'NEIS baseline', { selector: '.tag' })).toBeTruthy();
        // Methods and Data are About pages: the engine without the dial panel
        if (/methods|data/.test(path)) expect(screen.queryByRole('complementary', { name: 'Scenario dials' })).toBeNull();
        expect(pageAcronyms()).toEqual([]);
        expect(errors).toEqual([]);
      }, 60_000);
    }
  }
});

describe('dial panel and status strip', () => {
  it('sliders write the URL; PageUp steps by 25; chips and reset return to the paper', async () => {
    const router = renderAt('/overview');
    const slider = (await screen.findByLabelText('HAWKISHNESS', {}, { timeout: 20_000 })) as HTMLInputElement;
    expect(slider.type).toBe('range');
    expect(slider.step).toBe('5');
    expect(slider.getAttribute('aria-valuetext')).toBe('50, the NEIS baseline');
    const reset = screen.getByRole('button', { name: /reset to neis baseline/i });
    expect((reset as HTMLButtonElement).disabled).toBe(true);

    fireEvent.keyDown(slider, { key: 'PageUp' });
    await waitFor(() => expect(router.state.location.search).toBe('?h=75'));
    fireEvent.change(slider, { target: { value: '80' } });
    fireEvent.keyUp(slider, { key: 'ArrowRight' });
    await waitFor(() => expect(router.state.location.search).toBe('?h=80'));
    expect(slider.getAttribute('aria-valuetext')).toMatch(/^80, weights times \d\.\d\d$/);

    const chips = await screen.findByRole('list', { name: 'Changed settings' });
    expect(within(chips).getByText(/HAWKISHNESS 80/)).toBeTruthy();
    // every dial explains itself: an info button beside its name (the glossary's definition)
    fireEvent.click(screen.getByRole('button', { name: 'What is Hawkishness?' }));
    expect(screen.getByRole('dialog', { name: 'Hawkishness' }).textContent).toMatch(/Security first/);
    fireEvent.keyDown(document, { key: 'Escape' });
    expect(screen.queryByRole('dialog', { name: 'Hawkishness' })).toBeNull();
    // Show is enabled away from the baseline
    const change = screen.getByRole('radio', { name: 'Change' }) as HTMLInputElement;
    expect(change.disabled).toBe(false);
    fireEvent.click(change);
    await waitFor(() => expect(router.state.location.search).toBe('?h=80&show=change'));

    await waitFor(() => expect((screen.getByRole('button', { name: /reset to neis baseline/i }) as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(screen.getByRole('button', { name: /reset to neis baseline/i }));
    await waitFor(() => expect(router.state.location.search).toBe('?show=change'));
    expect(screen.getByText('NEIS baseline', { selector: '.tag' })).toBeTruthy();
    expect((screen.getByRole('radio', { name: 'Change' }) as HTMLInputElement).disabled).toBe(true);
    await settle(300);
  }, 60_000);

  it('a preset sets all four dials; invalid link values are reported', async () => {
    const router = renderAt('/overview?h=140&zzz=1');
    expect(await screen.findByText(/Ignored in the link/, {}, { timeout: 20_000 })).toBeTruthy();
    await waitFor(() => expect(router.state.location.search).toBe('?h=100'));
    const preset = screen.getByLabelText('Preset') as HTMLSelectElement;
    const options = [...preset.options].map((o) => o.value).filter(Boolean);
    expect(options.length).toBeGreaterThanOrEqual(5);
    const pick = options.find((o) => !/paper/i.test(o)) as string;
    fireEvent.change(preset, { target: { value: pick } });
    await waitFor(() => expect(router.state.location.search).not.toBe('?h=100'));
    expect(router.state.location.search).toMatch(/^\?([hbpg]=\d+&?)+$/);
    await settle(300);
  }, 60_000);

  it('the endowment screen has its own chip, and the chip resets it', async () => {
    // the synthetic bundle applies the screen (the paper's default): ?screen=0 moves it
    const router = renderAt('/strategic?screen=0');
    expect(await screen.findByRole('heading', { level: 1, name: /^Strategic Index$/ }, { timeout: 20_000 })).toBeTruthy();
    const chips = await screen.findByRole('list', { name: 'Changed settings' }, { timeout: 20_000 });
    expect(within(chips).getByText(/^Endowment screen off/)).toBeTruthy();
    fireEvent.click(within(chips).getByRole('button', { name: /reset endowment screen off/i }));
    await waitFor(() => expect(router.state.location.search).toBe(''));
    expect(screen.getByText('NEIS baseline', { selector: '.tag' })).toBeTruthy();
    await settle(300);
  }, 60_000);
});

describe('data downloads', () => {
  it('builds the zip of the current scenario in the browser', async () => {
    const blobs: Blob[] = [];
    const cu = URL.createObjectURL;
    const ru = URL.revokeObjectURL;
    URL.createObjectURL = ((b: Blob) => (blobs.push(b), 'blob:test')) as typeof URL.createObjectURL;
    URL.revokeObjectURL = (() => undefined) as typeof URL.revokeObjectURL;
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);
    try {
      renderAt('/data?h=80');
      const btn = await screen.findByRole('button', { name: /all tables/i }, { timeout: 20_000 });
      fireEvent.click(btn);
      await waitFor(() => expect(blobs.length).toBe(1), { timeout: 20_000 });
      const { unzipSync, strFromU8 } = await import('fflate');
      // (jsdom's Blob has no arrayBuffer())
      const buf = await new Promise<ArrayBuffer>((res, rej) => {
        const fr = new FileReader();
        fr.onload = () => res(fr.result as ArrayBuffer);
        fr.onerror = () => rej(fr.error);
        fr.readAsArrayBuffer(blobs[0]);
      });
      const files = unzipSync(new Uint8Array(buf));
      const names = Object.keys(files);
      for (const f of ['README.txt', 'parameters.csv', 'data_vintage.csv', 'strategic_index.csv', 'energy_security.csv']) expect(names).toContain(f);
      // UTF-8 with a byte-order mark (Excel reads the accents right)
      expect([...files['parameters.csv'].slice(0, 3)]).toEqual([0xef, 0xbb, 0xbf]);
      expect(strFromU8(files['strategic_index.csv'])).toMatch(/strategic_index/);
      expect(strFromU8(files['README.txt'])).toMatch(/Hawkishness 80/);
      expect(screen.getByText(/Downloaded \d+ tables/)).toBeTruthy();
    } finally {
      URL.createObjectURL = cu;
      // jsdom has no revokeObjectURL; download.ts revokes on a 1 s timer that outlives this test
      URL.revokeObjectURL = ru ?? ((() => undefined) as typeof URL.revokeObjectURL);
      click.mockRestore();
    }
  }, 60_000);
});

// ---- the shell: Home, redirects, module tabs (spec_sector.md 2, 10.3) -----------------------------

/** A client whose worker never answers: the engine stays 'starting' for good. */
function neverReadyClient(): EngineClient {
  return new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/');
}

const pathOf = (router: ReturnType<typeof renderAt>) => router.state.location.pathname + router.state.location.search;

/** The top tabs (IA refactor, 2026-10-16). */
const TABS = ['Sectors', 'Countries', 'Allied Industrial Base', 'About'];

describe('home and the shell', () => {
  it('bare / renders Home: no engine client, no request before idle; the idle prefetch fills the facts', async () => {
    setEngineClient(null);
    resetPrefetch();
    const create = vi.spyOn(EngineClient, 'create');
    const idle: (() => void)[] = [];
    const w = window as unknown as { requestIdleCallback?: (cb: () => void) => number; cancelIdleCallback?: (id: number) => void };
    w.requestIdleCallback = (cb) => idle.push(cb);
    w.cancelIdleCallback = () => undefined;
    const manifest = sectorManifestFixture();
    const fetchSpy = vi.fn(async (url: RequestInfo | URL, init?: RequestInit) => {
      void init;
      const u = String(url);
      if (u.endsWith('data/sector/manifest.json')) return new Response(JSON.stringify(manifest), { status: 200, headers: { 'Content-Type': 'application/json' } });
      return new Response(new Uint8Array([0x1f, 0x8b, 0]), { status: 200 });
    });
    const realFetch = globalThis.fetch;
    globalThis.fetch = fetchSpy as unknown as typeof fetch;
    try {
      const router = renderAt('/');
      expect(await screen.findByRole('heading', { level: 1, name: 'Explore the electro-industrial system' }, { timeout: 20_000 })).toBeTruthy();
      expect(pathOf(router)).toBe('/');
      expect(document.title).toBe('Allied Industrial Base Explorer | NEIS Center');
      // three entry cards (a Sector, a Country, the Allied Industrial Base) with the brief's CTAs to the
      // landing pages, the jump pickers, the questions; no tab is current on Home
      const cards = screen.getByRole('region', { name: 'Start here' });
      expect(within(cards).getAllByRole('heading', { level: 2 }).map((h) => h.textContent)).toEqual(['Explore a Sector', 'Explore a Country', 'Explore the Allied Industrial Base']);
      expect(within(cards).getAllByRole('link', { name: /^Explore / }).map((a) => [a.textContent, a.getAttribute('href')])).toEqual([
        ['Explore sectors', '/sector'],
        ['Explore countries', '/countries'],
        ['Explore AIB', '/aib'],
      ]);
      expect(screen.queryByText(/Strategic Insights/)).toBeNull();
      expect(screen.queryByText(/How the modules fit together/)).toBeNull();
      expect(screen.getAllByRole('heading', { level: 3 }).length).toBe(6);
      const tabs = screen.getByRole('navigation', { name: 'Modules' });
      expect(within(tabs).getAllByRole('link').map((a) => a.textContent)).toEqual(TABS);
      expect(within(tabs).getAllByRole('link').filter((a) => a.getAttribute('aria-current')).length).toBe(0);
      // the Country card: its question, entry links and the button
      expect(screen.getByText("What is this country's strategic position?")).toBeTruthy();
      expect(screen.getByRole('link', { name: 'Japan' }).getAttribute('href')).toBe('/countries/JPN');
      expect(screen.getByRole('link', { name: 'Germany' }).getAttribute('href')).toBe('/countries/DEU');
      expect(screen.getByRole('link', { name: 'United States: policy memo' }).getAttribute('href')).toBe('/countries/USA/memo');
      expect(screen.getByTestId('quadrant-glyph').getAttribute('aria-hidden')).toBe('true');
      expect(screen.getByRole('link', { name: 'Which supply chains justify a sectoral compact?' }).getAttribute('href')).toBe('/compacts');
      expect(screen.getByRole('link', { name: 'glossary' }).getAttribute('href')).toBe('/glossary');
      expect(bareAcronyms(document.body)).toEqual([]);
      await settle(300);
      expect(fetchSpy).not.toHaveBeenCalled();
      expect(create).not.toHaveBeenCalled();
      expect(peekEngineClient()).toBeNull();
      expect(screen.queryByTestId('coverage-glyph')).toBeNull();
      // idle: the Sector manifest (no-cache), then the sector-meta bytes; the country picker's options
      expect(idle.length).toBe(2);
      await act(async () => {
        for (const f of idle) f();
        await new Promise((r) => setTimeout(r, 50));
      });
      // (this test's Country manifest is unreadable: the country picker gives way to a link)
      expect(await screen.findByRole('link', { name: 'All countries' }, { timeout: 20_000 })).toBeTruthy();
      const sectorCall = fetchSpy.mock.calls.find((c) => /\/data\/sector\/manifest\.json$/.test(String(c[0])));
      expect(sectorCall?.[1]).toEqual({ cache: 'no-cache' });
      // the Country manifest is prefetched on idle too (A2.3), never a fact sheet
      await waitFor(() => expect(fetchSpy.mock.calls.some((c) => /\/data\/country\/manifest\.json$/.test(String(c[0])))).toBe(true), { timeout: 20_000 });
      expect(fetchSpy.mock.calls.some((c) => /\/data\/country\/facts-/.test(String(c[0])))).toBe(false);
      await waitFor(() => expect(fetchSpy.mock.calls.some((c) => /sector-meta\.0123abcd\.osb$/.test(String(c[0])))).toBe(true), { timeout: 20_000 });
      expect(await screen.findByText("China refines 74% of the world's lithium")).toBeTruthy();
      expect(screen.getByText('IEA Critical Minerals Dataset 2026, 2025')).toBeTruthy();
      const supply = screen.getByRole('link', { name: 'Who controls supply?' });
      expect(supply.getAttribute('href')).toBe('/sector/minerals/lithium');
      expect(supply.getAttribute('aria-describedby')).toBeTruthy();
      // a headline of another rule version shows no fact
      expect(screen.getByRole('link', { name: 'Where are the reserves?' }).getAttribute('aria-describedby')).toBeNull();
      const n = manifest.coverage_glyph.filter((g) => g.country_data).length;
      expect(screen.getByText(`Country data for ${n} of 33 technology-stage cells`)).toBeTruthy();
      expect(screen.getByTestId('coverage-glyph').children.length).toBe(33);
      expect(create).not.toHaveBeenCalled();
      expect(screen.getByText(/Sector data: export 2026-10-10 18:00 UTC/)).toBeTruthy();
    } finally {
      globalThis.fetch = realFetch;
      delete w.requestIdleCallback;
      delete w.cancelIdleCallback;
      create.mockRestore();
      resetSectorManifestCache();
      resetBuildInfo();
      resetPrefetch();
      setEngineClient(client);
    }
  }, 60_000);

  it('the jump picker opens a sector profile and a mineral page', async () => {
    setEngineClient(null);
    try {
      const router = renderAt('/');
      const tech = (await screen.findByLabelText('Technology', {}, { timeout: 20_000 })) as HTMLSelectElement;
      fireEvent.change(tech, { target: { value: 'solar' } });
      fireEvent.click(screen.getByRole('radio', { name: 'Upstream' }));
      fireEvent.click(screen.getByRole('button', { name: 'Go to Solar Upstream' }));
      await waitFor(() => expect(router.state.location.pathname).toBe('/sector/solar/upstream'), { timeout: 20_000 });
      await act(async () => {
        await router.navigate('/');
      });
      // Semiconductors is Midstream only
      fireEvent.change(await screen.findByLabelText('Technology', {}, { timeout: 20_000 }), { target: { value: 'semiconductors' } });
      expect((screen.getByRole('radio', { name: 'Upstream' }) as HTMLInputElement).disabled).toBe(true);
      fireEvent.click(screen.getByRole('button', { name: 'Go to Semiconductors Midstream' }));
      await waitFor(() => expect(router.state.location.pathname).toBe('/sector/semiconductors/midstream'), { timeout: 20_000 });
      await act(async () => {
        await router.navigate('/');
      });
      fireEvent.change(await screen.findByLabelText('Mineral', {}, { timeout: 20_000 }), { target: { value: 'rare-earths' } });
      fireEvent.click(screen.getByRole('button', { name: 'Go to Rare earths' }));
      await waitFor(() => expect(router.state.location.pathname).toBe('/sector/minerals/rare-earths'), { timeout: 20_000 });
      expect(peekEngineClient()).toBeNull();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);

  it('/?h=80 lands on /overview?h=80', async () => {
    const router = renderAt('/?h=80');
    expect(await screen.findByRole('heading', { level: 1, name: /^Overview$/ }, { timeout: 20_000 })).toBeTruthy();
    expect(pathOf(router)).toBe('/overview?h=80');
    expect(screen.getByText('Custom perspective', { selector: '.tag' })).toBeTruthy();
    await settle(300);
  }, 60_000);

  it("old one-economy Strategic links open that section of the Country profile; the bare paths stay", async () => {
    setCountryStoreForTests(new CountryStore(fixtureSource()));
    try {
      for (const [from, to] of [
        ['/country/USA', '/countries/USA#sectors'],
        ['/strategic/jpn', '/countries/JPN#opportunities'],
        ['/partners/DEU?h=80', '/countries/DEU?h=80#partnerships'],
      ]) {
        const router = renderAt(from);
        await waitFor(() => expect(pathOf(router) + router.state.location.hash).toBe(to), { timeout: 20_000 });
        const tabs = screen.getByRole('navigation', { name: 'Modules' });
        expect(within(tabs).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('true');
        expect(within(screen.getByRole('navigation', { name: 'Country views' })).getByRole('link', { name: 'Country profile' }).getAttribute('aria-current')).toBe('page');
        await settle(300);
        cleanup();
      }
      // the bare legacy path is still the advanced page, filed under Countries
      renderAt('/country');
      expect(await screen.findByRole('heading', { level: 1, name: /^Country (scores|profile)/ }, { timeout: 20_000 })).toBeTruthy();
      expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('true');
      await settle(300);
    } finally {
      setCountryStoreForTests(null);
    }
  }, 90_000);

  it('an unknown path renders Home with the notice', async () => {
    setEngineClient(null);
    try {
      const router = renderAt('/no-such-page?h=80');
      expect(await screen.findByRole('heading', { level: 1, name: 'Explore the electro-industrial system' }, { timeout: 20_000 })).toBeTruthy();
      expect(pathOf(router)).toBe('/');
      expect(screen.getByText('That page does not exist; showing the home page.')).toBeTruthy();
      fireEvent.click(screen.getByRole('button', { name: 'Dismiss' }));
      expect(screen.queryByText('That page does not exist; showing the home page.')).toBeNull();
      expect(peekEngineClient()).toBeNull();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);

  it('the retired allied network page: /network and /network/:stage go to the AIB overview with a notice', async () => {
    for (const path of ['/network', '/network/batteries-midstream?h=80']) {
      const router = renderAt(path);
      await waitFor(() => expect(router.state.location.pathname).toBe('/aib'), { timeout: 20_000 });
      expect(await screen.findByText(/The original allied network design has been retired/, {}, { timeout: 20_000 })).toBeTruthy();
      cleanup();
    }
    await settle(300);
  }, 60_000);

  it('aliases: /sector-insights and /sectors go to Sectors; /strategic-insights to Countries', async () => {
    const r1 = renderAt('/sector-insights?c=USA');
    await waitFor(() => expect(pathOf(r1)).toBe('/sector?c=USA'), { timeout: 20_000 });
    cleanup();
    const r0 = renderAt('/sectors');
    await waitFor(() => expect(pathOf(r0)).toBe('/sector'), { timeout: 20_000 });
    cleanup();
    const r2 = renderAt('/strategic-insights?h=80');
    await waitFor(() => expect(pathOf(r2)).toBe('/countries?h=80'), { timeout: 20_000 });
    await settle(300);
  }, 60_000);

  it('/about: the About landing links Methods, Data & sources and the Glossary; About is current; no engine', async () => {
    setEngineClient(null);
    try {
      renderAt('/about');
      expect(await screen.findByRole('heading', { level: 1, name: 'About' }, { timeout: 20_000 })).toBeTruthy();
      const tabs = screen.getByRole('navigation', { name: 'Modules' });
      expect(within(tabs).getByRole('link', { name: 'About' }).getAttribute('aria-current')).toBe('true');
      const pages = screen.getByRole('navigation', { name: 'About pages' });
      expect(within(pages).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(['/about', '/methods', '/data', '/glossary']);
      expect(within(screen.getByRole('main')).getByRole('link', { name: 'Methods' }).getAttribute('href')).toBe('/methods');
      expect(bareAcronyms(document.body)).toEqual([]);
      expect(peekEngineClient()).toBeNull();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);

  it('the tabs carry the country from a legacy Strategic page; the About pages carry the scenario', async () => {
    // (the synthetic bundle's technologies are not the Sector slugs: the tech / stage carry is
    // unit-tested in shell/lastLocation.test.ts)
    const router = renderAt('/overview?h=80');
    expect(await screen.findByRole('heading', { level: 1, name: /^Overview$/ }, { timeout: 20_000 })).toBeTruthy();
    await settle(200);
    const tabs = () => screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs()).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('true');
    expect(within(tabs()).getByRole('link', { name: 'Sectors' }).getAttribute('href')).toBe('/sector');
    fireEvent.click(within(tabs()).getByRole('link', { name: 'Sectors' }));
    await waitFor(() => expect(pathOf(router)).toBe('/sector'), { timeout: 20_000 });
    expect(within(tabs()).getByRole('link', { name: 'Sectors' }).getAttribute('aria-current')).toBe('true');
    expect(screen.getByRole('navigation', { name: 'Sector views' })).toBeTruthy();
    await act(async () => {
      await router.navigate('/sector/batteries/midstream');
    });
    // PROFILE follows the last visited cell
    await waitFor(() =>
      expect(within(screen.getByRole('navigation', { name: 'Sector views' })).getByRole('link', { name: 'Sector profile' }).getAttribute('aria-current')).toBe('page'),
    );
    // an old Strategic country link opens the Country profile, which carries the country as c, and
    // Home's buttons resume both modules
    setCountryStoreForTests(new CountryStore(fixtureSource()));
    await act(async () => {
      await router.navigate('/country/USA');
    });
    await waitFor(() => expect(pathOf(router)).toBe('/countries/USA'), { timeout: 20_000 });
    await waitFor(() => expect(within(tabs()).getByRole('link', { name: 'Sectors' }).getAttribute('href')).toBe('/sector/batteries/midstream?c=USA'));
    fireEvent.click(screen.getByRole('link', { name: 'Allied Industrial Base Explorer' }));
    await waitFor(() => expect(pathOf(router)).toBe('/'), { timeout: 20_000 });
    // ... and from elsewhere the Countries tab reopens the same country (A2.2)
    expect(within(tabs()).getByRole('link', { name: 'Countries' }).getAttribute('href')).toBe('/countries/USA');
    // Home's CTAs open the landing pages; the tabs resume where the reader was
    expect((await screen.findByRole('link', { name: 'Explore sectors' }, { timeout: 20_000 })).getAttribute('href')).toBe('/sector');
    expect(within(tabs()).getByRole('link', { name: 'Sectors' }).getAttribute('href')).toBe('/sector/batteries/midstream');
    // the About pages carry the scenario between Methods and Data
    await act(async () => {
      await router.navigate('/data?h=80');
    });
    expect(await screen.findByRole('heading', { level: 1, name: /^Data$/ }, { timeout: 20_000 })).toBeTruthy();
    const views = screen.getByRole('navigation', { name: 'About pages' });
    expect(within(views).getByRole('link', { name: 'Methods' }).getAttribute('href')).toBe('/methods?h=80');
    expect(within(views).getByRole('link', { name: 'Data & sources' }).getAttribute('aria-current')).toBe('page');
    await settle(300);
  }, 60_000);

  it('Sector renders while the engine never becomes ready; Strategic waits in content under the header', async () => {
    const stuck = neverReadyClient();
    setEngineClient(stuck);
    try {
      const router = renderAt('/sector');
      await waitFor(() => expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'Sectors' }).getAttribute('aria-current')).toBe('true'), { timeout: 20_000 });
      await settle(200);
      expect(screen.getByRole('main')).toBeTruthy();
      expect(screen.queryByText(/Loading the model/)).toBeNull();
      expect(stuck.getSnapshot().status).toBe('starting');
      // the engine gate is in content: the header and the footer stay
      await act(async () => {
        await router.navigate('/overview?h=80');
      });
      expect(await screen.findByText(/Loading the model/, {}, { timeout: 20_000 })).toBeTruthy();
      expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('true');
      expect(screen.getByRole('navigation', { name: 'Country views' })).toBeTruthy();
      expect(screen.getByRole('contentinfo')).toBeTruthy();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);

  it('Countries: the alias, the tab and its views nav; the engine never starts; tabs carry the country', async () => {
    const stuck = neverReadyClient();
    setEngineClient(stuck);
    // no country data in this test: the module shows its "No country data" notice under the shell
    setCountryStoreForTests(
      new CountryStore({
        manifest: async () => {
          throw new CountryLoadError('NO_DATA', 'data/country/manifest.json does not exist (HTTP 404).');
        },
        json: async () => {
          throw new CountryLoadError('NETWORK', 'no file');
        },
        url: (f) => `/data/country/${f}`,
      }),
    );
    try {
      const router = renderAt('/country-insights');
      await waitFor(() => expect(pathOf(router)).toBe('/countries'), { timeout: 20_000 });
      const tabs = () => screen.getByRole('navigation', { name: 'Modules' });
      await waitFor(() => expect(within(tabs()).getByRole('link', { name: 'Countries' }).getAttribute('aria-current')).toBe('true'), { timeout: 20_000 });
      expect(within(tabs()).getByRole('link', { name: 'Countries' }).getAttribute('href')).toBe('/countries');
      expect(await screen.findByText('No country data', {}, { timeout: 20_000 })).toBeTruthy();
      const views = screen.getByRole('navigation', { name: 'Country views' });
      expect(within(views).getAllByRole('link').map((a) => a.textContent)).toEqual(['All countries', 'Country profile']);
      expect(within(views).getByRole('link', { name: 'All countries' }).getAttribute('aria-current')).toBe('page');
      expect(stuck.getSnapshot().status).toBe('starting');
      // a profile: PROFILE is current; SECTORS carries the country; the AIB tab opens its overview
      await act(async () => {
        await router.navigate('/countries/jpn');
      });
      await waitFor(() => expect(within(screen.getByRole('navigation', { name: 'Country views' })).getByRole('link', { name: 'Country profile' }).getAttribute('aria-current')).toBe('page'));
      expect(within(screen.getByRole('navigation', { name: 'Country views' })).getByRole('link', { name: 'Country profile' }).getAttribute('href')).toBe('/countries/JPN');
      expect(within(tabs()).getByRole('link', { name: 'Sectors' }).getAttribute('href')).toBe('/sector?c=JPN');
      expect(within(tabs()).getByRole('link', { name: 'Allied Industrial Base' }).getAttribute('href')).toBe('/aib');
      // Sector with a highlighted country -> the Country tab opens that country
      await act(async () => {
        await router.navigate('/sector?c=DEU');
      });
      await waitFor(() => expect(within(tabs()).getByRole('link', { name: 'Countries' }).getAttribute('href')).toBe('/countries/DEU'));
      expect(bareAcronyms(screen.getByRole('banner'))).toEqual([]);
      expect(stuck.getSnapshot().status).toBe('starting');
    } finally {
      setCountryStoreForTests(null);
      setEngineClient(client);
    }
  }, 60_000);

  it('/glossary: a shell page reachable from the footer, filed under About, no engine', async () => {
    setEngineClient(null);
    try {
      const router = renderAt('/sector');
      const footer = await screen.findByRole('contentinfo', {}, { timeout: 20_000 });
      expect(within(footer).getByRole('link', { name: 'Country data notes' }).getAttribute('href')).toBe('/countries/about');
      expect(within(footer).queryByText(/Insights/)).toBeNull();
      fireEvent.click(within(footer).getByRole('link', { name: 'Glossary' }));
      await waitFor(() => expect(pathOf(router)).toBe('/glossary'), { timeout: 20_000 });
      expect(await screen.findByRole('heading', { level: 1, name: 'Glossary' }, { timeout: 20_000 })).toBeTruthy();
      expect(document.title).toBe('Glossary | Allied Industrial Base Explorer | NEIS Center');
      const tabs = screen.getByRole('navigation', { name: 'Modules' });
      expect(within(tabs).getAllByRole('link').filter((a) => a.getAttribute('aria-current')).map((a) => a.textContent)).toEqual(['About']);
      expect(document.getElementById('term-psi')).toBeTruthy();
      expect(bareAcronyms(document.body)).toEqual([]);
      expect(peekEngineClient()).toBeNull();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);
});

describe('EngineScope (IA refactor)', () => {
  function Probe() {
    const { setDial, scenario, isBaseline } = useScenario();
    return (
      <button type="button" onClick={() => setDial('hawkishness', 80, { replace: false })}>
        {isBaseline ? 'baseline' : 'custom'} {scenario.dials.hawkishness}
      </button>
    );
  }

  it("embeds the engine without the dial chrome; the host page's keys and hash survive a dial move", async () => {
    const router = createMemoryRouter(
      [{ path: '/host', element: <EngineScope hostKeys={['cell', 'all']}><Probe /></EngineScope> }],
      { initialEntries: ['/host?cell=solar-midstream&all=1#why'] },
    );
    render(<RouterProvider router={router} />);
    fireEvent.click(await screen.findByRole('button', { name: 'baseline 50' }, { timeout: 20_000 }));
    await waitFor(() => expect(router.state.location.search).toBe('?h=80&cell=solar-midstream&all=1'));
    expect(router.state.location.hash).toBe('#why');
    expect(screen.getByRole('button', { name: 'custom 80' })).toBeTruthy();
    // no "unknown setting" rewrite of the host keys, and no Strategic chrome
    expect(screen.queryByRole('complementary', { name: 'Scenario dials' })).toBeNull();
    await settle(200);
    expect(router.state.location.search).toBe('?h=80&cell=solar-midstream&all=1');
  }, 60_000);
});

describe('Home search and the About layout (IA refactor)', () => {
  it("Home's country picker opens a Country profile (the manifest loads on idle; no engine)", async () => {
    setEngineClient(null);
    setCountryStoreForTests(new CountryStore(fixtureSource()));
    try {
      const router = renderAt('/');
      const input = (await screen.findByRole('combobox', { name: 'Country' }, { timeout: 20_000 })) as HTMLInputElement;
      // the options arrive with the idle prefetch (the 1.5 s fallback under jsdom). Open the list once and
      // wait without touching the DOM: a waitFor callback that fires events re-runs on its own mutations
      // (MutationObserver), starves the timers and never ends.
      fireEvent.focus(input);
      expect(await screen.findByRole('option', { name: /Japan/ }, { timeout: 20_000 })).toBeTruthy();
      fireEvent.change(input, { target: { value: 'Japan' } });
      await waitFor(() => expect(within(screen.getByRole('listbox', { name: 'Country' })).getAllByRole('option').map((o) => o.textContent)).toEqual(['Japan JPN']));
      fireEvent.keyDown(input, { key: 'Enter' });
      fireEvent.click(await screen.findByRole('button', { name: 'Go to Japan' }));
      await waitFor(() => expect(router.state.location.pathname).toBe('/countries/JPN'), { timeout: 20_000 });
      expect(peekEngineClient()).toBeNull();
    } finally {
      setCountryStoreForTests(null);
      setEngineClient(client);
    }
  }, 60_000);

  it('/data with a custom perspective: one line names it, Reset returns to the NEIS baseline; no dial panel', async () => {
    const router = renderAt('/data?h=80&g=70');
    expect(await screen.findByRole('heading', { level: 1, name: /^Data$/ }, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText('Custom perspective', { selector: '.tag' })).toBeTruthy();
    expect(screen.getByText(/Hawkishness 80, Greenness 70|Hawkishness 80, Climate 70/)).toBeTruthy();
    expect(screen.queryByRole('complementary', { name: 'Scenario dials' })).toBeNull();
    expect(screen.queryByLabelText('HAWKISHNESS')).toBeNull();
    // filed under About, with the scenario carried between the About pages
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs).getByRole('link', { name: 'About' }).getAttribute('aria-current')).toBe('true');
    fireEvent.click(screen.getByRole('button', { name: 'Reset to NEIS baseline' }));
    await waitFor(() => expect(router.state.location.search).toBe(''));
    expect(screen.getByText('NEIS baseline', { selector: '.tag' })).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Reset to NEIS baseline' })).toBeNull();
    await settle(300);
  }, 60_000);
});

