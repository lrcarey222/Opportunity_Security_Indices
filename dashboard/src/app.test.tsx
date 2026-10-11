// UI smoke and behaviour tests (jsdom, spec_react.md 10.3 project `ui`): the whole app on the
// committed synthetic bundle (scripts/86, s1) with the engine host running in process instead of a
// Web Worker. Every view renders without an error at the baseline and in a scenario; the dial panel
// writes the URL (keyboard and pointer), the status strip and reset follow.
//
// The shell (spec_sector.md 2, 10.3): Home renders without an engine client and without a request
// before idle; legacy root links redirect; unknown paths go Home with a notice; aliases; the module
// tabs restore the last Strategic URL with its scenario; Sector renders while the engine never starts.

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { peekEngineClient, setEngineClient } from './state/EngineProvider.tsx';
import { routes } from './routes.tsx';
import { resetLastLocations } from './shell/lastLocation.ts';
import { resetBuildInfo } from './shell/buildInfo.ts';
import { resetSectorManifestCache } from './shell/sectorManifest.ts';
import { sectorManifestFixture } from './shell/__tests__/manifestFixture.ts';
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

const VIEWS: [string, RegExp][] = [
  ['/overview', /^Overview$/],
  ['/country', /^Country profile/],
  ['/space', /opportunity.security space/i],
  ['/strategic', /^Strategic Index$/],
  ['/partners', /^Partners$/],
  ['/network', /^Allied network$/],
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
        await settle(200);
        expect(screen.queryByText(/This view failed to render/)).toBeNull();
        expect(screen.queryByText(/Recomputed values differ from the export/)).toBeNull();
        // the charts drew (Plot and the d3 maps render into the jsdom document)
        if (process.env.UI_TEST_VERBOSE) console.log(path, screen.getByRole('main').textContent?.slice(0, 1500));
        if (!/methods|data|strategic/.test(path)) expect(screen.getByRole('main').querySelectorAll('figure svg').length).toBeGreaterThan(0);
        expect(screen.getByText(q ? 'Scenario' : 'Paper baseline', { selector: '.tag' })).toBeTruthy();
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
    expect(slider.getAttribute('aria-valuetext')).toBe("50, the paper's setting");
    const reset = screen.getByRole('button', { name: /reset to paper baseline/i });
    expect((reset as HTMLButtonElement).disabled).toBe(true);

    fireEvent.keyDown(slider, { key: 'PageUp' });
    await waitFor(() => expect(router.state.location.search).toBe('?h=75'));
    fireEvent.change(slider, { target: { value: '80' } });
    fireEvent.keyUp(slider, { key: 'ArrowRight' });
    await waitFor(() => expect(router.state.location.search).toBe('?h=80'));
    expect(slider.getAttribute('aria-valuetext')).toMatch(/^80, weights times \d\.\d\d$/);

    const chips = await screen.findByRole('list', { name: 'Changed settings' });
    expect(within(chips).getByText(/HAWKISHNESS 80/)).toBeTruthy();
    // Show is enabled away from the baseline
    const change = screen.getByRole('radio', { name: 'Change' }) as HTMLInputElement;
    expect(change.disabled).toBe(false);
    fireEvent.click(change);
    await waitFor(() => expect(router.state.location.search).toBe('?h=80&show=change'));

    await waitFor(() => expect((screen.getByRole('button', { name: /reset to paper baseline/i }) as HTMLButtonElement).disabled).toBe(false));
    fireEvent.click(screen.getByRole('button', { name: /reset to paper baseline/i }));
    await waitFor(() => expect(router.state.location.search).toBe('?show=change'));
    expect(screen.getByText('Paper baseline', { selector: '.tag' })).toBeTruthy();
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
    expect(screen.getByText('Paper baseline', { selector: '.tag' })).toBeTruthy();
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

describe('home and the shell', () => {
  it('bare / renders Home: no engine client, no request before idle; the idle prefetch fills the facts', async () => {
    setEngineClient(null);
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
      expect(await screen.findByRole('heading', { level: 1, name: 'Allied Industrial Base Explorer' }, { timeout: 20_000 })).toBeTruthy();
      expect(pathOf(router)).toBe('/');
      expect(document.title).toBe('Allied Industrial Base Explorer | NEIS Center');
      // module cards, six questions, the jump picker; no tab is current on Home
      expect(screen.getByRole('heading', { level: 2, name: 'Sector Insights' })).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: 'Strategic Insights' })).toBeTruthy();
      expect(screen.getAllByRole('heading', { level: 3 }).length).toBe(6);
      const tabs = screen.getByRole('navigation', { name: 'Modules' });
      expect(within(tabs).getAllByRole('link').filter((a) => a.getAttribute('aria-current')).length).toBe(0);
      await settle(300);
      expect(fetchSpy).not.toHaveBeenCalled();
      expect(create).not.toHaveBeenCalled();
      expect(peekEngineClient()).toBeNull();
      expect(screen.queryByTestId('coverage-glyph')).toBeNull();
      // idle: the manifest (no-cache), then the sector-meta bytes
      expect(idle.length).toBe(1);
      await act(async () => {
        idle[0]();
        await new Promise((r) => setTimeout(r, 50));
      });
      expect(String(fetchSpy.mock.calls[0][0])).toMatch(/\/data\/sector\/manifest\.json$/);
      expect(fetchSpy.mock.calls[0][1]).toEqual({ cache: 'no-cache' });
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
    expect(screen.getByText('Scenario', { selector: '.tag' })).toBeTruthy();
    await settle(300);
  }, 60_000);

  it('/country/USA still renders the Country view', async () => {
    const router = renderAt('/country/USA');
    expect(await screen.findByRole('heading', { level: 1, name: /^Country profile/ }, { timeout: 20_000 })).toBeTruthy();
    expect(pathOf(router)).toBe('/country/USA');
    const tabs = screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs).getByRole('link', { name: 'Strategic Insights' }).getAttribute('aria-current')).toBe('true');
    expect(screen.getByRole('navigation', { name: 'Strategic Insights views' })).toBeTruthy();
    await settle(300);
  }, 60_000);

  it('an unknown path renders Home with the notice', async () => {
    setEngineClient(null);
    try {
      const router = renderAt('/no-such-page?h=80');
      expect(await screen.findByRole('heading', { level: 1, name: 'Allied Industrial Base Explorer' }, { timeout: 20_000 })).toBeTruthy();
      expect(pathOf(router)).toBe('/');
      expect(screen.getByText('That page does not exist; showing the home page.')).toBeTruthy();
      fireEvent.click(screen.getByRole('button', { name: 'Dismiss' }));
      expect(screen.queryByText('That page does not exist; showing the home page.')).toBeNull();
      expect(peekEngineClient()).toBeNull();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);

  it('aliases: /sector-insights and /strategic-insights redirect', async () => {
    const r1 = renderAt('/sector-insights?c=USA');
    await waitFor(() => expect(pathOf(r1)).toBe('/sector?c=USA'), { timeout: 20_000 });
    cleanup();
    const r2 = renderAt('/strategic-insights?h=80');
    await waitFor(() => expect(pathOf(r2)).toBe('/overview?h=80'), { timeout: 20_000 });
    expect(await screen.findByRole('heading', { level: 1, name: /^Overview$/ }, { timeout: 20_000 })).toBeTruthy();
    await settle(300);
  }, 60_000);

  it('the module tabs restore the last Strategic URL with its scenario and carry the country', async () => {
    // (the synthetic bundle's technologies are not the Sector slugs: the tech / stage carry is
    // unit-tested in shell/lastLocation.test.ts)
    const router = renderAt('/overview?h=80');
    expect(await screen.findByRole('heading', { level: 1, name: /^Overview$/ }, { timeout: 20_000 })).toBeTruthy();
    await settle(200);
    const tabs = () => screen.getByRole('navigation', { name: 'Modules' });
    expect(within(tabs()).getByRole('link', { name: 'Sector Insights' }).getAttribute('href')).toBe('/sector');
    fireEvent.click(within(tabs()).getByRole('link', { name: 'Sector Insights' }));
    await waitFor(() => expect(pathOf(router)).toBe('/sector'), { timeout: 20_000 });
    expect(within(tabs()).getByRole('link', { name: 'Sector Insights' }).getAttribute('aria-current')).toBe('true');
    expect(screen.getByRole('navigation', { name: 'Sector Insights views' })).toBeTruthy();
    await act(async () => {
      await router.navigate('/sector/batteries/midstream');
    });
    // PROFILE follows the last visited cell
    await waitFor(() =>
      expect(within(screen.getByRole('navigation', { name: 'Sector Insights views' })).getByRole('link', { name: 'Profile' }).getAttribute('aria-current')).toBe('page'),
    );
    // Sector -> Strategic: the last Strategic location, scenario kept
    expect(within(tabs()).getByRole('link', { name: 'Strategic Insights' }).getAttribute('href')).toBe('/overview?h=80');
    fireEvent.click(within(tabs()).getByRole('link', { name: 'Strategic Insights' }));
    await waitFor(() => expect(pathOf(router)).toBe('/overview?h=80'), { timeout: 20_000 });
    expect(await screen.findByRole('heading', { level: 1, name: /^Overview$/ }, { timeout: 20_000 })).toBeTruthy();
    expect(screen.getByText('Scenario', { selector: '.tag' })).toBeTruthy();
    // a country page carries the country as c, and Home's buttons resume both modules
    await act(async () => {
      await router.navigate('/country/USA?h=80');
    });
    expect(await screen.findByRole('heading', { level: 1, name: /^Country profile/ }, { timeout: 20_000 })).toBeTruthy();
    expect(within(tabs()).getByRole('link', { name: 'Sector Insights' }).getAttribute('href')).toBe('/sector/batteries/midstream?c=USA');
    fireEvent.click(screen.getByRole('link', { name: 'Allied Industrial Base Explorer' }));
    await waitFor(() => expect(pathOf(router)).toBe('/'), { timeout: 20_000 });
    expect((await screen.findByRole('link', { name: 'Open Strategic Insights' }, { timeout: 20_000 })).getAttribute('href')).toBe('/country/USA?h=80');
    expect(screen.getByRole('link', { name: 'Open Sector Insights' }).getAttribute('href')).toBe('/sector/batteries/midstream');
    // the Strategic nav carries the scenario
    await act(async () => {
      await router.navigate('/space?h=80');
    });
    expect(await screen.findByRole('heading', { level: 1, name: /opportunity.security space/i }, { timeout: 20_000 })).toBeTruthy();
    const views = screen.getByRole('navigation', { name: 'Strategic Insights views' });
    expect(within(views).getByRole('link', { name: 'Methods' }).getAttribute('href')).toBe('/methods?h=80');
    expect(within(views).getByRole('link', { name: 'Space' }).getAttribute('aria-current')).toBe('page');
    await settle(300);
  }, 60_000);

  it('Sector renders while the engine never becomes ready; Strategic waits in content under the header', async () => {
    const stuck = neverReadyClient();
    setEngineClient(stuck);
    try {
      const router = renderAt('/sector');
      await waitFor(() => expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'Sector Insights' }).getAttribute('aria-current')).toBe('true'), { timeout: 20_000 });
      await settle(200);
      expect(screen.getByRole('main')).toBeTruthy();
      expect(screen.queryByText(/Loading the model/)).toBeNull();
      expect(stuck.getSnapshot().status).toBe('starting');
      // the engine gate is in content: the header and the footer stay
      await act(async () => {
        await router.navigate('/overview?h=80');
      });
      expect(await screen.findByText(/Loading the model/, {}, { timeout: 20_000 })).toBeTruthy();
      expect(within(screen.getByRole('navigation', { name: 'Modules' })).getByRole('link', { name: 'Strategic Insights' }).getAttribute('aria-current')).toBe('true');
      expect(screen.getByRole('navigation', { name: 'Strategic Insights views' })).toBeTruthy();
      expect(screen.getByRole('contentinfo')).toBeTruthy();
    } finally {
      setEngineClient(client);
    }
  }, 60_000);
});
