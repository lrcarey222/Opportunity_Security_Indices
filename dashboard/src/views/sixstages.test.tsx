// The Strategic views at six stages (2026-10-29; design S1, S4, E3): the faceted charts draw one panel per
// supply-chain stage in chain order, all side by side on a wide screen and wrapped two a row on a phone
// (views/stageFacets.ts); the coalition heatmap keeps one row of stage groups and scrolls instead; no view
// says Upstream / Midstream / Downstream or calls the baseline the paper's numbers. The views run on the
// committed synthetic bundle (scripts/86, s1: six stages) with the engine host in process, as in
// app.test.tsx; the chart width is set through a stubbed clientWidth.

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { setEngineClient } from '../state/EngineProvider.tsx';
import { routes } from '../routes.tsx';
import { resetLastLocations } from '../shell/lastLocation.ts';
import { SUPPLY_STAGES } from '../lib/stages.ts';
import { EngineClient, type WorkerLike } from '../worker/client.ts';
import { EngineHost, type FetchBytes } from '../worker/host.ts';
import type { FromWorker, ToWorker } from '../worker/protocol.ts';

const dataBase = resolve(process.cwd(), 'src/engine/__fixtures__/synthetic/s1/data') + '/';
const fetchDisk: FetchBytes = async (url) => {
  const b = readFileSync(url);
  const out = new Uint8Array(b.length);
  out.set(b);
  return out;
};
const inflate = async (b: Uint8Array) => new Uint8Array(gunzipSync(b));

/** Typed arrays into jsdom's realm (as a browser's postMessage clone does). */
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

function inProcessWorker(): WorkerLike {
  const w: WorkerLike = {
    onmessage: null,
    postMessage(msg: ToWorker) {
      const m = structuredClone(msg);
      setTimeout(() => void host.handle(m), 0);
    },
  };
  const deliver = (m: FromWorker) => setTimeout(() => w.onmessage?.({ data: intoThisRealm(structuredClone(m)) } as MessageEvent<FromWorker>), 0);
  const host = new EngineHost(deliver, fetchDisk, inflate, { selfCheck: false });
  return w;
}

let client: EngineClient;
let width = 1200;
const restore: (() => void)[] = [];

beforeAll(async () => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => width });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: /min-width: 1024px/.test(q), media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  const NodeRequest = globalThis.Request;
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete rest.signal;
      super(input, rest);
    }
  } as typeof Request;
  restore.push(() => (globalThis.Request = NodeRequest));
  const spy = vi.spyOn(console, 'error').mockImplementation(() => undefined);
  restore.push(() => spy.mockRestore());
  client = new EngineClient(inProcessWorker(), dataBase);
  await client.whenReady();
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

/** An element's position in its svg: its own x / y plus every translate() up to the svg. */
function svgPos(el: Element): { x: number; y: number } {
  let x = Number(el.getAttribute('x') ?? 0) || 0;
  let y = Number(el.getAttribute('y') ?? 0) || 0;
  for (let n: Element | null = el; n && n.tagName.toLowerCase() !== 'svg'; n = n.parentElement) {
    const m = /translate\(\s*([-\d.e]+)[ ,]+([-\d.e]+)\s*\)/.exec(n.getAttribute('transform') ?? '');
    if (m) {
      x += Number(m[1]);
      y += Number(m[2]);
    }
  }
  return { x, y };
}

/** The stage names drawn in a figure (texts equal to `names`), with their positions, in drawing order. */
function stageTexts(fig: Element, names: readonly string[]) {
  return [...fig.querySelectorAll('text')]
    .filter((t) => names.includes(t.textContent ?? ''))
    .map((t) => ({ name: t.textContent as string, ...svgPos(t) }));
}

/** The rows of panels: stage names grouped by their y (rounded), top to bottom, each row left to right. */
function panelRows(texts: { name: string; x: number; y: number }[]): string[][] {
  const ys = [...new Set(texts.map((t) => Math.round(t.y)))].sort((a, b) => a - b);
  return ys.map((y) => texts.filter((t) => Math.round(t.y) === y).sort((a, b) => a.x - b.x).map((t) => t.name));
}

const SIX = [...SUPPLY_STAGES];
const LEGACY = /\b(Upstream|Midstream|Downstream)\b|paper['’]s (three|numbers|values|value|weights|baseline)|three-stage|against the paper|Paper baseline/i;
/** Earlier names and versions (user, 2026-10-31: readers come to the Explorer fresh). */
const EARLIER = /\bOSI\b|\bLegacy\b|NEIS (baseline|default)|kept for comparison|the original (index model|framework)|Opportunity Security Indices/i;

async function open(path: string) {
  render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [path] })} />);
}

describe('six stage panels, wide and narrow', () => {
  const cases: [string, string, RegExp][] = [
    ['Country scores: the dot plot', '/country', /^Energy Security and Economic Opportunity of /],
    ['Opportunity-security space: the economy’s own sectors', '/space', /^The selected economy's cells by stage$/],
    ['Partners: the pair, sector by sector', '/partners', /^Friendshore and Partnership Opportunity by sector$/],
  ];
  for (const [what, path, name] of cases) {
    it(`${what}: six panels in chain order, one row at 1200 px, two a row at 360 px`, async () => {
      width = 1200;
      await open(path);
      const wide = await screen.findByRole('img', { name }, { timeout: 30_000 });
      await waitFor(() => expect(stageTexts(wide, SIX)).toHaveLength(6), { timeout: 30_000 });
      expect(panelRows(stageTexts(wide, SIX))).toEqual([SIX]);
      cleanup();
      width = 360;
      await open(path);
      const narrow = await screen.findByRole('img', { name }, { timeout: 30_000 });
      await waitFor(() => expect(stageTexts(narrow, SIX)).toHaveLength(6), { timeout: 30_000 });
      expect(panelRows(stageTexts(narrow, SIX))).toEqual([
        ['Extraction', 'Refining'],
        ['Components', 'Assembly'],
        ['Deployment', 'Recovery'],
      ]);
      // the figure fits the width (no sideways scroll)
      expect(Number(narrow.getAttribute('width'))).toBeLessThanOrEqual(360);
    }, 120_000);
  }

  it('Overview: the coalition heatmap keeps one row of six stage groups in chain order and scrolls on a phone', async () => {
    width = 360;
    await open('/overview');
    const fig = await screen.findByRole('img', { name: /^Coalition table of / }, { timeout: 30_000 });
    const upper = SIX.map((s) => s.toUpperCase());
    await waitFor(() => expect(stageTexts(fig, upper)).toHaveLength(6), { timeout: 30_000 });
    expect(panelRows(stageTexts(fig, upper))).toEqual([upper]);
    // wider than the phone: its region scrolls sideways
    expect(Number(fig.getAttribute('width'))).toBeGreaterThan(360);
    expect(screen.getByRole('region', { name: /scrolls sideways on narrow screens/ })).toBeTruthy();
  }, 120_000);
});

describe('no three-stage, paper-baseline or earlier-version words in the Strategic views; each is labelled Security and opportunity scores', () => {
  for (const path of ['/overview', '/country', '/space', '/strategic', '/partners', '/overview?h=80&show=change', '/strategic?h=80', '/partners?h=80&show=change', '/data']) {
    it(path, async () => {
      width = 1200;
      await open(path);
      const main = await screen.findByRole('main', {}, { timeout: 30_000 });
      await waitFor(() => expect(main.querySelectorAll('h2, h3').length).toBeGreaterThan(0), { timeout: 30_000 });
      // let the computed charts and tables settle
      await waitFor(() => expect(main.textContent ?? '').not.toMatch(/Computing|Loading partnership data/), { timeout: 30_000 });
      // the page's own text has rendered (charts' captions, tables), not just its frame
      expect((main.textContent ?? '').length).toBeGreaterThan(1500);
      expect(main.textContent ?? '').not.toMatch(LEGACY);
      expect(main.textContent ?? '').not.toMatch(EARLIER);
      if (!path.startsWith('/data')) {
        // labelled "Security and opportunity scores" (design D10, section 5; named by what it shows, 2026-10-31): the band says it
        // is a descriptive view, not a recommendation, and links a Country profile's scores sub-section (the default profile when none
        // was visited) and the Methods chapter; the eyebrow says it too
        const band = screen.getByTestId('advanced-band');
        expect(within(band).getByText('Security and opportunity scores', { selector: '.tag' })).toBeTruthy();
        expect(band.textContent).toContain('These scores describe relative security and opportunity: a descriptive view, not a recommendation.');
        expect(within(band).getByRole('link', { name: /^These scores on a Country profile/ }).getAttribute('href')).toBe('/countries/USA#legacy-osi');
        expect(within(band).getByRole('link', { name: 'Methods: the security and opportunity scores' }).getAttribute('href')).toBe('/methods#legacy-osi');
        expect([...main.querySelectorAll('.eyebrow')].some((e) => /^Security and opportunity scores · /.test(e.textContent ?? ''))).toBe(true);
        // a one-economy view links that economy's Country Strategy Map and its security and opportunity scores
        if (/^\/(country|strategic|partners)/.test(path)) {
          const link = screen.getByTestId('country-insights-link');
          const hrefs = within(link).getAllByRole('link').map((a) => a.getAttribute('href') ?? '');
          expect(hrefs).toHaveLength(2);
          expect(hrefs[0]).toMatch(/^\/countries\/[A-Z]{3}#strategy$/);
          expect(hrefs[1]).toMatch(/^\/countries\/[A-Z]{3}#legacy-osi$/);
        }
      }
    }, 120_000);
  }

  it('the Strategic Index view names a stage in words, not as a colour series', async () => {
    width = 1200;
    await open('/strategic');
    await screen.findByRole('heading', { name: /top ten sectors|Strategic Index/i }, { timeout: 30_000 });
    // the legend has the baseline tick and no stage swatches
    await waitFor(() => expect(screen.getAllByText(/^Baseline \(tick\)$|Strategic Index/).length).toBeGreaterThan(0), { timeout: 30_000 });
    expect(screen.queryByText(/^(Extraction|Refining|Components|Assembly|Deployment|Recovery|Upstream|Midstream|Downstream) sector$/)).toBeNull();
  }, 120_000);
});
