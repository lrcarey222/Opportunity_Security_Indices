// The Partners view's sector choice and the bilateral-investment cards (jsdom). The view runs on the
// committed synthetic bundle (scripts/86, s1) with the engine host in process, as in app.test.tsx;
// the investment cards on a small synthetic ties chunk (tiesFixture.ts).

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { routes } from '../../routes.tsx';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { EngineClient, type WorkerLike } from '../../worker/client.ts';
import { EngineHost, type FetchBytes } from '../../worker/host.ts';
import type { FromWorker, ToWorker } from '../../worker/protocol.ts';
import { InvestmentCards } from './InvestmentCard.tsx';
import { syntheticTies } from './tiesFixture.ts';

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
const restore: (() => void)[] = [];

beforeAll(async () => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
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

describe('Partners view: the sector choice', () => {
  it('starts on all sectors; choosing a sector writes the URL and ranks friendshoring opportunities and export markets in that sector', async () => {
    const router = createMemoryRouter(routes, { initialEntries: ['/partners'] });
    render(<RouterProvider router={router} />);
    const select = (await screen.findByLabelText('Sector', {}, { timeout: 20_000 })) as HTMLSelectElement;
    expect(select.value).toBe('');
    expect(await screen.findByRole('heading', { name: /its 20 strongest partners$/ }, { timeout: 30_000 })).toBeTruthy();
    const opt = [...select.options].find((o) => o.value !== '') as HTMLOptionElement;
    await act(async () => {
      fireEvent.change(select, { target: { value: opt.value } });
    });
    await waitFor(() => expect(router.state.location.search).toMatch(/tech=.+&stage=/));
    const label = opt.textContent as string;
    expect(screen.getByTestId('sector-note').textContent).toContain(label);
    expect(await screen.findByRole('heading', { name: `Friendshoring opportunities in ${label}` }, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByRole('heading', { name: `Best export markets in ${label}` })).toBeTruthy();
    expect(screen.queryByRole('heading', { name: /its 20 strongest partners$/ })).toBeNull();
    // the sector's own scores reach the charts (engine.cellPartners through the worker)
    await waitFor(() => expect(document.querySelectorAll('figure svg').length).toBeGreaterThan(1), { timeout: 30_000 });
    expect(screen.getByRole('heading', { name: new RegExp(`every partner in ${label.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`) })).toBeTruthy();
    const search = router.state.location.search;
    // back to all sectors
    await act(async () => {
      fireEvent.change(select, { target: { value: '' } });
    });
    await waitFor(() => expect(router.state.location.search).not.toMatch(/tech=/));
    expect(screen.queryByTestId('sector-note')).toBeNull();
    // a link with the sector opens on it (the country page's links)
    cleanup();
    render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [`/partners${search}`] })} />);
    const s2 = (await screen.findByLabelText('Sector', {}, { timeout: 20_000 })) as HTMLSelectElement;
    expect(s2.value).toBe(opt.value);
    expect(screen.getByTestId('sector-note')).toBeTruthy();
  }, 120_000);
});

describe('bilateral investment cards', () => {
  const ties = syntheticTies();
  const nameOf = (iso: string) => ({ USA: 'United States', JPN: 'Japan', DEU: 'Germany', NLD: 'Netherlands', IND: 'India' })[iso] ?? iso;

  it('draws inward and outward positions, lists conduit centres apart, and switches instruments', async () => {
    const onSelect = vi.fn();
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={null} nameOf={nameOf} onSelect={onSelect} />);
    const list = screen.getByRole('list', { name: 'the United States: direct investment by partner' });
    const items = within(list).getAllByRole('listitem').map((li) => li.textContent ?? '');
    expect(items[0]).toMatch(/^Japan/);
    // the conduit centre comes after every ranked partner, marked
    expect(items[items.length - 1]).toMatch(/^Netherlands · conduit centre/);
    expect(items.find((t) => t.startsWith('Germany'))).toMatch(/partly from the partner/);
    expect(within(list).getAllByText(/inward: \$650 billion \(2023, as reported by the partner\)/).length).toBe(1);
    fireEvent.click(within(list).getByRole('button', { name: /^Japan/ }));
    expect(onSelect).toHaveBeenCalledWith('JPN');
    fireEvent.click(screen.getByRole('radio', { name: 'Debt instruments' }));
    const debt = within(screen.getByRole('list', { name: 'the United States: direct investment by partner' })).getAllByRole('listitem');
    expect(debt).toHaveLength(1);
    // no factory-project card without a CM-FDI sector
    expect(screen.queryByTestId('cmfdi-attribution')).toBeNull();
    expect(screen.getByText(/Friendshore's investment tie ranks/)).toBeTruthy();
  });

  it('adds the factory projects of a CM-FDI sector, both ways, with the CC BY attribution', () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage: 'Midstream' }} nameOf={nameOf} />);
    expect(screen.getByRole('heading', { name: /^Factory projects in Solar . Midstream/ })).toBeTruthy();
    const list = screen.getByRole('list', { name: /factory projects in Solar . Midstream by partner/ });
    const items = within(list).getAllByRole('listitem');
    expect(items[0].textContent).toMatch(/^Japan/);
    expect(items[0].textContent).toMatch(/into the United States: 3 projects, \$500 million of disclosed capital/);
    expect(items[0].textContent).toMatch(/abroad: 1 project, capital not disclosed/);
    expect(screen.getByTestId('cmfdi-attribution').textContent).toMatch(/CC BY 4\.0/);
    cleanup();
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage: 'Upstream' }} nameOf={nameOf} />);
    expect(screen.queryByTestId('cmfdi-attribution')).toBeNull();
  });

  it('says so when the data are missing', () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={null} error="This data build has no direct-investment data." sector={null} nameOf={nameOf} />);
    expect(screen.getByText('This data build has no direct-investment data.')).toBeTruthy();
  });
});
