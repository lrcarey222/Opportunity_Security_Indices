// The Partners view's sector choice and the bilateral-investment cards (jsdom). The view runs on the
// committed synthetic bundle (scripts/86, s1) with the engine host in process, as in app.test.tsx;
// the investment cards (the IMF positions, then the cleantech factory projects with their technology
// choice, or a page's own sector's projects only) on a small synthetic ties chunk (tiesFixture.ts).

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
    // without a sector the factory-projects card shows all four technologies together
    const card = screen.getByRole('region', { name: 'Cleantech factory projects between the United States and its partners' });
    expect((within(card).getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement).value).toBe('');
    expect(within(card).getByRole('list', { name: 'the United States: factory projects in solar, wind, batteries and electric vehicles by partner' })).toBeTruthy();
    expect(screen.getByText(/Friendshore's investment tie ranks/)).toBeTruthy();
  });

  const projectsCard = () => screen.getByRole('region', { name: 'Cleantech factory projects between the United States and its partners' });
  const techSelect = () => within(projectsCard()).getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement;
  const projectItems = (words: string) => within(within(projectsCard()).getByRole('list', { name: `the United States: factory projects in ${words} by partner` })).getAllByRole('listitem');
  const regionNames = () => screen.getAllByRole('region').map((r) => document.getElementById(r.getAttribute('aria-labelledby') ?? '')?.textContent);

  it("adds the cleantech factory projects, both ways, on the sector's technology at any stage, with the CC BY attribution", () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage: 'Components' }} nameOf={nameOf} />);
    // the IMF positions first, then the factory projects
    expect(regionNames()).toEqual(['Direct investment between the United States and its partners', 'Cleantech factory projects between the United States and its partners']);
    expect(techSelect().value).toBe('Solar');
    expect([...techSelect().options].map((o) => o.textContent)).toEqual(['All four technologies', 'Solar', 'Wind', 'Batteries', 'Electric Vehicles']);
    const items = projectItems('solar');
    expect(items[0].textContent).toMatch(/^Japan/);
    expect(items[0].textContent).toMatch(/into the United States: 3 projects, \$500 million of disclosed capital/);
    expect(items[0].textContent).toMatch(/abroad: 1 project, capital not disclosed/);
    expect(screen.getByTestId('cmfdi-attribution').textContent).toMatch(/CC BY 4\.0/);
    // said once: the two CM-FDI stages carry the same projects
    expect(screen.getAllByTestId('cmfdi-same-projects')).toHaveLength(1);
    expect(screen.getByTestId('cmfdi-same-projects').textContent).toMatch(/^Components and Assembly carry the same factory projects: .* counted once, not once per stage\./);
    // the positions card names the CM-FDI stages from the data and points to the projects card
    expect(screen.getByText(/at\s+the Components and Assembly stages the tie is the factory projects instead \(see the factory projects card below\)/)).toBeTruthy();
    expect(within(projectsCard()).getByText(/The data cover four technologies only: solar, wind, batteries and electric vehicles\./)).toBeTruthy();
    cleanup();
    // Assembly carries the same projects (the same counts, not doubled), and so does every other stage of Solar
    for (const stage of ['Assembly', 'Extraction', 'Refining', 'Deployment', 'Recovery']) {
      render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage }} nameOf={nameOf} />);
      expect(techSelect().value).toBe('Solar');
      expect(projectItems('solar')[0].textContent).toMatch(/into the United States: 3 projects, \$500 million of disclosed capital/);
      expect(screen.getByTestId('cmfdi-attribution')).toBeTruthy();
      cleanup();
    }
  });

  it('the technology choice: all four together outside the four, one at a time, a plain line when there are none, following the page until the reader picks', () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Nuclear', stage: 'Components' }} nameOf={nameOf} />);
    expect(techSelect().value).toBe('');
    expect(techSelect().selectedOptions[0].textContent).toBe('All four technologies');
    // Japan: Solar and Wind together (3 + 5 projects into the United States); Germany: Solar
    const all = projectItems('solar, wind, batteries and electric vehicles');
    expect(all).toHaveLength(2);
    expect(all[0].textContent).toMatch(/^Japan.*into the United States: 8 projects, \$1\.4 billion of disclosed capital/);
    expect(all[1].textContent).toMatch(/^Germany.*abroad: 2 projects, \$80 million of disclosed capital/);
    // one technology at a time
    fireEvent.change(techSelect(), { target: { value: 'Wind' } });
    expect(projectItems('wind')[0].textContent).toMatch(/^Japan.*into the United States: 5 projects, \$900 million of disclosed capital/);
    // none: a plain line, not an empty chart
    fireEvent.change(techSelect(), { target: { value: 'Batteries' } });
    expect(within(projectsCard()).queryByRole('list', { name: /factory projects in batteries by partner/ })).toBeNull();
    expect(within(projectsCard()).getByText('No cross-border factory projects in batteries involve the United States, in either direction.')).toBeTruthy();
    cleanup();
    // the card follows the page's sector until the reader picks a technology here, then keeps the reader's choice
    const page = render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={null} nameOf={nameOf} />);
    expect(techSelect().value).toBe('');
    page.rerender(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Wind', stage: 'Assembly' }} nameOf={nameOf} />);
    expect(techSelect().value).toBe('Wind');
    page.rerender(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Gas', stage: 'Extraction' }} nameOf={nameOf} />);
    expect(techSelect().value).toBe('');
    fireEvent.change(techSelect(), { target: { value: 'Solar' } });
    page.rerender(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Electric Vehicles', stage: 'Components' }} nameOf={nameOf} />);
    expect(techSelect().value).toBe('Solar');
    cleanup();
    // an economy with no projects at all: the plain line for the four technologies
    render(<InvestmentCards iso="IND" name="India" ties={ties} sector={null} nameOf={nameOf} />);
    expect(screen.getByText('No cross-border factory projects in solar, wind, batteries or electric vehicles involve India, in either direction.')).toBeTruthy();
  });

  it("a page can ask for its own sector's projects only (one of the four technologies at a CM-FDI stage)", () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage: 'Components' }} nameOf={nameOf} projects="sector" />);
    expect(screen.getByRole('heading', { name: /^Factory projects in Solar . Components/ })).toBeTruthy();
    expect(screen.queryByRole('region', { name: /^Cleantech factory projects/ })).toBeNull();
    const list = screen.getByRole('list', { name: /factory projects in Solar . Components by partner/ });
    const items = within(list).getAllByRole('listitem');
    expect(items[0].textContent).toMatch(/^Japan/);
    expect(items[0].textContent).toMatch(/into the United States: 3 projects, \$500 million of disclosed capital/);
    expect(items[0].textContent).toMatch(/abroad: 1 project, capital not disclosed/);
    expect(screen.getByTestId('cmfdi-attribution').textContent).toMatch(/CC BY 4\.0/);
    expect(screen.getAllByTestId('cmfdi-same-projects')).toHaveLength(1);
    expect(screen.getByText(/at\s+the Components and Assembly stages the tie is the factory projects instead \(choose one of those sectors\)/)).toBeTruthy();
    cleanup();
    // Assembly carries the same projects (the same counts, not doubled); the page words its own hint
    render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={{ tech: 'Solar', stage: 'Assembly' }} nameOf={nameOf} projects="sector" projectsHint="shown below for those sectors" />);
    const asm = within(screen.getByRole('list', { name: /factory projects in Solar . Assembly by partner/ })).getAllByRole('listitem');
    expect(asm[0].textContent).toMatch(/into the United States: 3 projects, \$500 million of disclosed capital/);
    expect(screen.getByText(/the tie is the factory projects instead \(shown below for those sectors\)/)).toBeTruthy();
    cleanup();
    for (const sector of [null, ...['Extraction', 'Refining', 'Deployment', 'Recovery', 'Midstream'].map((stage) => ({ tech: 'Solar', stage })), { tech: 'Nuclear', stage: 'Components' }]) {
      render(<InvestmentCards iso="USA" name="the United States" ties={ties} sector={sector} nameOf={nameOf} projects="sector" />);
      expect(screen.queryByTestId('cmfdi-attribution')).toBeNull();
      cleanup();
    }
  });

  it('says so when the data are missing', () => {
    render(<InvestmentCards iso="USA" name="the United States" ties={null} error="This data build has no direct-investment data." sector={null} nameOf={nameOf} />);
    expect(screen.getByText('This data build has no direct-investment data.')).toBeTruthy();
    // said once: the factory projects come from the same data, so their card stays out, and no note points to it
    expect(screen.queryByRole('region', { name: /^Cleantech factory projects/ })).toBeNull();
    expect(screen.getByText(/the tie is the factory projects instead\. Positions are/)).toBeTruthy();
    expect(screen.queryByText(/see the factory projects card below/)).toBeNull();
  });
});
