// The recomposed Country profile (IA refactor 2026-10-16, brief s.9-12 and s.25): the framing derives
// (Make bets / dependencies / capabilities, the Allied Industrial Base place, the policy sentences) on the
// synthetic fixtures, the Partnerships tabs and the Role in the Allied Industrial Base on the AIB fixture
// (its economy AAA renamed USA), the engine staying idle on a baseline visit, and Workflow 2: the
// Perspective dials move the Strategic priority column but not the facts, and the country's own URL
// writes keep the dials.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { routes } from '../../routes.tsx';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient, type WorkerLike } from '../../worker/client.ts';
import { EngineHost, type FetchBytes } from '../../worker/host.ts';
import type { FromWorker, ToWorker } from '../../worker/protocol.ts';
import type { AibDoc } from '../../aib/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { compactTitle } from '../../aib/derive/format.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, readDoc } from '../../aib/__tests__/fixture.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { aibCountry, capabilities, compactsOfCell, dependencies, engineCellKey, makeBets, policyBehaviour, siMapOf, siOrder } from '../derive/framing.ts';
import { sectorPositionRows } from '../views/profile/SectorPositionSection.tsx';
import { fixtureSource, haveFixture, readSheet } from './fixture.ts';

/** The AIB fixture with its economy AAA renamed USA (a fact-sheet economy). */
function renamed(d: AibDoc): void {
  Object.assign(d, JSON.parse(JSON.stringify(d).replaceAll('"AAA"', '"USA"')) as AibDoc);
}

describe.skipIf(!haveFixture() || !haveAib())('Country profile framing (pure)', () => {
  const sheet = readSheet('USA');
  const doc = readDoc();
  renamed(doc);

  it('Make bets: Grow or Upgrade sectors by Strategic Index rank; a Perspective map reorders them', () => {
    const bets = makeBets(sheet, null, 99);
    expect(bets.length).toBeGreaterThan(0);
    for (const c of bets) expect(['grow', 'upgrade']).toContain(c.quadrant);
    const ranks = bets.map((c) => (c.si.in_sample ? c.si.rank.v : null)).filter((r): r is number => r !== null);
    expect([...ranks].sort((a, b) => a - b)).toEqual(ranks);
    // reverse the ranks under a "perspective": the order follows, the set does not change
    const order = siOrder(sheet, null);
    const over = new Map(order.map((c, i) => [c.key, { rank: order.length - i, delta: null, si: null, inSample: true, isNew: false }]));
    const bets2 = makeBets(sheet, over, 99);
    expect(new Set(bets2.map((c) => c.key))).toEqual(new Set(bets.map((c) => c.key)));
    if (bets.length > 1) expect(bets2.map((c) => c.key)).not.toEqual(bets.map((c) => c.key));
  });

  it('dependencies and capabilities are facts: corners and orders of the fact sheet only', () => {
    const deps = dependencies(sheet, doc, 99);
    for (const d of deps) {
      expect(['upgrade', 'secure']).toContain(d.cell.quadrant);
      expect(d.supplier).toBe(d.cell.trade?.sources?.[0] ?? null);
    }
    const es = deps.map((d) => sheet.summary.es_order.indexOf(d.cell.key));
    expect([...es].sort((a, b) => a - b)).toEqual(es);
    const caps = capabilities(sheet, null, 99);
    for (const c of caps.cells) expect(c.quadrant).toBe('grow');
  });

  it('compactsOfCell: a compact covers a cell through its nodes', () => {
    const node = doc.nodes.find((nd) => nd.compact && nd.yfCells.length)!;
    expect(compactsOfCell(doc, node.yfCells[0]).map((c) => c.id)).toContain(node.compact);
    expect(compactsOfCell(doc, 'no-such-cell')).toEqual([]);
  });

  it('compactsOfCell joins through the OSI crosswalk (yfCells), many-to-many, never through the node sector or stage', () => {
    // the AIB fixture is an electro-industrial dictionary bundle: AIB-C01 (dictionary sectors Magnets, Rare earths)
    expect(doc.meta.taxonomy).toBe('dictionary');
    // six stages (design A2): its magnets node sits in two Components cells, its rare-earth node in the Refining ones: one compact, four cells
    for (const key of ['electric-vehicles-components', 'wind-components', 'electric-vehicles-refining', 'wind-refining']) expect(compactsOfCell(doc, key).map((c) => c.id)).toContain('AIB-C01');
    // one cell, two compacts (the magnets of AIB-C01 and the wind towers of AIB-C03)
    expect(compactsOfCell(doc, 'wind-components').map((c) => c.id)).toEqual(['AIB-C01', 'AIB-C03']);
    // the wind towers reach OSI Wind Components only; a name, stage or three-stage join would find these
    expect(compactsOfCell(doc, 'wind-deployment')).toEqual([]);
    expect(compactsOfCell(doc, 'wind-midstream')).toEqual([]);
    expect(compactsOfCell(doc, 'magnets-components')).toEqual([]);
    // the dependencies list the compacts by their display title (labels with commas kept whole)
    const deps = dependencies(sheet, doc, 99);
    for (const d of deps) for (const cp of d.compacts) expect(cp.name).toBe(compactTitle(doc.compacts.find((c) => c.id === cp.id)!, doc.nodes));
    // the title is the AIB module's (imported): OSI technologies of the nodes' six-stage cells, then the node labels (design A1)
    expect(compactTitle(doc.compacts[0], doc.nodes)).toBe('Electric Vehicles / Wind: Magnets, magnet parts & assemblies; Rare-earth oxides & compounds');
  });

  it('aibCountry: roles, compacts and trade relationships of the economy', () => {
    const a = aibCountry(doc, doc.countries, 'USA');
    expect(a.roles.map((r) => r.role)).toContain('Demand anchor');
    const edges = doc.compacts.flatMap((c) => c.trade).filter((e) => e.from === 'USA' || e.to === 'USA');
    expect(a.trade.n).toBe(edges.length);
    expect(a.pairs.map((p) => p.usd)).toEqual([...a.pairs.map((p) => p.usd)].sort((x, y) => y - x));
    expect(aibCountry(doc, doc.countries, 'XYZ').roles).toEqual([]);
  });

  it('engine rows map to cell keys; sector position puts ranked sectors first', () => {
    expect(engineCellKey('Electric Grid', 'Components')).toBe('electric-grid-components');
    expect(engineCellKey('Batteries', 'Refining')).toBe('batteries-refining');
    expect(engineCellKey('Fusion', 'Extraction')).toBeNull();
    // a three-stage engine row is not a sector of the six-stage sheets
    expect(engineCellKey('Electric Grid', 'Midstream')).toBeNull();
    const m = siMapOf(
      [
        { iso3c: 'USA', tech: 'Solar', supply_chain: 'Assembly', in_sample: true, in_sample_base: true, rank_in_country: 2, rank_delta: 1, strategic_index: 0.5 },
        { iso3c: 'DEU', tech: 'Solar', supply_chain: 'Assembly', in_sample: true, rank_in_country: 1 },
        { iso3c: 'USA', tech: 'Solar', supply_chain: 'Midstream', in_sample: true, rank_in_country: 3 },
      ],
      'USA',
    );
    expect([...m.keys()]).toEqual(['solar-assembly']);
    expect(m.get('solar-assembly')).toMatchObject({ rank: 2, delta: 1, inSample: true, isNew: false });
    const rows = sectorPositionRows(sheet.cells, siOrder(sheet, null));
    expect(rows.length).toBe(sheet.cells.length);
    expect(rows[0].key).toBe(siOrder(sheet, null)[0]?.key ?? rows[0].key);
  });

  it('policy sentences name the climate rating and the agreement counts', () => {
    const lines = policyBehaviour(sheet, 'the United States');
    expect(lines[0]).toMatch(/Climate Action Tracker/);
    expect(lines.length).toBeGreaterThanOrEqual(1);
  });
});

// ---- the page --------------------------------------------------------------------------------------

const engineBase = resolve(process.cwd(), 'src/engine/__fixtures__/synthetic/s1/data') + '/';
const fetchDisk: FetchBytes = async (url) => {
  const b = readFileSync(url);
  const out = new Uint8Array(b.length);
  out.set(b);
  return out;
};
const inflate = async (b: Uint8Array) => new Uint8Array(gunzipSync(b));

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

const neverReady = () => new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/');

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
  setAibStoreForTests(new AibStore(aibSource(undefined, renamed)));
});

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
  setAibStoreForTests(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}
const h2 = (name: string | RegExp) => screen.findByRole('heading', { level: 2, name }, { timeout: 30_000 });

/** Sector -> the text of one column of the Sector position table, every page (66 sectors at six stages,
 *  12 a page: the dials reorder the rows, so the first page alone holds few of the same sectors). */
function column(header: string): Map<string, string> {
  const sec = document.getElementById('sectors')!;
  const pager = (name: string) => within(sec).queryAllByRole('button', { name }).find((b) => b.closest('#sectors table') === null && !!b.closest('#sectors')) as HTMLButtonElement | undefined;
  const out = new Map<string, string>();
  for (let prev = pager('Previous'); prev && !prev.disabled; prev = pager('Previous')) fireEvent.click(prev);
  for (let guard = 0; guard < 20; guard++) {
    const table = within(sec).getAllByRole('table')[0];
    const heads = within(table).getAllByRole('columnheader').map((h) => h.textContent ?? '');
    const j = heads.findIndex((h) => h.startsWith(header));
    for (const r of within(table).getAllByRole('row').slice(1)) {
      const cells = within(r).getAllByRole('cell');
      out.set(cells[0]?.textContent ?? '', cells[j]?.textContent ?? '');
    }
    const next = pager('Next');
    if (!next || next.disabled) break;
    fireEvent.click(next);
  }
  return out;
}

/** The same sectors carry the same values (rows can move: the table is sorted by priority). */
function sameFacts(before: Map<string, string>, after: Map<string, string>): void {
  const common = [...before.keys()].filter((k) => after.has(k));
  expect(common.length).toBeGreaterThan(3);
  for (const k of common) expect(after.get(k)).toBe(before.get(k));
}

describe.skipIf(!haveFixture() || !haveAib())('Country profile page', () => {
  // An AIB role link can name an economy the Country roster does not hold. The redirect to the landing
  // carries its notice in a stable state object (shell/Redirect.tsx): an inline one restarted the
  // navigation on every render while the landing chunk loaded, an endless loop.
  it('an economy outside the roster, reached from another page: one redirect to the landing with the notice', async () => {
    setEngineClient(neverReady());
    try {
      const r = renderAt('/countries/USA');
      await waitFor(() => expect(r.state.location.pathname).toBe('/countries/USA'), { timeout: 30_000 });
      await act(async () => {
        await r.navigate('/countries/QQQ#aib-role');
      });
      await waitFor(() => expect(r.state.location.pathname).toBe('/countries'), { timeout: 30_000 });
      expect(await screen.findByText("No country with the code 'QQQ'; showing all countries.", {}, { timeout: 30_000 })).toBeTruthy();
      expect(screen.getByRole('heading', { level: 1, name: 'Countries' })).toBeTruthy();
    } finally {
      setEngineClient(null);
    }
  }, 90_000);

  it('a baseline visit never starts the engine; Partnerships tabs and the Role in the Allied Industrial Base read the AIB results', async () => {
    const stuck = neverReady();
    setEngineClient(stuck);
    try {
      const r = renderAt('/countries/usa?scn=decoupling');
      await waitFor(() => expect(r.state.location.pathname).toBe('/countries/USA'), { timeout: 30_000 });
      await h2('Role in the Allied Industrial Base');
      const role = document.getElementById('aib-role')!;
      expect(await within(role).findByText('Demand anchor / Buyer', {}, { timeout: 20_000 })).toBeTruthy();
      expect(within(role).getByTestId('aib-relationships').textContent).toMatch(/Trade against investment/);
      expect(within(role).getAllByRole('link', { name: 'AIB-C01' })[0].getAttribute('href')).toBe('/compacts/AIB-C01?scn=decoupling');
      expect(within(role).getByRole('link', { name: /See all country roles/ }).getAttribute('href')).toBe('/aib/roles?economy=USA&scn=decoupling#role-USA');
      // Partnerships: Trade first, then Investment and Development through the URL
      const part = document.getElementById('partnerships')!;
      expect(within(part).getByTestId('ptab-trade')).toBeTruthy();
      expect(within(part).getByText(/In the compacts' coordinated trade the United States has/)).toBeTruthy();
      fireEvent.click(within(part).getByRole('radio', { name: 'Investment' }));
      await waitFor(() => expect(r.state.location.search).toBe('?ptab=investment&scn=decoupling'));
      expect(within(part).getByRole('heading', { name: 'Investment flows in the Allied Industrial Base' })).toBeTruthy();
      expect(within(part).getByTestId('aib-as-investor').textContent).toMatch(/New capacity at 2 hosts serves [$]570 m a year/);
      expect(within(part).getByTestId('aib-as-host').textContent).toMatch(/is not the best-ranked host/);
      fireEvent.click(within(part).getByRole('radio', { name: 'Development' }));
      await waitFor(() => expect(r.state.location.search).toBe('?ptab=development&scn=decoupling'));
      expect(within(part).getByRole('heading', { name: 'Capacity to build in its compacts' })).toBeTruthy();
      expect(stuck.getSnapshot().status).toBe('starting');
      // a disclosure of the model while the model loads: a loading line, the rest of the page stays
      fireEvent.click(screen.getByText(/^Explore details/));
      expect(await within(document.getElementById('fold-scores')!).findByText('Loading the model')).toBeTruthy();
      expect(screen.getByRole('heading', { level: 1, name: 'United States' })).toBeTruthy();
      expect(document.getElementById('aib-role')).toBeTruthy();
    } finally {
      setEngineClient(null);
    }
  }, 60_000);

  it('Workflow 2: the Perspective opens the four dials on the page; a dial moves the Strategic priority, not the facts; the URL keeps the dials', async () => {
    const client = new EngineClient(inProcessWorker(), engineBase);
    setEngineClient(client);
    try {
      const r = renderAt('/countries/USA');
      await h2('Sector position');
      const eo = column('Opportunity');
      const es = column('Security');
      const toggle = screen.getByRole('button', { name: /^Perspective: NEIS baseline/ });
      fireEvent.click(toggle);
      const reset = await screen.findByRole('button', { name: 'Reset to NEIS baseline' }, { timeout: 30_000 });
      expect((reset as HTMLButtonElement).disabled).toBe(true);
      for (const d of ['Hawkishness', 'Bullishness', 'Populism', 'Climate']) expect(screen.getByRole('slider', { name: new RegExp(d, 'i') })).toBeTruthy();
      await act(async () => {
        fireEvent.change(screen.getByRole('slider', { name: /hawkishness/i }), { target: { value: '90' } });
      });
      await waitFor(() => expect(r.state.location.search).toMatch(/h=90/));
      await waitFor(() => expect(screen.getAllByRole('button', { name: /^Perspective: custom/ }).length).toBe(1), { timeout: 30_000 });
      expect(screen.getByRole('heading', { name: 'Top strategic priorities under this perspective' })).toBeTruthy();
      await waitFor(() => expect(screen.getAllByText(/your perspective/).length).toBeGreaterThan(0), { timeout: 30_000 });
      // facts stay as they are
      sameFacts(eo, column('Opportunity'));
      sameFacts(es, column('Security'));
      // the country's own writes keep the dials (a sector chip of the four corners)
      const corners = screen.getByRole('region', { name: 'The four corners' });
      fireEvent.click(within(corners).getAllByRole('button', { pressed: false })[0]);
      await waitFor(() => expect(r.state.location.search).toMatch(/^\?cell=[a-z-]+&.*h=90/));
      // reset: back to the NEIS baseline
      fireEvent.click(screen.getAllByRole('button', { name: 'Reset to NEIS baseline' })[0]);
      await waitFor(() => expect(r.state.location.search).not.toMatch(/h=/));
      expect(screen.getByRole('button', { name: /^Perspective: NEIS baseline/ })).toBeTruthy();
    } finally {
      setEngineClient(null);
    }
  }, 90_000);
});

describe.skipIf(!haveFixture() || !haveAib())('Country profile: monthly trade by sub-component (2026-10-19)', () => {
  it('a core member with a monthly file: the card follows the selected sector and switches flow', async () => {
    setEngineClient(neverReady());
    try {
      renderAt('/countries/USA?cell=solar-assembly#sectors');
      const card = await screen.findByTestId('trade-monthly', {}, { timeout: 30_000 });
      expect(within(card).getByRole('heading', { level: 3, name: /Solar — Assembly: monthly imports by sub-component/ })).toBeTruthy();
      fireEvent.click(within(card).getByRole('radio', { name: 'Exports' }));
      expect(await within(card).findByRole('heading', { level: 3, name: /monthly exports by sub-component/ })).toBeTruthy();
    } finally {
      setEngineClient(null);
    }
  }, 90_000);

  it('a sector without a traded-goods basket falls back to the one with the most monthly trade and says why', async () => {
    setEngineClient(neverReady());
    try {
      renderAt('/countries/USA?cell=oil-extraction#sectors');
      const card = await screen.findByTestId('trade-monthly', {}, { timeout: 30_000 });
      expect(within(card).getByRole('heading', { level: 3, name: /Batteries — Assembly/ })).toBeTruthy();
      expect(within(card).getByText(/has no traded-goods basket \(HS6 codes\); showing Batteries — Assembly/)).toBeTruthy();
    } finally {
      setEngineClient(null);
    }
  }, 90_000);

  it('a core member without monthly data gets a notice; the card stays off non-core pages', async () => {
    setEngineClient(neverReady());
    try {
      renderAt('/countries/FRA#sectors');
      expect(await screen.findByTestId('trade-monthly-none', {}, { timeout: 30_000 })).toBeTruthy();
      cleanup();
      renderAt('/countries/BRA#sectors');
      await h2('Sector position');
      expect(screen.queryByTestId('trade-monthly')).toBeNull();
      expect(screen.queryByTestId('trade-monthly-none')).toBeNull();
    } finally {
      setEngineClient(null);
    }
  }, 90_000);
});
