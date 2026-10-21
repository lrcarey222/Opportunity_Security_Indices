// The scenario toggle in the app (2026-10-19; Managed Interdependence the default since 2026-10-20): the
// shared header of every AIB page (a radio group "Managed Interdependence | Decoupling", the chosen
// scenario's words, the framing line, the About link), the URL key scn
// (written on a choice, carried by the nav across the two layouts, a stray value dropped, an older bundle
// without the toggle), the lazy scenario files (never fetched before a hover or a choice; the hook idle for
// Decoupling and "missing" in an older bundle), About's "How the scenarios work", the Partnerships page's
// host key, and the shared comparison charts.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { splitSearch } from '../../state/url.ts';
import { AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { useAibScenario, useAibScenarioDoc } from '../data/hooks.ts';
import { nodeScenarios, scenarioTotalsAll, STRATEGIC_FRAMING } from '../derive/scenarios.ts';
import { fmtUsd } from '../derive/format.ts';
import { ScenarioBars, ScenarioClassBars, ScenarioCoverageCompare, scenarioValues } from '../charts/ScenarioCompare.tsx';
import { resetLastCompact } from '../nav.ts';
import { PARTNERSHIP_KEYS } from '../views/AibPartnershipsView.tsx';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, readManifest, stripScenarios, type FixtureSourceOpts } from './fixture.ts';
import type { AibDoc, AibScenarioId } from '../contract.ts';

const restore: (() => void)[] = [];
let source: AibSource & { calls: string[] };

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

function useSource(edit?: (d: AibDoc) => void, opts?: FixtureSourceOpts): void {
  source = fixtureSource(FIXTURE_DIR, edit, opts);
  setAibStoreForTests(new AibStore(source));
}

beforeEach(() => useSource());

afterEach(() => {
  cleanup();
  setAibStoreForTests(null);
  resetLastLocations();
  resetLastCompact();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const toggle = () => screen.findByTestId('scenario-toggle', {}, { timeout: 30_000 });
const views = () => screen.getByRole('navigation', { name: 'Allied Industrial Base views' });
const radio = (name: string) => screen.getByRole('radio', { name }) as HTMLInputElement;

describe.skipIf(!haveFixture())('the scenario toggle', () => {
  const manifest = readManifest();
  const files = manifest.files.scenarios ?? {};
  const scenarioFetched = () => source.calls.filter((f) => f.startsWith('scenario-'));

  it('every AIB page has it: "Managed Interdependence | Decoupling", Managed Interdependence the default (no scn), its words, the framing, the About link', async () => {
    if (!files.managed) return;
    const router = renderAt('/aib');
    const t = await toggle();
    const radios = within(t).getAllByRole('radio') as HTMLInputElement[];
    expect(radios.map((r) => r.closest('label')!.textContent)).toEqual(['Managed Interdependence', 'Decoupling']);
    expect(radio('Managed Interdependence').checked).toBe(true);
    expect(router.state.location.search).toBe('');
    expect(within(t).getByRole('radiogroup', { name: 'Scenario' })).toBeTruthy();
    expect(t.textContent).toMatch(/Managed Interdependence: No supplier is removed/);
    expect(t.textContent).toMatch(/not forecasts/);
    // the strategic compact is the persistent architecture, the scenario the stress applied to it
    const framing = readDoc().strategic?.framing ?? STRATEGIC_FRAMING;
    expect(within(t).getByTestId('scenario-framing').textContent).toBe(`${framing} Managed Interdependence is the default institutional lens; Decoupling is the stress test.`);
    expect(within(t).getByRole('link', { name: /How the scenarios work/ }).getAttribute('href')).toBe('/compacts/about#scenarios');
    expect(bareAcronyms(t)).toEqual([]);
    // the choice: scn=decoupling in the URL, the words, the nav and the About link carry it
    fireEvent.click(radio('Decoupling'));
    await waitFor(() => expect(router.state.location.search).toBe('?scn=decoupling'));
    expect(radio('Decoupling').checked).toBe(true);
    expect(t.textContent).toMatch(/Decoupling: .*stops supplying the allied market \(it keeps buying\)/);
    await waitFor(() => expect(within(views()).getAllByRole('link').every((a) => a.getAttribute('href')!.endsWith('?scn=decoupling'))).toBe(true));
    expect(within(t).getByRole('link', { name: /How the scenarios work/ }).getAttribute('href')).toBe('/compacts/about?scn=decoupling#scenarios');
    // and back: the default is never written
    fireEvent.click(radio('Managed Interdependence'));
    await waitFor(() => expect(router.state.location.search).toBe(''));
  });

  it('under Decoupling nothing is fetched before a hover or a choice; a hover prefetches the Managed Interdependence file only', async () => {
    if (!files.managed) return;
    renderAt('/aib?scn=decoupling');
    const t = await toggle();
    expect(radio('Decoupling').checked).toBe(true);
    expect(scenarioFetched()).toEqual([]);
    fireEvent.pointerEnter(t);
    await waitFor(() => expect(scenarioFetched()).toEqual([files.managed]));
  });

  it('the choice survives the move between /aib and /compacts (two layouts): the nav carries it, the toggle reads it', async () => {
    if (!files.managed) return;
    const router = renderAt('/aib?scn=decoupling');
    await toggle();
    expect(radio('Decoupling').checked).toBe(true);
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('href')).toBe('/compacts?scn=decoupling'));
    fireEvent.click(within(views()).getByRole('link', { name: 'Sectoral Compacts' }));
    await waitFor(() => expect(router.state.location.pathname).toBe('/compacts'));
    expect(router.state.location.search).toBe('?scn=decoupling');
    await waitFor(() => expect(radio('Decoupling').checked).toBe(true));
    // a workstream's page, the Workstreams index and About have it too
    await router.navigate(`/compacts/${readDoc().compacts[0].id}?scn=decoupling`);
    await waitFor(() => expect(radio('Decoupling').checked).toBe(true));
    await router.navigate('/compacts/workstreams?scn=decoupling');
    await waitFor(() => expect(radio('Decoupling').checked).toBe(true));
  });

  it("a stray scn is dropped (other keys and the hash kept), an older link's scn=managed too; an older bundle has no toggle and drops any scn", async () => {
    const router = renderAt('/compacts/workstreams?tech=x&scn=baseline#nodes');
    await toggle();
    await waitFor(() => expect(router.state.location.search).toBe('?tech=x'));
    expect(router.state.location.hash).toBe('#nodes');
    expect(radio('Managed Interdependence').checked).toBe(true);
    cleanup();
    const r1 = renderAt('/aib?scn=managed&x=1');
    await toggle();
    await waitFor(() => expect(r1.state.location.search).toBe('?x=1'));
    expect(radio('Managed Interdependence').checked).toBe(true);
    cleanup();
    useSource((d) => stripScenarios(d), { noScenarioFiles: true });
    const r2 = renderAt('/aib?scn=decoupling');
    await screen.findByRole('heading', { level: 1 }, { timeout: 30_000 });
    await waitFor(() => expect(r2.state.location.search).toBe(''));
    expect(screen.queryByTestId('scenario-toggle')).toBeNull();
    // its links write no scn (its pages show Decoupling, the main document)
    await waitFor(() => expect(within(views()).getAllByRole('link').every((a) => !a.getAttribute('href')!.includes('scn='))).toBe(true));
  });

  it("About: How the scenarios work, the three scenarios in order with this export's counts", async () => {
    renderAt('/compacts/about');
    expect(await screen.findByRole('heading', { level: 2, name: 'How the scenarios work' }, { timeout: 30_000 })).toBeTruthy();
    const s = screen.getByTestId('about-scenarios');
    const text = s.textContent ?? '';
    expect(within(s).getAllByRole('term').map((t) => t.textContent)).toEqual(['Baseline', 'Managed Interdependence', 'Decoupling']);
    expect(text).toContain('You are reading Managed Interdependence.');
    expect(text).toMatch(/In this export it runs on 3 nodes of 7 and touches 3 compacts of 3\./);
    expect(text).toMatch(/counterfactual market structures, not forecasts/);
    expect(within(s).getByRole('link', { name: /How Managed Interdependence sets its caps/ }).getAttribute('href')).toBe('/aib/diversification');
    expect(bareAcronyms(s)).toEqual([]);
  });

  it('About follows its #anchor (the toggle links #scenarios); the footer link to About carries the scenario', async () => {
    const seen: string[] = [];
    const prev = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function (this: Element) {
      seen.push(this.id);
    } as typeof Element.prototype.scrollIntoView;
    try {
      renderAt('/compacts/about?scn=decoupling#scenarios');
      expect(await screen.findByRole('heading', { level: 2, name: 'How the scenarios work' }, { timeout: 30_000 })).toBeTruthy();
      await waitFor(() => expect(seen).toContain('scenarios'));
      const footer = document.querySelector('footer');
      expect(footer).not.toBeNull();
      expect(within(footer as HTMLElement).getByRole('link', { name: 'How compacts are derived' }).getAttribute('href')).toBe('/compacts/about?scn=decoupling');
    } finally {
      Element.prototype.scrollIntoView = prev;
    }
  });

  it('the Partnerships page keeps scn as a host key, so an embedded Strategic view never strips it', () => {
    expect(PARTNERSHIP_KEYS).toContain('scn');
    expect(splitSearch('?mode=development&scn=decoupling&h=60', PARTNERSHIP_KEYS)).toEqual({ own: '?h=60', host: [['mode', 'development'], ['scn', 'decoupling']] });
  });
});

function Probe({ id }: { id: AibScenarioId }) {
  const s = useAibScenarioDoc(id);
  const t = useAibScenario();
  return (
    <div data-testid="probe">
      {JSON.stringify({ scenario: s.doc?.scenario ?? null, loading: s.loading, missing: s.missing, error: s.error?.code ?? null, toggle: t.scenario, href: t.href('/compacts/AIB-C01') })}
    </div>
  );
}

function renderProbe(id: AibScenarioId, path = '/x') {
  const router = createMemoryRouter([{ path: '/x', element: <Probe id={id} /> }], { initialEntries: [path] });
  render(<RouterProvider router={router} />);
}

const probe = () => JSON.parse(screen.getByTestId('probe').textContent ?? '{}') as Record<string, unknown>;

describe.skipIf(!haveFixture())('useAibScenarioDoc and useAibScenario (anywhere under the router)', () => {
  it('loads a scenario file lazily; Decoupling is idle; an older manifest says missing', async () => {
    if (!readManifest().files.scenarios?.baseline) return;
    renderProbe('baseline', '/x?scn=decoupling');
    await waitFor(() => expect(probe()).toMatchObject({ scenario: 'baseline', loading: false, missing: false, error: null, toggle: 'decoupling', href: '/compacts/AIB-C01?scn=decoupling' }));
    cleanup();
    // no scn: Managed Interdependence, the default (never written)
    renderProbe('decoupling');
    await waitFor(() => expect(probe()).toMatchObject({ scenario: null, loading: false, missing: false, toggle: 'managed', href: '/compacts/AIB-C01' }));
    expect(source.calls.filter((f) => f.startsWith('scenario-'))).toEqual([readManifest().files.scenarios!.baseline]);
    cleanup();
    useSource(undefined, { noScenarioFiles: true });
    renderProbe('managed');
    await waitFor(() => expect(probe()).toMatchObject({ scenario: null, loading: false, missing: true }));
  });

  it('a broken file is an error, not a crash', async () => {
    if (!readManifest().files.scenarios?.managed) return;
    useSource(undefined, { editScenario: (d) => void ((d as unknown as Record<string, unknown>).schema = 'x') });
    renderProbe('managed');
    await waitFor(() => expect(probe()).toMatchObject({ scenario: null, loading: false, error: 'CONTRACT' }));
  });
});

describe.skipIf(!haveFixture())('the shared comparison charts', () => {
  const doc = readDoc();
  const mag = doc.nodes.find((n) => n.id === 'ev-wind-mid-magnets') ?? doc.nodes[0];

  it('coverage: three rows in order, the toggled one current, Baseline a full kept bar, values in text', () => {
    render(<ScenarioCoverageCompare rows={[...nodeScenarios(mag)].reverse()} highlight="managed" label="Coverage of the magnets demand" meta={doc.meta} />);
    const fig = screen.getByRole('figure', { name: 'Coverage of the magnets demand' });
    const rows = within(fig).getAllByRole('listitem').filter((li) => li.hasAttribute('data-scenario'));
    expect(rows.map((r) => r.getAttribute('data-scenario'))).toEqual(['baseline', 'managed', 'decoupling']);
    expect(rows.map((r) => r.getAttribute('aria-current'))).toEqual([null, 'true', null]);
    expect(within(rows[1]).getByText('shown')).toBeTruthy();
    const kept = rows[0].querySelectorAll('[data-part]');
    expect([...kept].map((x) => x.getAttribute('data-part'))).toEqual(['kept']);
    expect((kept[0] as HTMLElement).style.width).toBe('100%');
    expect(rows[0].textContent).toContain('kept 100% · restored by trade 0% · new capacity 0%');
    expect(rows[0].textContent).toContain('trade as it is today, before any shock');
    expect(within(rows[1]).getByRole('img').getAttribute('aria-label')).toMatch(/^Managed Interdependence: kept 66% · restored by trade 12% · new capacity 22%/);
    expect(within(fig).getByRole('list', { name: 'Coverage parts' }).textContent).toBe('KeptRestored by tradeNew capacity');
    expect(bareAcronyms(fig)).toEqual([]);
    cleanup();
    // a node the scenario does not run on: unchanged, said so
    const ree = doc.nodes.find((n) => n.id === 'ev-wind-mid-ree-compounds') ?? doc.nodes[0];
    render(<ScenarioCoverageCompare rows={nodeScenarios(ree)} highlight="decoupling" label="Rare-earth compounds" meta={doc.meta} legend={false} />);
    const managedRow = document.querySelector('[data-scenario="managed"]') as HTMLElement;
    if (ree.diversify?.status !== 'diversified to the benchmark') expect(managedRow.textContent).toContain('unchanged: nothing is above its benchmark here');
    expect(screen.queryByRole('list', { name: 'Coverage parts' })).toBeNull();
  });

  it('a scalar on a shared scale and the lost supply by class, the toggled scenario current', () => {
    const totals = scenarioTotalsAll(doc.nodes);
    render(<ScenarioBars values={scenarioValues(totals, (t) => t.lostUsd)} highlight="decoupling" label="Supply lost or shifted" format={fmtUsd} meta={doc.meta} />);
    const bars = screen.getByRole('figure', { name: 'Supply lost or shifted' });
    const items = within(bars).getAllByRole('listitem');
    expect(items.map((r) => r.getAttribute('aria-current'))).toEqual([null, null, 'true']);
    expect(items[2].textContent).toContain(fmtUsd(totals[2].lostUsd));
    expect(items[0].querySelector('[role="img"] span')).toBeNull();
    cleanup();
    render(
      <ScenarioClassBars
        rows={totals.map((t) => ({ id: t.id, groups: t.byClass.map((g) => ({ cls: g.cls, usd: g.lostUsd })) }))}
        highlight="managed"
        label="Where the lost supply sits"
        format={fmtUsd}
        meta={doc.meta}
      />,
    );
    const cls = screen.getByRole('figure', { name: 'Where the lost supply sits' });
    const rows = within(cls).getAllByRole('listitem').filter((li) => li.hasAttribute('data-scenario'));
    expect(rows[0].textContent).toContain('nothing is lost before any shock');
    expect(rows[1].getAttribute('aria-current')).toBe('true');
    expect(rows[1].querySelectorAll('[data-cls]').length).toBe(totals[1].byClass.length);
    expect(within(cls).getByRole('list', { name: 'Classes' })).toBeTruthy();
  });
});
