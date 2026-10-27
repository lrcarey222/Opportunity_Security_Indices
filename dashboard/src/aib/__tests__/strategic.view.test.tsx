// The strategic layer in the app (2026-10-20): the two shared components (the supply-chain spine: steps in
// value-chain order, each chip coloured by its status under the scenario and printing it, keyboard-
// reachable, Enter or a click selects a node, the arrow keys move along the chain; the country x node
// division of labour: a table of dots that speaks every cell, the legend of what the dots mean, the measure
// choice, the table twin and a CSV), the strategic routes (/compacts lands on the Strategic Sectoral
// Compacts, whose cards link to the compact and to its workstreams; /compacts/s/:sid; /compacts/SC-... and
// the former landing's links redirected; the workstreams keep /compacts/:id; a bundle without the layer
// lands on the Workstreams index), the toggle's scenario on a strategic page (the same spine, other
// statuses) and useAibStrategic() outside the AIB layout.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { useState } from 'react';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { useAibStrategic } from '../data/hooks.ts';
import { DivisionOfLabour, divisionCsvRows } from '../charts/DivisionOfLabour.tsx';
import { ValueChainSpine } from '../charts/ValueChainSpine.tsx';
import { divisionMatrix, spineOf, strategicCompactOf, type DivisionMeasure } from '../derive/strategic.ts';
import { resetLastCompact } from '../nav.ts';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, stripStrategic, SYNTHETIC_DIR, type FixtureSourceOpts } from './fixture.ts';
import type { AibDoc } from '../contract.ts';

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

// the synthetic fixture always (its strategic section is the one these tests know)
function useSource(edit?: (d: AibDoc) => void, opts?: FixtureSourceOpts): void {
  source = fixtureSource(SYNTHETIC_DIR, edit, opts);
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

const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const doc = readDoc(SYNTHETIC_DIR);
const s = doc.strategic!;
const nodeOf = (id: string) => doc.nodes.find((n) => n.id === id) ?? null;
const name = (iso: string) => (doc.countries as Record<string, string>)[iso] ?? iso;
const mags = strategicCompactOf(s, 'SC-MAGNETS')!;
const MAG = 'ev-wind-mid-magnets';
const REE = 'ev-wind-mid-ree-compounds';

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('ValueChainSpine', () => {
  it('the steps in order, each chip printing its status; a click or Enter selects; the arrow keys move along the chain; the legend lists the statuses shown', () => {
    const onSelect = vi.fn();
    function Spine() {
      const [sel, setSel] = useState<string | null>(null);
      return (
        <ValueChainSpine
          steps={spineOf(mags, 'managed', nodeOf)}
          label="Permanent Magnets under Managed Interdependence"
          selected={sel}
          onSelect={(id) => {
            onSelect(id);
            setSel(id);
          }}
          name={name}
        />
      );
    }
    render(<Spine />);
    const group = screen.getByRole('group', { name: 'Permanent Magnets under Managed Interdependence' });
    const steps = within(group).getAllByTestId('spine-step');
    expect(steps.map((x) => x.querySelector('p')!.textContent)).toEqual(['Step 1: Separated oxides', 'Step 2: Magnets']);
    const chips = within(group).getAllByRole('button');
    expect(chips.map((b) => b.getAttribute('data-node'))).toEqual([REE, MAG]);
    expect(chips[0].textContent).toBe('Rare-earth oxides & compoundsNOT STRESSED');
    expect(chips[1].getAttribute('aria-label')).toBe('Magnets, magnet parts & assemblies: BUILD; coordinated coverage 78%; best-ranked new-capacity host Carpania');
    expect(chips.every((b) => b.getAttribute('aria-pressed') === 'false')).toBe(true);
    // a click selects (pressed); Enter on a focused chip too (a native button)
    fireEvent.click(chips[1]);
    expect(onSelect).toHaveBeenLastCalledWith(MAG);
    expect(within(group).getAllByRole('button')[1].getAttribute('aria-pressed')).toBe('true');
    chips[0].focus();
    fireEvent.keyDown(chips[0], { key: 'ArrowRight' });
    expect(document.activeElement).toBe(chips[1]);
    fireEvent.keyDown(chips[1], { key: 'ArrowRight' });
    expect(document.activeElement).toBe(chips[1]);
    fireEvent.keyDown(chips[1], { key: 'Home' });
    expect(document.activeElement).toBe(chips[0]);
    fireEvent.keyDown(chips[0], { key: 'End' });
    expect(document.activeElement).toBe(chips[1]);
    // the legend: the statuses on the spine, in legend order
    expect(within(screen.getByTestId('spine-legend')).getAllByRole('listitem').map((li) => li.textContent)).toEqual(['BUILD', 'NOT STRESSED']);
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('the same nodes in every scenario: only the statuses change', () => {
    const { rerender } = render(<ValueChainSpine steps={spineOf(mags, 'baseline', nodeOf)} label="x" />);
    const statuses = () => screen.getAllByRole('button').map((b) => [b.getAttribute('data-node'), b.textContent!.replace(nodeOf(b.getAttribute('data-node')!)!.label, '')]);
    expect(statuses()).toEqual([
      [REE, 'NO SHOCK'],
      [MAG, 'NO SHOCK'],
    ]);
    rerender(<ValueChainSpine steps={spineOf(mags, 'decoupling', nodeOf)} label="x" />);
    expect(statuses()).toEqual([
      [REE, 'MANAGE SPECIALIZATION'],
      [MAG, 'BUILD'],
    ]);
  });
});

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('DivisionOfLabour', () => {
  it('a table of dots that speaks every cell, the legend, the measure choice, the table twin and a CSV', () => {
    const onSelectNode = vi.fn();
    function Card() {
      const [m, setM] = useState<DivisionMeasure>('supply');
      return <DivisionOfLabour matrix={divisionMatrix(s, mags, { measure: m, nodeOf })} name={name} onMeasure={setM} onSelectNode={onSelectNode} />;
    }
    render(<Card />);
    const card = screen.getByRole('region', { name: 'Who does what' });
    const table = within(card).getByRole('table', { name: /^Division of labour in Permanent Magnets & Drive Systems: physical capability by economy and node$/ });
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Economy', 'Separated oxidesRare-earth oxides & compounds', 'MagnetsMagnets, magnet parts & assemblies']);
    const rows = within(table).getAllByRole('rowheader');
    expect(rows.map((r) => r.textContent)).toEqual([
      'Zemblanon-alliedProcessing anchor',
      'CarpaniaProcessing anchor',
      'DorvaliaCapacity-building host',
      'AvalonDemand anchor',
      'BorduriaDemand anchor',
      'ElboniaBridge / coordination partner',
    ]);
    // readable on a phone (2026-10-21 review): headers in normal case and wrapping, the economy column sticky
    expect(rows[0].style.position).toBe('sticky');
    expect(rows[0].style.textTransform).toBe('none');
    expect(rows[0].style.whiteSpace).toBe('normal');
    expect(within(table).getAllByRole('columnheader')[1].style.textTransform).toBe('none');
    // every cell is spoken: its value and its evidence flags
    const avalon = rows[3].closest('tr')!;
    expect([...avalon.querySelectorAll('td')].map((td) => td.querySelector('.sr-only')!.textContent)).toEqual(['4% of world supply', '18% of world supply; anchor position, specialist position']);
    expect(avalon.querySelectorAll('td')[1].querySelectorAll('circle[fill="none"]')).toHaveLength(1);
    const bo = rows[4].closest('tr')!;
    expect(bo.querySelectorAll('td')[0].querySelector('.sr-only')!.textContent).toBe('no material position');
    // the legend: what the dots mean
    expect(within(card).getByTestId('division-legend').textContent).toMatch(/^Dots: Share of world supply of the node.*2% to 10%10% to 25%25% or more.*no economy is expected to build every node/);
    // a column header selects its node
    fireEvent.click(within(table).getByRole('button', { name: 'Magnets, magnet parts & assemblies' }));
    expect(onSelectNode).toHaveBeenCalledWith(MAG);
    // another measure: the dots and the legend follow
    fireEvent.click(within(card).getByRole('radio', { name: 'Investment host' }));
    const t2 = within(card).getByRole('table', { name: /investment host by economy and node$/ });
    expect(within(t2).getAllByRole('rowheader').map((r) => r.textContent!.replace(/(Processing anchor|Capacity-building host|Bridge \/ coordination partner)$/, ''))).toEqual(['Carpania', 'Dorvalia', 'Elbonia']);
    expect(within(card).getByTestId('division-legend').textContent).toMatch(/the best-ranked host/);
    // the table twin: the values as numbers
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    const twin = within(card).getByRole('table');
    expect(within(twin).getAllByRole('columnheader').map((h) => (h.textContent ?? '').replace(/[↑↓↕▲▼]/g, '').trim())).toEqual([
      'Economy', 'Allied', 'Role in the compact', 'Rare-earth oxides & compounds (Separated oxides)', 'Magnets, magnet parts & assemblies (Magnets)',
    ]);
    expect(within(twin).getAllByRole('row')[1].textContent).toContain('rank 1');
    expect(within(card).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    expect(bareAcronyms(card)).toEqual([]);
  });

  it('the CSV lists every cell with every measure and the flags spelt out; a capped matrix says how many economies are left out', () => {
    const m = divisionMatrix(s, mags, { nodeOf, maxRows: 2 });
    const rows = divisionCsvRows(m, name, (id) => nodeOf(id)!.label);
    expect(rows).toHaveLength(mags.division.length);
    expect(rows.find((r) => r.iso3 === 'CCC' && r.node_id === MAG)).toMatchObject({
      strategic_compact_id: 'SC-MAGNETS', economy: 'Carpania', node: 'Magnets, magnet parts & assemblies', value_chain_step: 'Magnets', share_of_world_supply: 0.12,
      share_of_allied_demand: 0.06, best_host_rank: 1, anchor_position: true, specialist_position: true, top_capable_host: true, momentum: false, dots_supply: 2,
    });
    render(<DivisionOfLabour matrix={m} name={name} title="Who does what in magnets" />);
    expect(screen.getByText('4 more economies have a smaller position; the CSV lists every one.')).toBeTruthy();
    expect(screen.queryByRole('radiogroup')).toBeNull();
  });
});

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('the strategic routes', () => {
  it('/compacts lands on the Strategic Sectoral Compacts: each card links to its compact and its workstreams, the toggle carried', async () => {
    const r = renderAt('/compacts?scn=decoupling');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const cards = await screen.findAllByTestId('strategic-compact-card');
    expect(cards).toHaveLength(s.compacts.length);
    const first = within(cards[0]);
    expect(first.getByRole('link', { name: 'View compact: Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling');
    expect(first.getByRole('link', { name: 'View workstreams: Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/workstreams?scn=decoupling#sc-SC-MAGNETS');
    await waitFor(() => expect(within(screen.getByRole('navigation', { name: 'Allied Industrial Base views' })).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page'));
    // the strategic compact page: the spine under the toggled scenario
    fireEvent.click(first.getByRole('link', { name: 'View compact: Permanent Magnets & Drive Systems' }));
    expect(await h1('Permanent Magnets & Drive Systems')).toBeTruthy();
    expect(r.state.location.pathname + r.state.location.search).toBe('/compacts/s/SC-MAGNETS?scn=decoupling');
    const spine = screen.getByRole('group', { name: 'Permanent Magnets & Drive Systems under Decoupling' });
    expect(within(spine).getAllByRole('button').map((b) => b.textContent)).toEqual(['Rare-earth oxides & compoundsMANAGE SPECIALIZATION', 'Magnets, magnet parts & assembliesBUILD']);
    await waitFor(() => expect(within(screen.getByRole('navigation', { name: 'Allied Industrial Base views' })).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page'));
    // the toggle: Managed Interdependence (the default): the same spine, other statuses
    fireEvent.click(screen.getByRole('radio', { name: 'Managed Interdependence' }));
    await waitFor(() => expect(r.state.location.search).toBe(''));
    const spine2 = await screen.findByRole('group', { name: 'Permanent Magnets & Drive Systems under Managed Interdependence' });
    expect(within(spine2).getAllByRole('button').map((b) => b.textContent)).toEqual(['Rare-earth oxides & compoundsNOT STRESSED', 'Magnets, magnet parts & assembliesBUILD']);
    // a workstream link opens the compact candidate's page (/compacts/:id kept)
    fireEvent.click(screen.getByRole('link', { name: 'AIB-C01' }));
    await waitFor(() => expect(r.state.location.pathname).toBe('/compacts/AIB-C01'), { timeout: 30_000 });
  });

  it('redirects: /compacts/SC-... to the strategic compact; the former landing\'s filters and sections to the Workstreams index; /compacts/s to the landing; an unknown strategic id to the landing with a notice', async () => {
    const r = renderAt('/compacts/sc-magnets?scn=decoupling#division');
    expect(await h1('Permanent Magnets & Drive Systems')).toBeTruthy();
    expect(r.state.location.pathname + r.state.location.search + r.state.location.hash).toBe('/compacts/s/SC-MAGNETS?scn=decoupling#division');
    cleanup();
    const r2 = renderAt('/compacts?tech=Solar#nodes');
    await waitFor(() => expect(r2.state.location.pathname).toBe('/compacts/workstreams'), { timeout: 30_000 });
    expect(r2.state.location.search + r2.state.location.hash).toBe('?tech=Solar#nodes');
    cleanup();
    const r3 = renderAt('/compacts/s?scn=decoupling');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(r3.state.location.pathname + r3.state.location.search).toBe('/compacts?scn=decoupling');
    cleanup();
    const r4 = renderAt('/compacts/s/SC-NOPE/extra');
    expect(await screen.findByText("No strategic compact with the id 'SC-NOPE'; showing all compacts.", undefined, { timeout: 30_000 })).toBeTruthy();
    expect(r4.state.location.pathname).toBe('/compacts');
    cleanup();
    // the workstreams keep their pages
    const r5 = renderAt('/compacts/workstreams/extra');
    await waitFor(() => expect(r5.state.location.pathname).toBe('/compacts/workstreams'), { timeout: 30_000 });
    // (the compact candidates are the Workstreams of the strategic compacts)
    expect(await h1('Workstreams')).toBeTruthy();
    expect(screen.getAllByRole('article').length).toBe(doc.compacts.length);
  });

  it('a bundle without the strategic layer lands on the Workstreams index', async () => {
    useSource((d) => void stripStrategic(d));
    const r = renderAt('/compacts');
    await waitFor(() => expect(r.state.location.pathname).toBe('/compacts/workstreams'), { timeout: 30_000 });
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    // the toggle keeps the generic framing
    expect(screen.getByTestId('scenario-framing').textContent).toMatch(/^Strategic compact = persistent architecture; scenario = the stress applied to it\./);
  });
});

// the directory of the view tests (AIB_DATA_DIR=public/data/aib: the real export)
describe.skipIf(!haveFixture(FIXTURE_DIR) || !readDoc(FIXTURE_DIR).strategic)('every strategic compact page (AIB_DATA_DIR)', () => {
  it('the landing lists every compact; each page draws its spine under both toggle choices and its division of labour, without a bare acronym', async () => {
    source = fixtureSource(FIXTURE_DIR);
    setAibStoreForTests(new AibStore(source));
    const fx = readDoc(FIXTURE_DIR);
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(await screen.findAllByTestId('strategic-compact-card')).toHaveLength(fx.strategic!.compacts.length);
    cleanup();
    for (const c of fx.strategic!.compacts) {
      for (const q of ['', '?scn=decoupling']) {
        renderAt(`/compacts/s/${c.id}${q}`);
        expect(await h1(c.name)).toBeTruthy();
        const nodes = c.steps.flatMap((x) => x.nodes);
        expect(within(screen.getByTestId('value-chain-spine')).getAllByRole('button').map((b) => b.getAttribute('data-node'))).toEqual(nodes);
        expect(screen.getByTestId('division-of-labour')).toBeTruthy();
        expect(bareAcronyms(document.querySelector('main') as HTMLElement)).toEqual([]);
        cleanup();
      }
    }
  }, 120_000);
});

function Probe() {
  const l = useAibStrategic();
  return <div data-testid="probe">{JSON.stringify({ loading: l.loading, missing: l.missing, error: l.error?.code ?? null, scenario: l.scenario, compacts: l.strategic?.compacts.length ?? null })}</div>;
}

const probe = () => JSON.parse(screen.getByTestId('probe').textContent ?? '{}') as Record<string, unknown>;

function renderProbe(path = '/x') {
  const router = createMemoryRouter([{ path: '/x', element: <Probe /> }], { initialEntries: [path] });
  render(<RouterProvider router={router} />);
}

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('useAibStrategic (anywhere under the router)', () => {
  it('reads the layer with the toggle\'s scenario; missing in a bundle without it; an error is an error', async () => {
    renderProbe();
    await waitFor(() => expect(probe()).toEqual({ loading: false, missing: false, error: null, scenario: 'managed', compacts: s.compacts.length }));
    cleanup();
    renderProbe('/x?scn=decoupling');
    await waitFor(() => expect(probe()).toMatchObject({ scenario: 'decoupling' }));
    cleanup();
    useSource((d) => void stripStrategic(d));
    renderProbe();
    await waitFor(() => expect(probe()).toEqual({ loading: false, missing: true, error: null, scenario: null, compacts: null }));
    cleanup();
    useSource((d) => void ((d as unknown as Record<string, unknown>).strategic = 'x'));
    renderProbe();
    await waitFor(() => expect(probe()).toMatchObject({ loading: false, missing: false, error: 'CONTRACT' }));
  });
});
