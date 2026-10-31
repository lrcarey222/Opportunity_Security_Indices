// The Strategic Sectoral Compact pages (2026-10-20, task s.14-16, s.20, s.21, s.24, s.26) on the synthetic
// fixture: the landing (cards grouped by family that carry only the name, supply-chain span, core countries
// with roles, and under the toggled scenario the mission, policy mix, active workstreams and result; the
// persistent parts unchanged by the toggle; links to the compact and its workstreams; the same compacts
// under every scenario with a table twin and a CSV); one compact's page (its seven questions in order, the
// spine whose chip or division column opens the node evidence with its status in every scenario and a link
// to its workstream's node detail, the vulnerabilities, missions, portfolio and menu, the core countries
// and division of labour linking to each country's role, the workstreams, and Managed Interdependence
// against Decoupling with Baseline as the reference); the Workstreams index relabelled, grouped by
// strategic compact (#sc-<id> marked) with each card's compact linked; a workstream's page naming its
// strategic compact and linking back; a bundle without the layer keeping the former words.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { resetLastCompact } from '../nav.ts';
import { compactFamilies } from '../views/StrategicCompactsView.tsx';
import { STRATEGIC_SECTIONS } from '../views/StrategicCompactView.tsx';
import { STRATEGIC_SECTION_TITLE } from '../views/strategic/shared.tsx';
import { fixtureSource, haveFixture, readDoc, stripStrategic, SYNTHETIC_DIR } from './fixture.ts';
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
function useSource(edit?: (d: AibDoc) => void): void {
  source = fixtureSource(SYNTHETIC_DIR, edit);
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
const main = () => document.querySelector('main') as HTMLElement;
const noAcronyms = () => expect(bareAcronyms(main())).toEqual([]);
/** The element's text as a reader sees it: its text nodes joined by a space (a term and its description do not run together), no space before punctuation. */
function text(el: Element | null | undefined): string {
  if (!el) return '';
  const w = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
  const parts: string[] = [];
  while (w.nextNode()) parts.push(w.currentNode.nodeValue ?? '');
  return parts.join(' ').replace(/\s+/g, ' ').replace(/\s+([,.;:)?!])/g, '$1').replace(/\(\s+/g, '(').trim();
}
const doc = readDoc(SYNTHETIC_DIR);
const s = doc.strategic!;
const MAG = 'ev-wind-mid-magnets';
const REE = 'ev-wind-mid-ree-compounds';

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('the Strategic Sectoral Compacts landing', () => {
  it('cards grouped by family carry only the seven items; the persistent parts stay when the toggle moves; links to the compact and its workstreams', async () => {
    const r = renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    // the families, in the export's order of first appearance
    const families = compactFamilies(s.compacts).map((f) => f.family);
    expect(families).toEqual(['Electrotechnology supply chains', 'Generation', 'Grid']);
    expect(within(main()).getAllByRole('heading', { level: 2 }).map((h) => h.textContent).slice(0, 3)).toEqual(families);
    expect(screen.getByTestId('strategic-reading').textContent).toMatch(/^Results below use Managed Interdependence \(the default view; choose Major-supplier disruption above for the stress test\)/);
    // the page's question first
    expect(screen.getByTestId('page-question').textContent).toBe('Which bottlenecks justify a Sectoral Compact?');
    const cards = await screen.findAllByTestId('strategic-compact-card');
    expect(cards.map((c) => c.getAttribute('data-compact'))).toEqual(s.compacts.map((c) => c.id));
    // only: name, supply chain, core countries, and under the scenario mission, policy mix, workstreams, result
    const mag = cards[0];
    expect(within(mag).getAllByRole('heading').map((h) => h.textContent)).toEqual(['Permanent Magnets & Drive Systems']);
    expect(within(mag).getAllByRole('term').map((t) => t.textContent)).toEqual(['Supply chain', 'Core partners', 'Mission', 'Actions needed', 'Workstreams', 'Result']);
    expect(within(mag).getByRole('list', { name: 'Supply chain of Permanent Magnets & Drive Systems, in order' }).textContent).toBe('Separated oxides→Magnets');
    expect(within(within(mag).getByRole('list', { name: /^Core countries of Permanent Magnets/ })).getAllByRole('listitem').map(text)).toEqual([
      'Avalon — demand anchor', 'Carpania — processing anchor', 'Dorvalia — capacity-building host', 'Elbonia — bridge / coordination partner',
    ]);
    const under = within(mag).getByTestId('compact-under-scenario');
    expect(text(under)).toBe(
      "Under Managed Interdependence Mission Build missing capacity in magnets. Actions needed Build capacity 1 of 2 supply-chain steps needs action: 1 Build capacity, 1 Unaffected Workstreams 1 of 1 workstream active Result 24% of the compact's allied demand (US$3.3bn) is lost; coverage 80% with existing relationships, 85% with coordinated trade, 89% after simulated new capacity.",
    );
    expect(within(mag).getByRole('link', { name: 'View compact: Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    expect(within(mag).getByRole('link', { name: 'View workstreams: Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/workstreams#sc-SC-MAGNETS');
    // a compact without a workstream says so
    const solar = cards.find((c) => c.getAttribute('data-compact') === 'SC-SOLAR')!;
    expect(text(within(solar).getByTestId('compact-under-scenario'))).toMatch(/Workstreams No workstream: none of the analysis’s clusters falls mainly in this compact/);
    // the toggle: the persistent parts are the same, the scenario's part follows it
    const persistent = cards.map((c) => text(within(c).getByTestId('compact-persistent')));
    fireEvent.click(screen.getByRole('radio', { name: 'Major-supplier disruption' }));
    await waitFor(() => expect(r.state.location.search).toBe('?scn=decoupling'));
    await waitFor(() => expect(text(within(screen.getAllByTestId('strategic-compact-card')[0]).getByTestId('compact-under-scenario'))).toMatch(/^Under Major-supplier disruption Mission Build missing capacity in magnets; preserve/));
    expect(screen.getAllByTestId('strategic-compact-card').map((c) => text(within(c).getByTestId('compact-persistent')))).toEqual(persistent);
    expect(within(screen.getAllByTestId('strategic-compact-card')[0]).getByRole('link', { name: 'View compact: Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling');
    noAcronyms();
  });

  it('the same compacts under every scenario: a row per compact, a cell per scenario, the toggled one marked; a table twin and a CSV', async () => {
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const card = screen.getByRole('region', { name: 'The same compacts under every scenario' });
    const rows = within(card).getByTestId('scenario-matrix').querySelectorAll(':scope > li');
    expect([...rows].map((li) => li.getAttribute('data-compact'))).toEqual(s.compacts.map((c) => c.id));
    const cells = [...rows[0].querySelectorAll('[data-scenario]')];
    expect(cells.map((c) => c.getAttribute('data-scenario'))).toEqual(['baseline', 'managed', 'partial', 'decoupling']);
    expect(cells.filter((c) => c.getAttribute('aria-current') === 'true').map((c) => c.getAttribute('data-scenario'))).toEqual(['managed']);
    // the same nodes in every cell: one square per node
    for (const c of cells) expect(c.querySelectorAll('[data-testid="mini-spine"] > span > span')).toHaveLength(2);
    expect(cells.map((c) => text(c.querySelector('dd > span:not(.sr-only):not([data-testid])')))).toEqual([
      'no disruption',
      'Build capacity · 1 of 2 supply-chain steps needs action',
      'Coordinate trade + Manage specialization · 2 of 2 supply-chain steps need action',
      'Build capacity + Manage specialization + Coordinate trade · 2 of 2 supply-chain steps need action',
    ]);
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    expect(within(within(card).getByRole('table')).getAllByRole('row')).toHaveLength(s.compacts.length + 1);
    expect(within(card).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    noAcronyms();
  });
});

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('one Strategic Sectoral Compact', () => {
  it('the seven questions in order; the spine opens a node’s evidence in every scenario and links to its workstream; the division’s column does too', async () => {
    renderAt('/compacts/s/SC-MAGNETS');
    expect(await h1('Permanent Magnets & Drive Systems')).toBeTruthy();
    expect(within(main()).getAllByRole('heading', { level: 2 }).map((h) => h.textContent)).toEqual(STRATEGIC_SECTIONS.map(([, t]) => t));
    expect(within(screen.getByRole('navigation', { name: 'Sections of this compact' })).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(STRATEGIC_SECTIONS.map(([id]) => `#${id}`));
    expect(screen.getByTestId('compact-facts').textContent).toBe('Electrotechnology supply chains · 2 stages · 2 supply-chain steps · 1 workstream');
    // why: the mission and the figures under the toggled scenario
    const why = screen.getByTestId('compact-mission');
    expect(within(why).getByText('Build missing capacity in magnets.')).toBeTruthy();
    expect(text(why)).toMatch(/Supply to shift\s?\$3\.3 bn\s?24% of allied demand/);
    // the spine: a chip opens the node's evidence
    expect(screen.getByTestId('spine-hint')).toBeTruthy();
    const spine = screen.getByRole('group', { name: 'Permanent Magnets & Drive Systems under Managed Interdependence' });
    fireEvent.click(within(spine).getAllByRole('button')[1]);
    const ev = screen.getByTestId('node-evidence');
    expect(ev.getAttribute('data-node')).toBe(MAG);
    expect(within(ev).getByRole('heading', { level: 3 }).textContent).toBe('Magnets, magnet parts & assemblies');
    const across = within(ev).getByRole('list', { name: 'Magnets, magnet parts & assemblies: status in each scenario' });
    expect(within(across).getAllByRole('listitem').map((li) => text(li).split(' served')[0])).toEqual([
      "Today's network: No disruption", 'Managed Interdependence: Build capacity shown', 'Partial supplier disruption: Coordinate trade', 'Major-supplier disruption: Build capacity',
    ]);
    expect(within(ev).getByRole('link', { name: /^Full evidence for this step in workstream AIB-C01/ }).getAttribute('href')).toBe('/compacts/AIB-C01#nodes');
    expect(text(within(ev).getByRole('list', { name: /status in each scenario/ }).querySelector('[aria-current="true"]'))).toMatch(/best-ranked host for new capacity Carpania/);
    expect(text(ev)).toMatch(/Largest suppliers \(world supply\) Zembla 62%, Avalon 18%, Carpania 12%, Borduria 4%/);
    fireEvent.click(within(ev).getByRole('button', { name: 'Close the evidence for Magnets, magnet parts & assemblies' }));
    expect(screen.queryByTestId('node-evidence')).toBeNull();
    // the division of labour's column header selects the node too
    const division = screen.getByTestId('division-of-labour');
    fireEvent.click(within(division).getByRole('button', { name: 'Rare-earth oxides & compounds' }));
    expect(screen.getByTestId('node-evidence').getAttribute('data-node')).toBe(REE);
    // the evidence opens in the supply chain above: focus moves to its heading (2026-10-21 review)
    await waitFor(() => expect(document.activeElement?.id).toBe(`node-evidence-${REE}`));
    expect(within(screen.getByRole('group', { name: /under Managed Interdependence$/ })).getAllByRole('button')[0].getAttribute('aria-pressed')).toBe('true');
    noAcronyms();
  });

  it('vulnerabilities, missions, portfolio and menu; core countries and the division link to each country’s role; the workstreams link to their pages', async () => {
    renderAt('/compacts/s/SC-MAGNETS');
    expect(await h1('Permanent Magnets & Drive Systems')).toBeTruthy();
    const conc = screen.getByTestId('concentration');
    expect(text(conc)).toMatch(/Leading suppliers outside the alliance Zembla — processing anchor: 70% share of world exports or production of rare-earth oxides & compounds/);
    expect(text(conc)).toMatch(/Critical dependencies outside the alliance \(Managed Interdependence\) Zembla/);
    expect(text(conc)).toMatch(/Allied partners the compact cannot do without \(Managed Interdependence\) Carpania/);
    expect(within(screen.getByRole('list', { name: 'Supply-chain steps with the largest gap under Managed Interdependence' })).getAllByRole('listitem').map((li) => li.getAttribute('data-node'))).toEqual([MAG]);
    // missions (each with the export's own basis), the four-treatment portfolio in plain words and the menu from the steps' treatments
    expect(text(screen.getByTestId('missions'))).toMatch(/^Missions Primary mission Build missing capacity in magnets BUILD/);
    expect(within(screen.getByRole('list', { name: 'Supply-chain steps by treatment under Managed Interdependence' })).getAllByRole('listitem').map(text)).toEqual([
      '1 Build capacity', '0 Coordinate trade', '0 Manage specialization', '0 Low intervention',
    ]);
    expect(screen.getByTestId('portfolio-rest').textContent).toBe('The other 1 supply-chain step: 1 unaffected under Managed Interdependence.');
    const menu = screen.getByTestId('policy-menu');
    expect(within(within(menu).getByRole('list', { name: 'Instruments for Build capacity' })).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      'coordinated investment', 'DFI / ECA financing', 'guarantees', 'price stabilization', 'long-term offtake', 'demand aggregation',
    ]);
    expect(within(menu).getByText('For magnets, magnet parts & assemblies')).toBeTruthy();
    // who does what: the core countries link to their role in the Allied Industrial Base, and so do the division's rows
    const core = within(screen.getByRole('list', { name: 'Core countries of Permanent Magnets & Drive Systems' })).getAllByRole('listitem');
    expect(core.map((li) => li.getAttribute('data-iso3'))).toEqual(['AAA', 'CCC', 'DDD', 'EEE']);
    expect(within(core[0]).getByRole('link', { name: 'Avalon' }).getAttribute('href')).toBe('/countries/AAA#aib-role');
    expect(text(core[0])).toMatch(/^Avalon Demand anchor Also: specialized technology supplier Strongest at Magnets, magnet parts & assemblies \(36% share of world imports\)/);
    expect(text(core[0])).toMatch(/Its overall role Demand & Market Anchor; Capital & Investment Anchor$/);
    const division = screen.getByTestId('division-of-labour');
    expect(within(division).getByRole('link', { name: 'Carpania' }).getAttribute('href')).toBe('/countries/CCC#aib-role');
    // the workstreams underneath
    const ws = screen.getByRole('list', { name: 'Workstreams of Permanent Magnets & Drive Systems' });
    const card = within(ws).getByRole('article');
    expect(card.getAttribute('data-workstream')).toBe('AIB-C01');
    expect(within(card).getByRole('link', { name: 'AIB-C01' }).getAttribute('href')).toBe('/compacts/AIB-C01');
    expect(within(card).getByText('Active under Managed Interdependence')).toBeTruthy();
    expect(text(card)).toMatch(/Filed under this compact, which holds 100% of its allied demand/);
    expect(screen.getByRole('link', { name: /^All workstreams, by compact/ }).getAttribute('href')).toBe('/compacts/workstreams#sc-SC-MAGNETS');
    noAcronyms();
  });

  it('what changes: Managed Interdependence against Decoupling with Baseline as the reference, every node in every scenario; a compact without a workstream says so', async () => {
    renderAt('/compacts/s/SC-MAGNETS?scn=decoupling');
    expect(await h1('Permanent Magnets & Drive Systems')).toBeTruthy();
    const section = screen.getByRole('region', { name: STRATEGIC_SECTION_TITLE.scenarios });
    expect(text(section)).toMatch(
      /^What changes under each scenario\? Under Managed Interdependence, 1 of 2 supply-chain steps needs action and \$3\.3 bn \(24%\) of the compact’s allied demand has to shift; under Major-supplier disruption, 2 of 2 supply-chain steps need action and \$9\.1 bn \(66%\) is lost\. 1 supply-chain step of 2 changes what it needs between the two\./,
    );
    const table = within(screen.getByTestId('scenario-compare')).getByRole('table');
    const heads = within(table).getAllByRole('columnheader');
    expect(heads.slice(1).map((h) => h.getAttribute('data-scenario'))).toEqual(['baseline', 'managed', 'decoupling']);
    expect(heads.filter((h) => h.getAttribute('aria-current') === 'true').map((h) => h.getAttribute('data-scenario'))).toEqual(['decoupling']);
    const row = (key: string) => [...table.querySelectorAll(`tr[data-row="${key}"] td`)].map(text);
    expect(row('mix')).toEqual(['no disruption', 'Build capacity', 'Build capacity + Manage specialization + Coordinate trade']);
    expect(row('nodes')).toEqual(['0 of 2', '1 of 2', '2 of 2']);
    expect(row('hosts')).toEqual(['not simulated', 'Carpania', 'Dorvalia']);
    // every node in every scenario, the changed one marked
    const grid = screen.getByTestId('node-status-grid');
    expect(within(grid).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Supply-chain step', "Today's network", 'Managed Interdependence', 'Partial supplier disruption', 'Major-supplier disruption']);
    const nodeRow = (id: string) => grid.querySelector(`tr[data-node="${id}"]`) as HTMLElement;
    expect([...nodeRow(REE).querySelectorAll('td')].map(text)).toEqual(['No disruption', 'Unaffected', 'Manage specialization', 'Manage specialization']);
    expect(text(nodeRow(REE).querySelector('th'))).toMatch(/changed$/);
    expect(text(nodeRow(MAG).querySelector('th'))).not.toMatch(/changed$/);
    expect(screen.getByRole('region', { name: 'Allied demand met under each scenario' })).toBeTruthy();
    noAcronyms();
    cleanup();
    renderAt('/compacts/s/sc-solar');
    expect(await h1('Solar Manufacturing')).toBeTruthy();
    expect(text(screen.getByRole('region', { name: STRATEGIC_SECTION_TITLE.workstreams }))).toMatch(/No workstream: none of the analysis’s clusters falls mainly in this compact/);
  });
});

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('the Workstreams and their strategic compacts', () => {
  it('the Workstreams index: grouped by strategic compact (the target marked), each card names its compact; a workstream links back up', async () => {
    const r = renderAt('/compacts/workstreams#sc-SC-BATTERIES');
    expect(await h1('Workstreams')).toBeTruthy();
    expect(screen.getByRole('link', { name: '← Strategic Sectoral Compacts' }).getAttribute('href')).toBe('/compacts');
    const groups = within(screen.getByRole('list', { name: 'Strategic Sectoral Compacts and their workstreams' })).getAllByRole('listitem').filter((li) => li.id.startsWith('sc-'));
    expect(groups.map((g) => g.id)).toEqual(s.compacts.map((c) => `sc-${c.id}`));
    expect(groups.filter((g) => g.getAttribute('aria-current') === 'true').map((g) => g.id)).toEqual(['sc-SC-BATTERIES']);
    expect(within(groups[0]).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    expect(text(groups[0])).toMatch(/1 workstream, 1 active under Managed Interdependence/);
    expect(text(groups[3])).toMatch(/No workstream/);
    // each card names its strategic compact; its link is the compact workstream's page
    const cards = within(screen.getByRole('region', { name: `The workstreams (${doc.compacts.length})` })).getAllByRole('article');
    const c1 = cards.find((c) => c.getAttribute('data-compact') === 'AIB-C01')!;
    expect(text(within(c1).getByTestId('workstream-home'))).toBe('Workstream of Permanent Magnets & Drive Systems');
    expect(within(within(c1).getByTestId('workstream-home')).getByRole('link').getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    expect(within(c1).getByRole('link', { name: /^View compact workstream: / }).getAttribute('href')).toBe('/compacts/AIB-C01');
    noAcronyms();
    // a workstream's page: its strategic compact above it, and a way back
    fireEvent.click(within(c1).getByRole('link', { name: /^View compact workstream: / }));
    await waitFor(() => expect(r.state.location.pathname).toBe('/compacts/AIB-C01'), { timeout: 30_000 });
    await screen.findByTestId('workstream-parent', undefined, { timeout: 30_000 });
    expect(main().querySelector('header .eyebrow')?.textContent).toBe('WORKSTREAM · AIB-C01');
    const parent = screen.getByTestId('workstream-parent');
    expect(text(parent)).toBe('A workstream of the Strategic Sectoral Compact Permanent Magnets & Drive Systems, which holds 100% of its allied demand. Active under Managed Interdependence.');
    expect(within(parent).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS');
    const back = within(screen.getByTestId('workstream-back')).getAllByRole('link');
    expect(back.map((a) => [a.textContent, a.getAttribute('href')])).toEqual([
      ['← Permanent Magnets & Drive Systems', '/compacts/s/SC-MAGNETS#workstreams'],
      ['All workstreams', '/compacts/workstreams#sc-SC-MAGNETS'],
    ]);
    expect(screen.getByRole('heading', { level: 2, name: 'Why this workstream?' })).toBeTruthy();
    expect(screen.getByRole('navigation', { name: 'Sections of this workstream' })).toBeTruthy();
    noAcronyms();
  });

  it('a bundle without the strategic layer keeps the former words: Sectoral Compacts, View compact, Why this compact?', async () => {
    useSource((d) => void stripStrategic(d));
    renderAt('/compacts/workstreams');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(screen.queryByRole('region', { name: 'Workstreams by strategic compact' })).toBeNull();
    expect(screen.queryAllByTestId('workstream-home')).toEqual([]);
    const cards = within(screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` })).getAllByRole('article');
    expect(within(cards[0]).getByRole('link', { name: /^View compact: / })).toBeTruthy();
    cleanup();
    renderAt(`/compacts/${doc.compacts[0].id}`);
    expect(await screen.findByRole('heading', { level: 2, name: 'Why this compact?' }, { timeout: 30_000 })).toBeTruthy();
    expect(screen.queryByTestId('workstream-parent')).toBeNull();
    expect(main().querySelector('header .eyebrow')?.textContent).toBe(`SECTORAL COMPACT · ${doc.compacts[0].id}`);
    expect(screen.getByRole('link', { name: '← All compacts' }).getAttribute('href')).toBe('/compacts');
  });
});
