// The Country page's canonical framework on the real route (spec XXII UI; design 4, 5b, D1, D8, D10, D14; 2026-10-29):
// the R-written Country fixture with the posture fixture's Aland served as the United States. The Strategy Map renders all
// six stages; the Deployment & Use column is set apart; Accelerate and Watch markers render; the Need panels, the Prize &
// Position scatter, the six Strategic Choices groups, the Policy Alignment table and the Allied Strategy (from the canonical
// partner outputs and the AIB status) render; the Legacy OSI framework stays out of sections 1-6 (no dials, no engine);
// every entry point opens the same drawer through ?node= (and a deep link opens it); and the page stays usable at phone
// width (the chip nav, the matrix's own scroller, the bottom-sheet drawer).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import { setPostureStore } from '../../posture/store.ts';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { STRATEGY_SECTIONS } from '../derive/strategyPage.ts';
import { fixtureSource, haveFixture } from './fixture.ts';

const restore: (() => void)[] = [];
let wide = true;

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => (wide ? 900 : 360) });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  const mm = window.matchMedia;
  // wide: every min-width query matches (a desktop); narrow: none does (a phone)
  window.matchMedia = ((q: string) => ({
    matches: wide && /min-width/.test(q),
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

let engine: EngineClient;
beforeEach(() => {
  wide = true;
  setCountryStoreForTests(new CountryStore(fixtureSource()));
  setPostureStore(fixtureStore({ economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
  engine = new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/');
  setEngineClient(engine);
});

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
  setPostureStore(null);
  setEngineClient(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const sec = (id: string) => document.getElementById(id) as HTMLElement;
const nodeOf = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search).get('node');

/** The profile of the United States with the strategy loaded (the Country Strategy Map drawn). */
async function usa(path = '/countries/USA'): Promise<ReturnType<typeof renderAt>> {
  const r = renderAt(path);
  await screen.findByRole('heading', { level: 1, name: 'United States' }, { timeout: 30_000 });
  // the h1 shows while the fact sheet loads too: wait for the Strategy section's map
  await screen.findByRole('region', { name: 'Country Strategy Map' }, { timeout: 30_000 });
  expect(sec('strategy')).toBeTruthy();
  return r;
}

/** The drawer's node heading once it is open. */
async function drawerTitle(): Promise<string> {
  const d = await screen.findByTestId('cell-drawer', {}, { timeout: 30_000 });
  return within(d).getByRole('heading', { level: 3 }).textContent ?? '';
}

describe.skipIf(!haveFixture())('Country page: the canonical framework (spec XXII UI)', () => {
  it('the Strategy Map renders all six stages, sets the Deployment & Use column apart and draws the Accelerate and Watch markers', async () => {
    await usa();
    const strategy = sec('strategy');
    const grid = within(strategy).getByRole('grid', { name: /^Country Strategy Map/ });
    const heads = within(grid).getAllByRole('columnheader').slice(1);
    expect(heads.map((h) => h.getAttribute('title'))).toEqual(SUPPLY_STAGES.map((s) => STAGE_DISPLAY[s]));
    expect(heads.map((h) => h.getAttribute('title'))).toEqual(['Extraction', 'Refining & Processing', 'Component Manufacturing', 'Final Assembly & Equipment', 'Deployment & Use', 'Recovery & Recycling']);
    // Deployment & Use: its header and cells marked, the band and the helper words (spec XI.5)
    expect(heads.filter((h) => h.getAttribute('data-deployment') === 'true').map((h) => h.getAttribute('title'))).toEqual(['Deployment & Use']);
    expect(grid.querySelectorAll('[role="gridcell"][data-deployment="true"]')).toHaveLength(2);
    expect(within(strategy).getByTestId('strategy-map-band').textContent).toMatch(/Supply-chain strategy →.*Energy-system strategy.*→ Circularity/);
    expect(within(strategy).getByTestId('strategy-map-helper').textContent).toMatch(/Deployment & Use column is set apart/);
    // the markers: Accelerate (up arrow) and Watch (dot) at Deployment, Watch at a producing stage too; P, C, linked arrows for Compete
    const cell = (name: RegExp) => within(grid).getByRole('button', { name }) as HTMLElement;
    const mark = (b: HTMLElement) => within(b).getByTestId('cell-mark').textContent;
    expect(mark(cell(/^Batteries Deployment & Use: Deploy · Accelerate/))).toBe('↑');
    expect(cell(/^Batteries Deployment & Use/).getAttribute('data-mode')).toBe('accelerate');
    expect(mark(cell(/^Solar Deployment & Use: Deploy · Watch/))).toBe('•');
    expect(mark(cell(/^Solar Extraction: Deploy · Watch/))).toBe('•');
    expect(mark(cell(/^Batteries Final Assembly & Equipment: Compete · Preserve/))).toBe('P');
    expect(mark(cell(/^Batteries Component Manufacturing: Compete · Catch Up/))).toBe('C');
    expect(mark(cell(/^Batteries Extraction: Compete · Partner/))).toBe('⇄');
    // three background treatments only (Secure, Compete, Deploy) plus the hatched not-assessed node
    const postures = [...grid.querySelectorAll('button[data-posture]')].map((b) => b.getAttribute('data-posture'));
    expect(new Set(postures)).toEqual(new Set(['S', 'C', 'D', 'N']));
    expect(cell(/^Batteries Recovery & Recycling: Not assessed/).getAttribute('data-posture')).toBe('N');
    // the map leads the section (spec XI.3, the dominant visual): no intro paragraph between the heading and
    // the map, the full reading of how it is built after it
    const heading = strategy.querySelector('h2')!;
    expect(heading.nextElementSibling?.tagName).not.toBe('P');
    expect(within(strategy).queryByText(/^One recommended choice for every technology/)).toBeNull();
    const how = within(strategy).getByTestId('strategy-how');
    expect(how.textContent).toMatch(/^How the map is built\. For every technology at each of the six supply-chain stages/);
    expect(how.textContent).toMatch(/Its thresholds are fixed, not a weighting of preferences/);
    // the Legacy OSI's Perspective dials are not named in the strategy's own words (they live in Legacy OSI)
    expect(how.textContent).not.toMatch(/Perspective|dials/);
    expect(grid.compareDocumentPosition(how) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  });

  it('the Need panels render: supply-chain need by producing node with reason tags, deployment need by technology with named scenarios', async () => {
    await usa();
    const need = sec('need');
    expect(within(need).getByText('What problems does this country need to solve?')).toBeTruthy();
    const supply = await within(need).findByTestId('supply-need-ranking', {}, { timeout: 30_000 });
    const rows = within(supply).getAllByRole('button');
    // producing stages only: no Deployment & Use row, Secure nodes first
    expect(rows.every((b) => !/Deployment & Use/.test(b.getAttribute('aria-label') ?? ''))).toBe(true);
    expect(rows[0].getAttribute('aria-label')).toMatch(/^Batteries Refining & Processing: supply-chain need 95; Secure/);
    expect(within(supply).getAllByTestId('need-tags').some((t) => /CHOKEPOINT/.test(t.textContent ?? ''))).toBe(true);
    const dep = within(need).getByTestId('deployment-need-ranking');
    expect(within(dep).getAllByRole('button').map((b) => b.getAttribute('data-key'))).toEqual(['Batteries|Deployment', 'Solar|Deployment']);
    await waitFor(() => expect(within(need).getByTestId('deployment-scenarios').textContent).toMatch(/Current policy.*National commitments/), { timeout: 30_000 });
    // the energy-system context cards wait in a disclosure
    expect(within(need).getByText(/^Energy-system context:/)).toBeTruthy();
  });

  it('the Prize & Position scatter renders over the producing nodes: Industrial Opportunity across, Competitive Position up, Secure outlined at its place', async () => {
    await usa();
    const pp = sec('prize-position');
    const scatter = within(pp).getByTestId('prize-position-scatter');
    const points = [...scatter.querySelectorAll('[data-verb]')];
    expect(points.length).toBeGreaterThan(0);
    expect(new Set(points.map((p) => p.getAttribute('data-verb')))).toEqual(new Set(['secure', 'preserve', 'catch_up', 'partner', 'watch']));
    expect(points.every((p) => !/Deployment & Use/.test(p.getAttribute('aria-label') ?? ''))).toBe(true);
    expect(within(pp).getByRole('region', { name: 'Where is the prize, and where is the country already strong?' })).toBeTruthy();
    // a point's words: need, opportunity, position, momentum, alignment, evidence
    const preserve = points.find((p) => /^Batteries Final Assembly & Equipment/.test(p.getAttribute('aria-label') ?? ''))!;
    expect(preserve.getAttribute('aria-label')).toMatch(/Industrial Opportunity 90%; Competitive Position 95%; supply-chain need 40%; trade momentum/);
    // the monthly trade card follows the selected node (the United States reports monthly trade in the fixture)
    expect(within(pp).queryByTestId('trade-monthly') ?? within(pp).queryByText(/Loading monthly trade/)).toBeTruthy();
  });

  it('the Strategic Choices groups render, one per verb, with the review list and the nodes not assessed', async () => {
    await usa();
    const choices = sec('choices');
    const groups = await within(choices).findAllByTestId('choice-group', {}, { timeout: 30_000 });
    expect(groups.map((g) => g.getAttribute('data-verb'))).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch']);
    expect(groups.map((g) => within(g).getByRole('heading', { level: 4 }).textContent?.replace(/\d+$/, ''))).toEqual([
      // each heading carries its marker: P, C, linked arrows, the Accelerate up arrow and the Watch dot
      'Secure', 'PCompete · Preserve', 'CCompete · Catch Up', '⇄Compete · Partner', '↑Deploy · Accelerate', '•Deploy · Watch',
    ]);
    expect(within(groups[5]).getByText('Do not prioritize domestic industrial intervention')).toBeTruthy();
    expect(within(choices).getByTestId('choice-review')).toBeTruthy();
    expect(within(choices).getByTestId('choice-not-assessed')).toBeTruthy();
    // Partner links to the allied division of the AIB (a producing node)
    expect(within(groups[3]).getByTestId('choice-aib-link').getAttribute('href')).toBe('/aib#division');
  });

  it('Policy Alignment renders: node, recommendation, observed intensity, support, protection, momentum and alignment; the observed-policy evidence below', async () => {
    await usa();
    const pol = sec('policy-alignment');
    const table = within(within(pol).getByTestId('policy-alignment')).getByRole('table');
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Node', 'Recommendation', 'Observed policy intensity', 'Support', 'Protection', 'Policy momentum', 'Alignment']);
    await waitFor(() => expect(table.querySelector('[data-key="Batteries|Extraction"]')?.textContent).toMatch(/Compete · Partner.*misaligned/i), { timeout: 30_000 });
    expect(table.querySelector('[data-key="Batteries|Deployment"]')?.getAttribute('data-alignment')).toBe('deployment_gap');
    // no overall score; the evidence beneath carries no Strategic Index comparison (a Legacy OSI reading)
    const ev = within(pol).getByTestId('policy-behaviour');
    expect(ev.textContent).toMatch(/Climate Action Tracker/);
    expect(ev.textContent).not.toMatch(/Strategic Index/);
  });

  it('Allied Strategy uses the canonical partner outputs and the AIB status; the generic partnership material waits under a closed disclosure', async () => {
    await usa();
    const allied = sec('allied');
    // the opening sentence, computed from the outputs (the Secure nodes' delivery comes with the detail document)
    await waitFor(
      () => expect(within(allied).getByTestId('allied-sentence').textContent).toMatch(/^1 of the United States' 7 priority nodes is better solved through allies; 2 Secure nodes also depend on trusted foreign capacity/),
      { timeout: 30_000 },
    );
    const list = within(allied).getByTestId('allied-strategy');
    const items = within(list).getAllByRole('listitem').filter((li) => li.hasAttribute('data-key'));
    expect(items.map((li) => li.getAttribute('data-key'))).toEqual(['Batteries|Extraction', 'Batteries|Refining', 'Solar|Refining']);
    // the Partner node: its recommended partners from the detail document, the AIB status from the allied division
    await waitFor(() => expect(within(items[0]).getByText('Bland')).toBeTruthy(), { timeout: 30_000 });
    expect(within(items[0]).getByText('Cland')).toBeTruthy();
    expect(items[0].getAttribute('data-status')).toBe('covered');
    expect(within(items[1]).getByTestId('allied-status').textContent).toMatch(/Gap/);
    expect(within(items[0]).getByTestId('allied-aib-link').getAttribute('href')).toBe('/aib#division');
    const explore = within(allied).getByTestId('allied-explore');
    expect(within(explore).getByText('Explore all partnership analysis')).toBeTruthy();
    expect(document.getElementById('fold-partnerships-explore')?.hasAttribute('open')).toBe(false);
    expect(within(allied).queryByRole('heading', { name: 'Partnerships' })).toBeNull();
    fireEvent.click(within(explore).getByText('Explore all partnership analysis'));
    expect(await within(allied).findByRole('heading', { level: 3, name: 'Partnerships' }, { timeout: 30_000 })).toBeTruthy();
    expect(within(allied).getByRole('heading', { level: 3, name: 'Role in the Allied Industrial Base' })).toBeTruthy();
  });

  it('the Legacy OSI framework stays out of sections 1-6: no dials, no corners, no Strategic Index, and the engine never starts', async () => {
    await usa();
    for (const s of STRATEGY_SECTIONS.slice(0, 6)) {
      const t = sec(s.id).textContent ?? '';
      expect(t, s.id).not.toMatch(/Energy Security|Economic Opportunity|Strategic Index|Perspective: |Perspective dials|\bdials?\b|four corners|\bGrow\b|\bExposed\b|NEIS baseline/);
    }
    expect(screen.queryByRole('button', { name: /^Perspective:/ })).toBeNull();
    expect(sec('legacy-osi').closest('section#evidence')).toBeTruthy();
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(false);
    // the dials sit in the Legacy OSI results as "Explore scenarios", apart from the strategy
    fireEvent.click(screen.getByText(/^Show the Legacy OSI results for the United States/));
    const explore = await screen.findByTestId('explore-scenarios', {}, { timeout: 30_000 });
    expect(within(explore).getByRole('heading', { level: 3, name: 'Explore scenarios' })).toBeTruthy();
    expect(within(explore).getByText(/never change the strategy in the sections above/)).toBeTruthy();
    expect(within(explore).getByRole('button', { name: /^Perspective: NEIS baseline/ })).toBeTruthy();
    expect(engine.getSnapshot().manifest).toBeNull();
    // Evidence & Methodology: the framework chain, the stage mapping, coverage and freshness, the downloads
    const ev = sec('evidence');
    expect(within(ev).getByTestId('framework-chain').textContent).toMatch(/Need → Prize → Position → Choice → Policy Alignment/);
    // Watch has both routes (as Methods and the glossary say): low need with a low prize, and low need with a high prize but no domestic case
    expect(within(ev).getByTestId('framework-chain').textContent).toContain('low need is Watch, with a low prize and also with a high prize where there is neither a strong position nor a credible pathway.');
    expect(within(ev).getByTestId('framework-chain').textContent).not.toMatch(/low need and low prize is Watch/);
    expect(within(within(ev).getByTestId('stage-mapping')).getAllByRole('rowheader').map((h) => h.textContent)).toEqual(SUPPLY_STAGES.map((s, i) => `${i + 1}. ${STAGE_DISPLAY[s]}`));
    const cov = within(ev).getByTestId('evidence-coverage');
    expect(cov.textContent).toMatch(/11 of 12 technology and stage nodes assessed; 1 not assessed/);
    expect(cov.textContent).toMatch(/Structural trade position over 2022-2024/);
    expect(cov.textContent).toMatch(/Sep 2025 - Aug 2026 against Sep 2024 - Aug 2025 \(through Aug 2026\)/);
    expect(sec('downloads').closest('section#evidence')).toBeTruthy();
    expect(bareAcronyms(document.body)).toEqual([]);
  });

  it('cross-navigation: the map, a Need row, a scatter point, a choice card, a policy row and an allied node all open the same drawer through ?node=', async () => {
    const r = await usa();
    const open = async (click: () => void, key: string, title: string) => {
      click();
      await waitFor(() => expect(nodeOf(r)).toBe(key), { timeout: 30_000 });
      expect(await drawerTitle()).toBe(title);
      expect(screen.getAllByTestId('cell-drawer')).toHaveLength(1);
      // the five questions, in order
      expect(within(screen.getByTestId('cell-drawer')).getAllByTestId('drawer-question').map((q) => q.getAttribute('data-question'))).toEqual(['need', 'prize', 'position', 'choice', 'policy']);
    };
    await open(() => fireEvent.click(within(sec('strategy')).getByRole('button', { name: /^Solar Extraction/ })), 'Solar|Extraction', 'Solar · Extraction');
    await open(() => fireEvent.click(within(within(sec('need')).getByTestId('supply-need-ranking')).getAllByRole('button')[1]), 'Solar|Refining', 'Solar · Refining & Processing');
    await open(() => fireEvent.click(within(sec('prize-position')).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ })), 'Batteries|Assembly', 'Batteries · Final Assembly & Equipment');
    await open(() => fireEvent.click(within(within(sec('choices')).getAllByTestId('choice-group')[2]).getByRole('button', { name: /Batteries · Component Manufacturing/ })), 'Batteries|Components', 'Batteries · Component Manufacturing');
    const policyRow = within(sec('policy-alignment')).getByTestId('policy-alignment').querySelector('[data-key="Solar|Deployment"]') as HTMLElement;
    await open(() => fireEvent.click(within(policyRow).getByRole('button')), 'Solar|Deployment', 'Solar · Deployment & Use');
    const alliedItem = within(sec('allied')).getAllByRole('listitem').find((li) => li.getAttribute('data-key') === 'Batteries|Extraction')!;
    await open(() => fireEvent.click(within(alliedItem).getAllByRole('button')[0]), 'Batteries|Extraction', 'Batteries · Extraction');
    // the selected node is pressed on the map too
    expect(within(sec('strategy')).getByRole('button', { name: /^Batteries Extraction/ }).getAttribute('aria-pressed')).toBe('true');
  }, 90_000);

  it('a shared link opens its node; a node the data do not know is reported as ignored', async () => {
    const r = await usa('/countries/USA?node=Batteries%7CRefining');
    expect(await drawerTitle()).toBe('Batteries · Refining & Processing');
    expect(nodeOf(r)).toBe('Batteries|Refining');
    cleanup();
    await usa('/countries/USA?node=Fusion%7CRefining');
    expect(await screen.findByText('Ignored in the link', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/node=Fusion\|Refining \(Fusion is not a technology of the posture data; ignored\)/)).toBeTruthy();
    expect(screen.queryByTestId('cell-drawer')).toBeNull();
    cleanup();
    // an old three-stage stage maps to its one six-stage successor, with a notice
    const r3 = await usa('/countries/USA?node=Solar%7CDownstream');
    expect(await drawerTitle()).toBe('Solar · Deployment & Use');
    expect(nodeOf(r3)).toBe('Solar|Downstream');
  });

  it('the monthly trade card follows the selected node, and picking a sector there selects that node', async () => {
    const r = await usa('/countries/USA?node=Solar%7CAssembly');
    const card = await within(sec('prize-position')).findByTestId('trade-monthly', {}, { timeout: 30_000 });
    expect(within(card).getByRole('heading', { level: 3, name: /^Solar — Assembly: monthly imports by sub-component/ })).toBeTruthy();
    const pick = within(card).getByRole('combobox', { name: 'Sector' }) as HTMLSelectElement;
    const batteries = [...pick.options].find((o) => o.value === 'batteries-assembly');
    if (batteries) {
      fireEvent.change(pick, { target: { value: 'batteries-assembly' } });
      await waitFor(() => expect(nodeOf(r)).toBe('Batteries|Assembly'));
    }
  });

  it('narrow width (phone): the section chips scroll sideways, the matrix keeps its own scroller, the drawer is a bottom sheet, the head wraps', async () => {
    wide = false;
    const r = await usa();
    const nav = screen.getByRole('navigation', { name: 'Sections of this page' });
    expect(nav.className).toMatch(/overflow-x-auto/);
    expect(nav.className).toMatch(/sticky/);
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual(STRATEGY_SECTIONS.map((s) => s.label));
    // the matrix scrolls inside its card, never the page; the short stage names are there for phones
    const grid = within(sec('strategy')).getByRole('grid', { name: /^Country Strategy Map/ });
    expect(grid.closest('.overflow-x-auto')).toBeTruthy();
    expect(within(grid).getAllByRole('columnheader').slice(1).map((h) => h.querySelector('[aria-hidden="true"]')?.textContent)).toEqual(['Ext', 'Ref', 'Com', 'Asm', 'Dep', 'Rec']);
    // the wide tables (policy alignment, legacy mapping) scroll inside their own boxes
    expect(within(sec('policy-alignment')).getByTestId('policy-alignment').className).toMatch(/overflow-x-auto/);
    expect(within(sec('legacy-osi')).getByTestId('legacy-mapping').className).toMatch(/overflow-x-auto/);
    // the head's actions wrap, and so does their row with the country picker
    expect(screen.getByTestId('profile-actions').className).toMatch(/flex-wrap/);
    expect(screen.getByTestId('profile-actions').parentElement?.className).toMatch(/flex-wrap/);
    expect(within(screen.getByTestId('profile-actions').parentElement as HTMLElement).getByRole('combobox', { name: /Choose a country/ })).toBeTruthy();
    // the drawer opens as a bottom sheet (a side panel from the sm breakpoint)
    fireEvent.click(within(grid).getByRole('button', { name: /^Batteries Component Manufacturing/ }));
    await waitFor(() => expect(nodeOf(r)).toBe('Batteries|Components'));
    const d = await screen.findByTestId('cell-drawer', {}, { timeout: 30_000 });
    expect(d.className).toMatch(/fixed inset-x-0 bottom-0/);
    expect(d.className).toMatch(/sm:right-4/);
    expect(d.className).toMatch(/max-h-\[75vh\] overflow-y-auto/);
    fireEvent.keyDown(d, { key: 'Escape' });
    await waitFor(() => expect(nodeOf(r)).toBeNull());
  });
});
