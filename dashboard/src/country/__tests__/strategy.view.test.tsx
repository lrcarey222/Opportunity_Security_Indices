// The Country page's canonical framework on the real route (spec XXII UI; design 4, 5b, D1, D8, D10, D14; 2026-10-29; the
// fences of 2026-10-30): the R-written Country fixture with the posture fixture's Aland served as the United States. The
// Strategy Map renders all six stages, each cell its choice colour alone; the Deployment & Use column is set apart and holds
// only Accelerate and Watch; the legend groups the choices by fence. Then the three fence sections, each listing only its
// own nodes: High fence (Secure, the top five by supply-chain need, the need variables in a closed drop-down), Medium fence
// (Preserve, Catch Up and Partner by Strategic Industrial Value with ties by position, Accelerate by deployment need, the Prize x
// Position chart, and the Prize, Competitive Position, Partner (the partnership analysis as it read before the redesign) and
// Accelerate sub-sections with their variables), Low fence (Watch: the biggest prizes not chased and the deployments closest
// to Accelerate, with the engine's reasons, and the drop-downs); a fence without a node says so in one line. Policy Alignment
// renders; Allied Strategy lists only the compacts in which the economy holds a role (or says it holds none); the Legacy OSI
// framework stays out of the strategy sections (no dials, no engine); every entry point opens the same drawer through ?node=
// (and a deep link opens it); and the page stays usable at phone width (the chip nav, the matrix's and the tables' own
// scrollers, the bottom-sheet drawer).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { cssHex } from '../../components/testing/color.ts';
import type { Verb } from '../../posture/contract.ts';
import { VERB_COLOR } from '../../posture/grammar.ts';
import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../lib/stages.ts';
import { PostureStore, setPostureStore } from '../../posture/store.ts';
import { fixtureCore, fixtureDetail, fixtureStore, renameEconomies, withCell } from '../../posture/__tests__/fixture.ts';
import type { AibDoc, AibScenarioDoc } from '../../aib/contract.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';
import { fixtureSource as aibSource, haveFixture as haveAib, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { CountryStore, setCountryStoreForTests } from '../data/store.ts';
import { MEDIUM_SUBSECTIONS, STRATEGY_SECTIONS } from '../derive/strategyPage.ts';
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
  setAibStoreForTests(null);
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

/** The keys, choices, values and explaining fields of a top-five list, in order. */
const topKeys = (el: HTMLElement) => [...el.querySelectorAll('li[data-key]')].map((li) => li.getAttribute('data-key'));
const topVerbs = (el: HTMLElement) => [...el.querySelectorAll('li[data-key]')].map((li) => li.getAttribute('data-verb'));
const topValues = (el: HTMLElement) => [...el.querySelectorAll('[data-testid="fence-value"]')].map((x) => x.textContent);
const topWhy = (el: HTMLElement) => [...el.querySelectorAll('[data-testid="fence-why"]')].map((x) => x.textContent);
/** A table's column headers and its body rows as cell texts. */
const heads = (el: HTMLElement) => [...el.querySelectorAll('thead th')].map((th) => th.textContent);
const rows = (el: HTMLElement) => [...el.querySelectorAll('tbody tr')].map((tr) => [...tr.children].map((td) => td.textContent));
/** One body row of a table by its first cell (the node). */
const rowOf = (el: HTMLElement, node: string) => [...el.querySelectorAll('tbody tr')].find((tr) => tr.children[0]?.textContent === node) as HTMLElement;
/** Open a closed drop-down of the page (a Fold) by its id. */
async function openFold(id: string): Promise<HTMLElement> {
  const d = document.getElementById(`fold-${id}`) as HTMLElement;
  expect(d.hasAttribute('open'), id).toBe(false);
  fireEvent.click(d.querySelector('summary') as HTMLElement);
  await waitFor(() => expect(document.getElementById(`fold-${id}`)?.hasAttribute('open')).toBe(true));
  return document.getElementById(`fold-${id}`) as HTMLElement;
}
/** The AIB synthetic fixture with its economy AAA renamed USA (a fact-sheet economy), in place. */
function renamed<D extends AibDoc | AibScenarioDoc>(d: D): void {
  Object.assign(d, JSON.parse(JSON.stringify(d).replaceAll('"AAA"', '"USA"')) as D);
}

/** The drawer's node heading once it is open. */
async function drawerTitle(): Promise<string> {
  const d = await screen.findByTestId('cell-drawer', {}, { timeout: 30_000 });
  return within(d).getByRole('heading', { level: 3 }).textContent ?? '';
}

describe.skipIf(!haveFixture())('Country page: the canonical framework (spec XXII UI)', () => {
  it('the Strategy Map renders all six stages in choice colours alone, sets the Deployment & Use column apart (Accelerate and Watch) and groups the legend by fence', async () => {
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
    // colour alone: Accelerate and Watch at Deployment, Watch at a producing stage too, Preserve, Catch Up and Partner; no glyph in any cell
    const cell = (name: RegExp) => within(grid).getByRole('button', { name }) as HTMLElement;
    const fill = (b: HTMLElement) => [b.getAttribute('data-verb'), cssHex(b.style.background), b.textContent];
    expect(fill(cell(/^Batteries Deployment & Use: Accelerate · Medium fence/))).toEqual(['accelerate', '#a3b63b', '']);
    expect(cell(/^Batteries Deployment & Use/).getAttribute('data-mode')).toBe('accelerate');
    expect(fill(cell(/^Solar Deployment & Use: Watch · Low fence/))).toEqual(['watch', '#4b5563', '']);
    expect(fill(cell(/^Solar Extraction: Watch · Low fence/))).toEqual(['watch', '#4b5563', '']);
    expect(fill(cell(/^Batteries Final Assembly & Equipment: Preserve · Medium fence/))).toEqual(['preserve', '#1889b0', '']);
    expect(fill(cell(/^Batteries Component Manufacturing: Catch Up · Medium fence/))).toEqual(['catch_up', '#b4e7fe', '']);
    expect(fill(cell(/^Batteries Extraction: Partner · Medium fence/))).toEqual(['partner', '#55c4c5', '']);
    expect(fill(cell(/^Batteries Refining & Processing: Secure · High fence/))).toEqual(['secure', '#f58229', '']);
    const buttons = [...grid.querySelectorAll('button[data-verb]')] as HTMLElement[];
    expect(buttons.every((b) => b.textContent === '' && b.children.length === 0)).toBe(true);
    for (const b of buttons.filter((x) => x.getAttribute('data-verb') !== 'not_assessed')) expect(cssHex(b.style.background)).toBe(VERB_COLOR[b.getAttribute('data-verb') as Verb]);
    // the Deployment & Use column: Accelerate or Watch only, never Secure
    const depVerbs = buttons.filter((b) => b.closest('[data-deployment="true"]')).map((b) => b.getAttribute('data-verb'));
    expect(new Set(depVerbs)).toEqual(new Set(['accelerate', 'watch']));
    // the hatched not-assessed node, never a colour
    expect(cell(/^Batteries Recovery & Recycling: Not assessed/).getAttribute('data-verb')).toBe('not_assessed');
    expect(cell(/^Batteries Recovery & Recycling: Not assessed/).style.background).toContain('repeating-linear-gradient');
    // the legend above the map: the choices by fence, each with the node count
    const legend = within(strategy).getByTestId('posture-legend');
    expect(legend.textContent).toBe('High fence:Secure 3Medium fence:Preserve 2,Catch Up 1,Partner 1,Accelerate 1Low fence:Watch 3Not assessed 1');
    expect(legend.compareDocumentPosition(grid) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
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

  it('the three fence sections follow the map, each with the fence name and its one-line meaning; the old anchors land on them', async () => {
    await usa();
    const ids = STRATEGY_SECTIONS.map((x) => x.id);
    expect(ids.slice(0, 4)).toEqual(['strategy', 'high-fence', 'medium-fence', 'low-fence']);
    expect(['high-fence', 'medium-fence', 'low-fence'].map((id) => sec(id).querySelector('h2')?.textContent)).toEqual([
      'High fence · Secure', 'Medium fence · Preserve, Catch Up, Partner, Accelerate', 'Low fence · Watch',
    ]);
    expect(sec('high-fence').textContent).toContain('An acute vulnerability, secured whatever the prize.');
    expect(sec('medium-fence').textContent).toContain('Compete for the capability, at home or with partners, or accelerate deployment.');
    expect(sec('low-fence').textContent).toContain('Watch: domestic priority is low.');
    // each fence's legend: only its own choices, with the economy's counts
    expect(within(sec('high-fence')).getByTestId('posture-legend').textContent).toBe('High fence:Secure 3');
    expect(within(sec('medium-fence')).getAllByTestId('posture-legend')[0].textContent).toBe('Medium fence:Preserve 2,Catch Up 1,Partner 1,Accelerate 1');
    expect(within(sec('low-fence')).getByTestId('posture-legend').textContent).toBe('Low fence:Watch 3');
    // the old anchors: #need the High fence, #prize-position and #choices the Medium fence
    expect(document.getElementById('need')?.closest('section')?.id).toBe('high-fence');
    expect(document.getElementById('prize-position')?.closest('section')?.id).toBe('medium-fence');
    expect(document.getElementById('choices')?.closest('section')?.id).toBe('medium-fence');
    // the Need, Prize & Position and Choices sections are gone
    expect(screen.queryByTestId('need-panels')).toBeNull();
    expect(screen.queryByTestId('strategic-choices')).toBeNull();
  });

  it('High fence: only the Secure nodes, ranked by supply-chain need with their rationale; the drop-down lists every Secure node with the need variables', async () => {
    await usa();
    const high = sec('high-fence');
    const top = within(high).getByTestId('high-top');
    expect(within(top).getByRole('heading', { level: 3 }).textContent).toBe('Top 5 supply-chain nodes by need');
    expect(topKeys(top)).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components']);
    expect(new Set(topVerbs(top))).toEqual(new Set(['secure']));
    expect(topValues(top)).toEqual(['95', '90', '70']);
    expect(topWhy(top)).toEqual(['Secure · Coercive chokepoint', 'Secure · Systemic resilience', 'Secure · Operational control']);
    // the choice colour of each entry
    expect([...top.querySelectorAll('li [data-swatch]')].map((x) => cssHex((x as HTMLElement).style.background))).toEqual([VERB_COLOR.secure, VERB_COLOR.secure, VERB_COLOR.secure]);
    // nothing of another fence
    expect(high.querySelectorAll('[data-verb]:not([data-verb="secure"])')).toHaveLength(0);
    // the drop-down: closed, then every Secure node x the need variables, missing values "not measured"
    expect(high.querySelector('[data-testid="high-table"]')).toBeNull();
    const fold = await openFold('high-all');
    expect(fold.querySelector('summary')?.textContent).toMatch(/All 3 Secure nodes × need variables: import dependence, supplier concentration, risky supplier exposure, lack of trusted alternatives/);
    const table = within(fold).getByTestId('high-table');
    expect(heads(table)).toEqual(['Node', 'Supply-chain need', 'Import dependence', 'Supplier concentration', 'Risky supplier exposure', 'Lack of trusted alternatives', 'Security rationale']);
    await waitFor(() => expect(rowOf(table, 'Batteries · Refining & Processing').textContent).toContain('92'), { timeout: 30_000 });
    expect(rows(table)).toEqual([
      ['Batteries · Refining & Processing', '95', '95', '90', '92', 'not measured', 'Coercive chokepoint'],
      ['Solar · Refining & Processing', '90', '90', '85', '95', 'not measured', 'Systemic resilience'],
      ['Solar · Component Manufacturing', '70', '70', '60', '80', 'not measured', 'Operational control'],
    ]);
    // numbers right-aligned
    expect([...table.querySelectorAll('tbody tr:first-child td')].map((td) => td.className)).toEqual(['', 'num', 'num', 'num', 'num', 'num', '']);
  });

  it('Medium fence: Preserve, Catch Up and Partner by Strategic Industrial Value (ties by position), Accelerate by deployment need, then the Prize × Position chart', async () => {
    await usa();
    const med = sec('medium-fence');
    const supply = within(med).getByTestId('medium-top-supply');
    expect(within(supply).getByRole('heading', { level: 3 }).textContent).toBe('Top 5 supply-chain nodes');
    // Strategic Industrial Value 90, 80, then two at 30: Solar Assembly (position 90) before Batteries Extraction (position 20)
    expect(topKeys(supply)).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
    expect(topValues(supply)).toEqual(['90', '80', '30', '30']);
    expect(new Set(topVerbs(supply))).toEqual(new Set(['preserve', 'catch_up', 'partner']));
    await waitFor(() => expect(topWhy(supply)[1]).toBe('Catch Up · Pathway: Credible (feasibility, pipeline)'), { timeout: 30_000 });
    expect(topWhy(supply)).toEqual(['Preserve · Competitive Position 95', 'Catch Up · Pathway: Credible (feasibility, pipeline)', 'Preserve · Competitive Position 90', 'Partner · Leading partner: Bland']);
    expect([...supply.querySelectorAll('li')].map((li) => cssHex((li.querySelector('[data-swatch]') as HTMLElement).style.background))).toEqual(
      (['preserve', 'catch_up', 'preserve', 'partner'] as Verb[]).map((v) => VERB_COLOR[v]),
    );
    const deploy = within(med).getByTestId('medium-top-deploy');
    expect(within(deploy).getByRole('heading', { level: 3 }).textContent).toBe('Top 5 deployment (Accelerate)');
    expect(topKeys(deploy)).toEqual(['Batteries|Deployment']);
    expect(topVerbs(deploy)).toEqual(['accelerate']);
    expect(topValues(deploy)).toEqual(['85']);
    // the top deployment family, with its scenario and basis
    await waitFor(() => expect(topWhy(deploy)).toEqual(['Accelerate · Top need: Demand adequacy 90 · Current policy · national data']), { timeout: 30_000 });
    // nothing of the other fences in the top lists
    expect(med.querySelectorAll('[data-testid^="medium-top"] li[data-verb="secure"], [data-testid^="medium-top"] li[data-verb="watch"]')).toHaveLength(0);
    // the Prize x Position chart comes next, on the quadrant-scatter look
    const scatter = within(med).getByTestId('prize-position-scatter');
    expect(deploy.compareDocumentPosition(scatter) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    const points = [...scatter.querySelectorAll('[data-verb]')];
    expect(new Set(points.map((pt) => pt.getAttribute('data-verb')))).toEqual(new Set(['secure', 'preserve', 'catch_up', 'partner', 'watch']));
    expect(points.every((pt) => !/Deployment & Use/.test(pt.getAttribute('aria-label') ?? ''))).toBe(true);
    expect(within(med).getByRole('region', { name: 'Where is the prize, and where is the country already strong?' })).toBeTruthy();
    // the economy's name mid-sentence: lower-case "the", and the possessive of a name in -s
    expect(med.textContent).toContain("Each dot is one of the United States' producing-stage nodes");
    expect(med.textContent).not.toMatch(/The United States's|one of The United States/);
    const dots = [...scatter.querySelectorAll('.quad-dots path')];
    expect(dots).toHaveLength(points.length);
    expect(new Set(dots.map((d) => d.getAttribute('fill')))).toEqual(new Set(points.map((pt) => VERB_COLOR[pt.getAttribute('data-verb') as Verb])));
    expect(scatter.querySelectorAll('.quad-rule-x line, .quad-rule-y line')).toHaveLength(2);
    const preserve = points.find((pt) => /^Batteries Final Assembly & Equipment/.test(pt.getAttribute('aria-label') ?? ''))!;
    expect(preserve.getAttribute('aria-label')).toMatch(/Strategic Industrial Value 90%; Competitive Position 95%; supply-chain need 40%; trade momentum/);
  });

  it('Medium fence sub-sections: Prize, Competitive Position, Partner (the partnership analysis) and Accelerate, each with its variables', async () => {
    await usa();
    const med = sec('medium-fence');
    // four level-3 sub-sections in order, each with its anchor, after the chart
    const subs = [...med.querySelectorAll('section[id^="medium-"]')].filter((x) => MEDIUM_SUBSECTIONS.some((m) => m.id === x.id));
    expect(subs.map((x) => x.id)).toEqual(['medium-prize', 'medium-position', 'medium-partner', 'medium-accelerate']);
    expect(subs.map((x) => x.querySelector('h3')?.textContent)).toEqual(['Prize', 'Competitive Position', 'Partner', 'Accelerate']);
    expect(within(med).getByTestId('prize-position-scatter').compareDocumentPosition(subs[0]) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();

    // Prize: what the prize measures, then the nodes x the prize variables (four rows, within the top five: no drop-down)
    const prize = sec('medium-prize');
    expect(prize.textContent).toMatch(/Strategic Industrial Value\s*asks whether domestic capability in the node is strategically valuable; high at 60 or more\./);
    expect(prize.textContent).toMatch(
      /three families: the market prize \(Industrial Opportunity\s*: expected world demand growth, the node prize, and investment momentum, lifted by home-market growth only where national data exist\), industrial commons \(process knowledge, workforce and suppliers, cross-sector spillover\) and future option value/,
    );
    expect(prize.textContent).toMatch(/Where only the market speaks for it, the value is capped below the bar\./);
    const pt = within(prize).getByTestId('medium-prize-table');
    // the Catch Up node names its credible pathway under its choice (no column of dashes for the others); the headers wrap to fit the card
    // the Strategic Industrial Value and its three families (the market prize; industrial commons and future option from the
    // node evidence, a market-only node capped), then the market prize's components
    expect(heads(pt)).toEqual([
      'Node', 'Choice', 'Strategic Industrial Value', 'Market prize', 'Industrial commons', 'Future option', 'Node prize', 'Home-market growth', 'Investment momentum', 'Market outlook',
      'Basis', 'Supply-chain need',
    ]);
    expect((pt.querySelector('thead th') as HTMLElement).style.whiteSpace).toBe('normal');
    await waitFor(() => expect(rowOf(pt, 'Batteries · Component Manufacturing').textContent).toContain('feasibility'), { timeout: 30_000 });
    expect(rows(pt)).toEqual([
      ['Batteries · Final Assembly & Equipment', 'Preserve', '90', '85', '100', '50', '90', 'not measured', 'not measured', 'not measured', 'global context', '40'],
      ['Batteries · Component Manufacturing', 'Catch UpCredible (feasibility, pipeline)', '80', '70', '50', '0', '85', '75', '70', 'not measured', 'national data', '50'],
      ['Solar · Final Assembly & Equipment', 'Preserve', '30', '30', '50', '0', '30', 'not measured', 'not measured', 'oversupplied', 'not measured', '80'],
      ['Batteries · Extraction', 'Partner', '30capped: market only', '30', '33', '0', '35', 'not measured', 'not measured', 'not measured', 'global context', '80'],
    ]);
    expect(within(pt).getAllByTestId('fence-pathway')).toHaveLength(1);
    expect(prize.querySelector('[id^="fold-medium-prize"]')).toBeNull();

    // Competitive Position: the Preserve bar named, the nodes x the position evidence in Competitive Position order, the monthly trade card
    const pos = sec('medium-position');
    expect(pos.textContent).toMatch(/The Preserve bar is 75: the top quarter of the node's contenders/);
    expect(pos.textContent).toMatch(/listed by Competitive Position, strongest first/);
    const qt = within(pos).getByTestId('medium-position-table');
    expect(heads(qt)).toEqual(['Node', 'Choice', 'Competitive Position', 'World supply share', 'Export position', 'Structural trade share', 'Current investment', 'Trade momentum', 'Evidence']);
    await waitFor(() => expect(rowOf(qt, 'Batteries · Component Manufacturing').textContent).toContain('60'), { timeout: 30_000 });
    // the structural share where R wrote one (Solar Assembly: momentum moved it); where R dropped an identical
    // one, the export position's value, said to be the same (never "not measured" beside a measured export position)
    expect(rows(qt)).toEqual([
      ['Batteries · Final Assembly & Equipment', 'Preserve', '95', '12.0%', '90', '90same as export', 'not measured', 'rising', 'Weak'],
      ['Solar · Final Assembly & Equipment', 'Preserve', '90', '8.0%', '85', '70', 'not measured', 'not measured', 'Strong'],
      ['Batteries · Component Manufacturing', 'Catch Up', '40', '3.0%', '35', '35same as export', '60', 'flat', 'Strong'],
      ['Batteries · Extraction', 'Partner', '20', '0.4%', '20', '20same as export', 'not measured', 'rising', 'Strong'],
    ]);
    const cps = rows(qt).map((r) => Number(r[2]));
    expect(cps).toEqual([...cps].sort((a, b) => b - a));
    expect(within(pos).queryByTestId('trade-monthly') ?? within(pos).queryByText(/Loading monthly trade/)).toBeTruthy();

    // Partner: a lead naming the Partner nodes above the partnership analysis, which reads as before the redesign (open, not in a drop-down)
    const partner = sec('medium-partner');
    for (const id of ['partnerships', 'partners', 'allied-partnerships']) expect(document.getElementById(id)?.closest('section')?.id, id).toBe('medium-partner');
    const lead = within(partner).getByTestId('partner-lead');
    expect(lead.textContent).toMatch(/^The United States' Partner node: Batteries · Extraction \(leading partner: Bland\)\./);
    const intro = within(partner).getByText(/^Who the United States could trade, invest, build and align with, read three ways/);
    expect(lead.compareDocumentPosition(intro) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(within(partner).getByTestId('ptab-trade')).toBeTruthy();
    expect(within(partner).getByText(/^Every partner of the United States: priority partners beside today's partners/)).toBeTruthy();
    expect(screen.queryByText('Explore all partnership analysis')).toBeNull();

    // Accelerate: the Accelerate nodes x the deployment-need families, each with its scenario; the energy-system context
    const acc = sec('medium-accelerate');
    const at = within(acc).getByTestId('medium-accelerate-table');
    // one choice in the table (Accelerate): no Choice column; the caption names it
    expect(heads(at)).toEqual(['Technology', 'Deployment need', 'Demand adequacy', 'Target / climate gap', 'System need', 'Climate-aligned (alternative)', 'Deployment trend']);
    expect(at.querySelector('caption')?.textContent).toMatch(/the deployment need of the Accelerate nodes$/);
    await waitFor(
      () =>
        expect(rows(at)[0]).toEqual([
          'Batteries', '85', '90Current policy · national data', '80National commitments · national data', 'not measured',
          '+104% vs current policiesClimate-aligned · World 2035, Net Zero vs Current Policies · global context', 'rising',
        ]),
      { timeout: 30_000 },
    );
    expect(rows(at)).toHaveLength(1);
    expect(within(acc).getByTestId('deployment-scenarios').textContent).toMatch(/Current policy.*National commitments/);
    expect(within(acc).getByText(/^Energy-system context:/)).toBeTruthy();
    // each sub-section's table lists only the Medium fence's nodes
    for (const t of [pt, qt]) expect(new Set([...t.querySelectorAll('[data-verb]')].map((x) => x.getAttribute('data-verb')))).toEqual(new Set(['preserve', 'catch_up', 'partner']));
    // the Accelerate table: the Accelerate node only (Batteries), never the Deployment & Use Watch node (Solar)
    expect(rows(at).map((r) => r[0])).toEqual(['Batteries']);
  });

  it('Low fence: producing-stage Watch by Strategic Industrial Value, Deployment & Use Watch by deployment need, each with the engine reasons; the drop-downs list every Watch node with its variables', async () => {
    await usa();
    const low = sec('low-fence');
    const supply = within(low).getByTestId('low-top-supply');
    expect(topKeys(supply)).toEqual(['Solar|Extraction', 'Solar|Recovery']);
    expect(topValues(supply)).toEqual(['20', '15']);
    await waitFor(() => expect(topWhy(supply)).toEqual(['Watch · Low need; Low prize; Ample diversified supply', 'Watch · Low need; Low prize; Emerging technology, research and option value']), {
      timeout: 30_000,
    });
    const deploy = within(low).getByTestId('low-top-deploy');
    expect(topKeys(deploy)).toEqual(['Solar|Deployment']);
    expect(topValues(deploy)).toEqual(['40']);
    expect(topWhy(deploy)).toEqual(['Watch · Deployment need below the acceleration bar']);
    expect(new Set([...low.querySelectorAll('li[data-verb]')].map((li) => li.getAttribute('data-verb')))).toEqual(new Set(['watch']));
    // the drop-downs, closed: the producing-stage Watch nodes x prize, position and need; the deployment Watch nodes x the deployment variables
    const sf = await openFold('low-supply-all');
    const lp = within(sf).getByTestId('low-prize-table');
    // one choice in the Low fence's tables (Watch): no Choice column
    expect(heads(lp)).toEqual([
      'Node', 'Strategic Industrial Value', 'Market prize', 'Industrial commons', 'Future option', 'Node prize', 'Home-market growth', 'Investment momentum', 'Market outlook', 'Basis',
      'Supply-chain need', 'Why Watch',
    ]);
    // Solar Extraction is market only (capped); Solar Recovery has no node evidence: commons and option not measured, never zero
    expect(rows(lp).map((r) => [r[0], r[1], r[3], r[4], r[10], r[11]])).toEqual([
      ['Solar · Extraction', '20capped: market only', '17', '0', '20', 'Low need; Low prize; Ample diversified supply'],
      ['Solar · Recovery & Recycling', '15', 'not measured', 'not measured', '10', 'Low need; Low prize; Emerging technology, research and option value'],
    ]);
    const lq = within(sf).getByTestId('low-position-table');
    expect(heads(lq)).toEqual(['Node', 'Competitive Position', 'World supply share', 'Export position', 'Structural trade share', 'Current investment', 'Trade momentum', 'Evidence']);
    expect(rows(lq).map((r) => r.slice(0, 2))).toEqual([['Solar · Extraction', '30'], ['Solar · Recovery & Recycling', '5']]);
    const df = await openFold('low-deploy-all');
    const ld = within(df).getByTestId('low-deploy-table');
    expect(heads(ld)).toEqual(['Technology', 'Deployment need', 'Demand adequacy', 'Target / climate gap', 'System need', 'Climate-aligned (alternative)', 'Deployment trend', 'Why Watch']);
    expect(rows(ld)).toEqual([
      ['Solar', '40', '40Current policy · observed trend', '30National commitments · national data', '50national data', 'not measured', '+8% a year', 'Deployment need below the acceleration bar'],
    ]);
  });

  it('a fence without a node says so in one line (no Secure node; no Accelerate node)', async () => {
    // Aland with its three Secure nodes and its Accelerate node turned Watch
    let core = fixtureCore();
    for (const [t, s] of [[0, 1], [1, 1], [1, 2], [0, 4]] as const) core = withCell(core, 0, t, s, { p: 'D', v: 'deploy_watch', m: null, dm: 'watch', sec: null });
    setPostureStore(fixtureStore({ core, economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
    await usa();
    const high = sec('high-fence');
    expect(within(high).getByTestId('fence-empty').textContent).toBe('No Secure nodes: no acute vulnerability passed the Secure tests.');
    expect(document.getElementById('fold-high-all')).toBeNull();
    expect(within(sec('medium-fence')).getByTestId('medium-top-deploy').textContent).toMatch(/No Accelerate nodes: no technology's deployment need reached the acceleration bar \(67\)\./);
    expect(sec('medium-accelerate').textContent).toMatch(/No Accelerate nodes/);
    expect(within(sec('low-fence')).getByTestId('posture-legend').textContent).toBe('Low fence:Watch 7');
  });

  it('Policy Alignment renders: node, recommendation, observed intensity, support, protection, momentum and alignment; the observed-policy evidence below', async () => {
    await usa();
    const pol = sec('policy-alignment');
    const table = within(within(pol).getByTestId('policy-alignment')).getByRole('table');
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent)).toEqual(['Node', 'Recommendation', 'Observed policy intensity', 'Support', 'Protection', 'Policy momentum', 'Alignment']);
    await waitFor(() => expect(table.querySelector('[data-key="Batteries|Extraction"]')?.textContent).toMatch(/Partner · Medium fence.*misaligned/i), { timeout: 30_000 });
    expect(table.querySelector('[data-key="Batteries|Deployment"]')?.getAttribute('data-alignment')).toBe('deployment_gap');
    expect(pol.textContent).toContain("percentile ranks among the United States' own nodes");
    expect(pol.textContent).not.toMatch(/among The United States/);
    // no overall score; the evidence beneath carries no Strategic Index comparison (a Legacy OSI reading)
    const ev = within(pol).getByTestId('policy-behaviour');
    expect(ev.textContent).toMatch(/Climate Action Tracker/);
    expect(ev.textContent).not.toMatch(/Strategic Index/);
  });

  it.skipIf(!haveAib(SYNTHETIC_DIR))('Allied Strategy lists only the compacts in which the economy holds a role, each linking to its page; the scenario toggle carries', async () => {
    setAibStoreForTests(new AibStore(aibSource(SYNTHETIC_DIR, renamed, { editScenario: (d) => renamed(d) })));
    const r = await usa('/countries/USA?scn=decoupling');
    const allied = sec('allied');
    // the strategic compacts with its role in each, the most relevant first, each linking to its page with the toggle
    const strat = await within(allied).findByTestId('allied-strategic-compacts', {}, { timeout: 30_000 });
    expect(within(strat).getByText(/has a role in 5 strategic compacts of 5, as a core country in 5\./)).toBeTruthy();
    const items = within(within(strat).getByRole('list', { name: 'Roles in the strategic compacts' })).getAllByRole('listitem');
    expect(items.map((li) => li.getAttribute('data-compact'))).toEqual(['SC-WIND', 'SC-MAGNETS', 'SC-BATTERIES', 'SC-GRID', 'SC-SOLAR']);
    expect(within(items[1]).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling');
    expect(items[1].textContent).toContain('Demand anchor in magnets (36% of world imports)');
    // the Sectoral Compacts whose coalition it sits in, with the roles the model names there, under the scenario
    const ops = within(allied).getByTestId('allied-operational-compacts');
    expect(within(ops).getByTestId('allied-scenario').textContent).toMatch(/^Shown under Decoupling/);
    const opItems = within(within(ops).getByRole('list', { name: "The United States' roles in the Sectoral Compacts" })).getAllByRole('listitem');
    expect(opItems.map((li) => li.getAttribute('data-compact'))).toEqual(['AIB-C01', 'AIB-C02', 'AIB-C03']);
    expect(within(opItems[0]).getByRole('link', { name: 'AIB-C01' }).getAttribute('href')).toBe('/compacts/AIB-C01?scn=decoupling');
    expect(opItems.every((li) => /Demand anchor/.test(li.textContent ?? ''))).toBe(true);
    // nothing else: no per-node partner table, no role summaries, no network relationships, no partnership analysis
    for (const id of ['allied-strategy', 'allied-sentence', 'aib-strategic-roles', 'aib-bilateral', 'aib-roles', 'aib-relationships', 'allied-explore']) expect(within(allied).queryByTestId(id), id).toBeNull();
    expect(within(allied).queryByRole('heading', { name: 'Partnerships' })).toBeNull();
    expect(within(allied).getByRole('link', { name: /See all country roles/ }).getAttribute('href')).toBe('/aib/roles?economy=USA&scn=decoupling#role-USA');
    // the old anchors land here
    for (const id of ['aib-role', 'allied-aib-role']) expect(document.getElementById(id)?.closest('section')?.id, id).toBe('allied');
    // the toggle: Managed Interdependence, its own compacts' coalitions, the links without a scenario
    fireEvent.click(within(ops).getByRole('button', { name: /^Show Managed Interdependence instead/ }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('scn')).not.toBe('decoupling'));
    await waitFor(() => expect(within(sec('allied')).getByTestId('allied-scenario').textContent).toMatch(/^Shown under Managed Interdependence/), { timeout: 30_000 });
    await waitFor(() => expect(within(sec('allied')).getAllByRole('link').map((a) => a.getAttribute('href'))).toContain('/compacts/s/SC-WIND'), { timeout: 30_000 });
    expect(bareAcronyms(sec('allied'))).toEqual([]);
  }, 90_000);

  it.skipIf(!haveAib(SYNTHETIC_DIR))("Allied Strategy: an economy without a role in any compact says so in one line, with a link to every economy's roles", async () => {
    setAibStoreForTests(new AibStore(aibSource(SYNTHETIC_DIR)));
    await usa();
    const none = await within(sec('allied')).findByTestId('allied-no-role', {}, { timeout: 30_000 });
    expect(none.textContent).toMatch(/^The United States holds no role in a Strategic Sectoral Compact or in a Sectoral Compact's coalition under Managed Interdependence\./);
    expect(within(none).getByRole('link').getAttribute('href')).toBe('/aib/roles');
    expect(within(sec('allied')).queryByTestId('allied-strategic-compacts')).toBeNull();
    expect(within(sec('allied')).queryByTestId('allied-operational-compacts')).toBeNull();
  });

  it('the Legacy OSI framework stays out of the strategy sections (Strategy, the three fences, Policy Alignment, Allied Strategy): no dials, no corners, no Strategic Index, and the engine never starts', async () => {
    await usa();
    // the one exemption, by the user's design: the Medium fence's Partner sub-section carries the partnership
    // analysis as it read before the redesign, where the Legacy OSI partner scores are one of three lenses, named so
    const without = (el: HTMLElement, id: string) => {
      const c = el.cloneNode(true) as HTMLElement;
      c.querySelector(`#${id}`)?.remove();
      return c.textContent ?? '';
    };
    for (const s of STRATEGY_SECTIONS.slice(0, 6)) {
      const t = s.id === 'medium-fence' ? without(sec(s.id), 'medium-partner') : (sec(s.id).textContent ?? '');
      expect(t, s.id).not.toMatch(
        /Energy Security|Economic Opportunity|Strategic Index|Perspective: |Perspective dials|\bdials?\b|four corners|\bGrow\b|\bExposed\b|NEIS baseline|Legacy OSI|Friendshore|Partnership Strength/,
      );
    }
    expect(sec('medium-partner').textContent).toMatch(/the Legacy OSI partner scores/);
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

  it('the long tables follow the top-five pattern: the top five shown, every node in a closed drop-down; the position table runs by Competitive Position', async () => {
    // Aland with its two producing-stage Watch nodes turned Preserve and Partner: six Medium-fence nodes
    let core = fixtureCore();
    core = withCell(core, 0, 1, 0, { p: 'C', v: 'compete_preserve', m: 'preserve', dm: null, prize: 0.5, cp: 0.85 });
    core = withCell(core, 0, 1, 5, { p: 'C', v: 'compete_partner', m: 'partner', dm: null, prize: 0.2, cp: 0.3 });
    setPostureStore(fixtureStore({ core, economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
    await usa();
    const prize = sec('medium-prize');
    const first = within(prize).getByTestId('medium-prize-table-first');
    expect(first.querySelector('caption')?.textContent).toMatch(/: the top 5$/);
    // by Strategic Industrial Value: 90, 80, 50, 30 (position 90), 30 (position 20); the sixth (20) only in the drop-down
    expect(rows(first).map((r) => r[0])).toEqual([
      'Batteries · Final Assembly & Equipment', 'Batteries · Component Manufacturing', 'Solar · Extraction', 'Solar · Final Assembly & Equipment', 'Batteries · Extraction',
    ]);
    expect(within(prize).queryByTestId('medium-prize-table')).toBeNull();
    const all = await openFold('medium-prize-all');
    expect(all.querySelector('summary')?.textContent).toMatch(/All 6 Preserve, Catch Up and Partner nodes × the prize variables$/);
    expect(rows(within(all).getByTestId('medium-prize-table')).map((r) => r[0])).toEqual([
      'Batteries · Final Assembly & Equipment', 'Batteries · Component Manufacturing', 'Solar · Extraction', 'Solar · Final Assembly & Equipment', 'Batteries · Extraction',
      'Solar · Recovery & Recycling',
    ]);
    // Competitive Position: its own order, 95, 90, 85, 40, 30 shown; 20 in the drop-down
    const pos = sec('medium-position');
    const pfirst = within(pos).getByTestId('medium-position-table-first');
    expect(rows(pfirst).map((r) => [r[0], r[2]])).toEqual([
      ['Batteries · Final Assembly & Equipment', '95'], ['Solar · Final Assembly & Equipment', '90'], ['Solar · Extraction', '85'], ['Batteries · Component Manufacturing', '40'],
      ['Solar · Recovery & Recycling', '30'],
    ]);
    const pall = await openFold('medium-position-all');
    expect(rows(within(pall).getByTestId('medium-position-table')).map((r) => r[2])).toEqual(['95', '90', '85', '40', '30', '20']);
    // a top-five row opens the drawer like every other entry point
    fireEvent.click(rowOf(pfirst, 'Solar · Extraction'));
    expect(await drawerTitle()).toBe('Solar · Extraction');
  });

  it('a detail document that cannot be loaded: its values say so, with Retry in each fence, never "not measured"; the Watch reasons wait for the engine', async () => {
    const usaOnly = { AAA: { iso3: 'USA', name: 'United States' } } as const;
    let fail = true;
    const files: Record<string, unknown> = {
      'manifest.json': { schema: 'osi-posture', schema_version: 2, created: 'x', core: 'posture-core.1.json', details: { USA: 'posture-detail-USA.1.json' }, counts: {} },
      'posture-core.1.json': renameEconomies(fixtureCore(), usaOnly),
      'posture-detail-USA.1.json': renameEconomies(fixtureDetail(), usaOnly),
    };
    setPostureStore(
      new PostureStore('/x/', async (url: string) => {
        const f = url.replace('/x/', '');
        if (fail && f.startsWith('posture-detail')) return new Response('', { status: 500 });
        return f in files ? new Response(JSON.stringify(files[f]), { status: 200 }) : new Response('', { status: 404 });
      }),
    );
    await usa();
    const note = await within(sec('high-fence')).findByTestId('fence-detail-error', {}, { timeout: 30_000 });
    expect(note.textContent).toMatch(/The node details could not be loaded/);
    expect(note.textContent).toMatch(/could not be fetched \(HTTP 500\)/);
    for (const id of ['high-fence', 'medium-fence', 'low-fence']) expect(within(within(sec(id)).getByTestId('fence-detail-error')).getByRole('button', { name: 'Retry' }), id).toBeTruthy();
    // the Watch reasons are the engine's (the detail's): not recomputed from the thresholds while it is missing
    expect(topWhy(within(sec('low-fence')).getByTestId('low-top-supply'))).toEqual(['Watch · could not be loaded', 'Watch · could not be loaded']);
    expect(topWhy(within(sec('medium-fence')).getByTestId('medium-top-deploy'))).toEqual(['Accelerate · could not be loaded']);
    // the core's own values stay; every value of the detail says it could not be loaded
    const high = await openFold('high-all');
    expect(rows(within(high).getByTestId('high-table'))[0]).toEqual([
      'Batteries · Refining & Processing', '95', 'could not be loaded', 'could not be loaded', 'could not be loaded', 'could not be loaded', 'Coercive chokepoint',
    ]);
    for (const id of ['medium-prize-table', 'medium-position-table', 'medium-accelerate-table']) {
      const t = screen.getByTestId(id);
      expect(t.textContent, id).toContain('could not be loaded');
      expect(t.textContent, id).not.toContain('not measured');
    }
    // Retry loads it: the values and the engine's reasons arrive, the notices go
    fail = false;
    fireEvent.click(within(within(sec('medium-fence')).getByTestId('fence-detail-error')).getByRole('button', { name: 'Retry' }));
    await waitFor(() => expect(screen.queryByTestId('fence-detail-error')).toBeNull(), { timeout: 30_000 });
    await waitFor(() => expect(topWhy(within(sec('low-fence')).getByTestId('low-top-supply'))[0]).toBe('Watch · Low need; Low prize; Ample diversified supply'), { timeout: 30_000 });
    expect(screen.getByTestId('medium-prize-table').textContent).not.toContain('could not be loaded');
  });

  it('cross-navigation: the map, every top-five list, every fence table, the scatter, the Partner lead and a policy row all open the same drawer through ?node=', async () => {
    const r = await usa();
    const open = async (click: () => void, key: string, title: string) => {
      click();
      await waitFor(() => expect(nodeOf(r)).toBe(key), { timeout: 30_000 });
      expect(await drawerTitle()).toBe(title);
      expect(screen.getAllByTestId('cell-drawer')).toHaveLength(1);
      // the five questions, in order
      expect(within(screen.getByTestId('cell-drawer')).getAllByTestId('drawer-question').map((q) => q.getAttribute('data-question'))).toEqual(['need', 'prize', 'position', 'choice', 'policy']);
    };
    const entry = (testId: string, key: string) => () => fireEvent.click(within(screen.getByTestId(testId)).getAllByRole('button').find((b) => b.closest('li')?.getAttribute('data-key') === key)!);
    const row = (testId: string, node: string) => () => fireEvent.click(rowOf(screen.getByTestId(testId), node));
    await open(() => fireEvent.click(within(sec('strategy')).getByRole('button', { name: /^Solar Extraction/ })), 'Solar|Extraction', 'Solar · Extraction');
    // the top-five entries of each fence
    await open(entry('high-top', 'Solar|Refining'), 'Solar|Refining', 'Solar · Refining & Processing');
    await open(entry('medium-top-supply', 'Batteries|Components'), 'Batteries|Components', 'Batteries · Component Manufacturing');
    await open(entry('medium-top-deploy', 'Batteries|Deployment'), 'Batteries|Deployment', 'Batteries · Deployment & Use');
    await open(entry('low-top-supply', 'Solar|Recovery'), 'Solar|Recovery', 'Solar · Recovery & Recycling');
    await open(entry('low-top-deploy', 'Solar|Deployment'), 'Solar|Deployment', 'Solar · Deployment & Use');
    // the scatter, the Medium fence's tables and the Partner lead
    await open(() => fireEvent.click(within(screen.getByTestId('prize-position-scatter')).getByRole('button', { name: /^Batteries Final Assembly & Equipment/ })), 'Batteries|Assembly', 'Batteries · Final Assembly & Equipment');
    await open(row('medium-prize-table', 'Solar · Final Assembly & Equipment'), 'Solar|Assembly', 'Solar · Final Assembly & Equipment');
    await open(row('medium-position-table', 'Batteries · Component Manufacturing'), 'Batteries|Components', 'Batteries · Component Manufacturing');
    await open(() => fireEvent.click(within(within(sec('medium-partner')).getByTestId('partner-lead')).getByRole('button', { name: 'Batteries · Extraction' })), 'Batteries|Extraction', 'Batteries · Extraction');
    await open(row('medium-accelerate-table', 'Batteries'), 'Batteries|Deployment', 'Batteries · Deployment & Use');
    // the drop-down tables of the High and Low fences, by click and by keyboard
    await openFold('high-all');
    await open(row('high-table', 'Solar · Component Manufacturing'), 'Solar|Components', 'Solar · Component Manufacturing');
    await openFold('low-supply-all');
    await open(() => fireEvent.keyDown(rowOf(screen.getByTestId('low-position-table'), 'Solar · Extraction'), { key: 'Enter' }), 'Solar|Extraction', 'Solar · Extraction');
    await openFold('low-deploy-all');
    await open(row('low-deploy-table', 'Solar'), 'Solar|Deployment', 'Solar · Deployment & Use');
    // a policy row
    const policyRow = within(sec('policy-alignment')).getByTestId('policy-alignment').querySelector('[data-key="Batteries|Refining"]') as HTMLElement;
    await open(() => fireEvent.click(within(policyRow).getByRole('button')), 'Batteries|Refining', 'Batteries · Refining & Processing');
    // the selected node is pressed on the map and in its fence's list, and marked in its table
    expect(within(sec('strategy')).getByRole('button', { name: /^Batteries Refining & Processing/ }).getAttribute('aria-pressed')).toBe('true');
    expect(within(screen.getByTestId('high-top')).getAllByRole('button')[0].getAttribute('aria-pressed')).toBe('true');
    expect(rowOf(screen.getByTestId('high-table'), 'Batteries · Refining & Processing').getAttribute('aria-selected')).toBe('true');
  }, 120_000);

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
    const card = await within(sec('medium-position')).findByTestId('trade-monthly', {}, { timeout: 30_000 });
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
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual(STRATEGY_SECTIONS.map((s) => s.nav ?? s.label));
    expect(within(nav).getAllByRole('link').map((a) => a.textContent)).toEqual([
      'Strategy', 'High fence · Secure', 'Medium fence · Preserve, Catch Up, Partner, Accelerate', 'Low fence · Watch', 'Policy Alignment', 'Allied Strategy', 'Evidence & Methodology',
    ]);
    // the matrix scrolls inside its card, never the page; the short stage names are there for phones
    expect(nav.className).toMatch(/\bbg-ink-900\b/);
    expect(nav.className).not.toMatch(/bg-ink-900\/\d+/);
    const grid = within(sec('strategy')).getByRole('grid', { name: /^Country Strategy Map/ });
    expect(grid.closest('.overflow-x-auto')).toBeTruthy();
    // the map fits a phone's card (about 300 px at a 375 px screen): the column minimums and the gaps, so the
    // Recovery & Recycling column is never cut off
    for (const row of [...grid.querySelectorAll('[role="row"]')] as HTMLElement[]) {
      const tracks = row.style.gridTemplateColumns.match(/minmax\([\d.]+rem|[\d.]+rem(?![^(]*\))/g) ?? [];
      const mins = tracks.map((t) => parseFloat(t.replace('minmax(', '')) * 16);
      expect(mins).toHaveLength(9);
      expect(mins.reduce((a, b) => a + b, 0) + 2 * (mins.length - 1)).toBeLessThanOrEqual(300);
    }
    expect(within(grid).getAllByRole('columnheader').slice(1).map((h) => h.querySelector('[aria-hidden="true"]')?.textContent)).toEqual(['Ext', 'Ref', 'Com', 'Asm', 'Dep', 'Rec']);
    // the wide tables (the fences' tables, policy alignment, legacy mapping) scroll inside their own boxes
    for (const id of ['medium-prize-table', 'medium-position-table', 'medium-accelerate-table']) expect(screen.getByTestId(id).querySelector('table')?.parentElement?.className, id).toMatch(/overflow-x-auto/);
    expect(within(sec('policy-alignment')).getByTestId('policy-alignment').className).toMatch(/overflow-x-auto/);
    // the fences' top lists stack: two columns only from the lg breakpoint
    expect(screen.getByTestId('medium-top-supply').parentElement?.className).toBe('grid gap-4 lg:grid-cols-2');
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

describe.skipIf(!haveFixture())('Country page: layers as yard units (Yards & Fences, 2026-10-30)', () => {
  beforeEach(() => setPostureStore(fixtureStore({ yards: true, economies: { AAA: { iso3: 'USA', name: 'United States' } } })));
  const layerOf = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search).get('layer');
  const CONTROL = 'Battery-management firmware, energy-management software and remote updates';

  it("a layer whose own choice differs from its step's sits behind its own fence, named on a Layer line, ranked with its step's value", async () => {
    await usa();
    // High fence: the Secure control layer of a Preserve step (Medium fence), after the Secure steps (its step's need, 40)
    const high = within(sec('high-fence')).getByTestId('high-top');
    await waitFor(() => expect(topKeys(high)).toEqual(['Batteries|Refining', 'Solar|Refining', 'Solar|Components', 'Batteries|Assembly#bess-control']), { timeout: 30_000 });
    expect(new Set(topVerbs(high))).toEqual(new Set(['secure']));
    expect(topValues(high)).toEqual(['95', '90', '70', '40']);
    const li = high.querySelector('li[data-layer="bess-control"]') as HTMLElement;
    expect(within(li).getByTestId('fence-layer').textContent).toBe(`Layer: ${CONTROL}`);
    expect(topWhy(high)[3]).toBe('Secure · Control built partly into the hardware: the high fence extends to the hardware.');
    expect(high.textContent).toContain('a layer ranks with its step');
    expect(within(li).getByRole('button').getAttribute('aria-label')).toBe(
      `Batteries Final Assembly & Equipment, layer ${CONTROL}: Secure; supply-chain need 40 (its step’s); Control built partly into the hardware: the high fence extends to the hardware.`,
    );
    // the High fence's table: a Layer column, the step's need variables on the layer's row
    const table = within(await openFold('high-all')).getByTestId('high-table');
    expect(heads(table)).toEqual(['Node', 'Layer', 'Supply-chain need', 'Import dependence', 'Supplier concentration', 'Risky supplier exposure', 'Lack of trusted alternatives', 'Security rationale']);
    expect(rows(table).map((r) => [r[0], r[1], r[2]])).toEqual([
      ['Batteries · Refining & Processing', 'Whole step', '95'],
      ['Solar · Refining & Processing', 'Whole step', '90'],
      ['Solar · Component Manufacturing', 'Whole step', '70'],
      ['Batteries · Final Assembly & Equipment', `${CONTROL}Small yard`, '40'],
    ]);
    // Low fence: the Watch layers by their steps' prize (90 and 30), before the Watch steps (20, 15)
    const low = within(sec('low-fence')).getByTestId('low-top-supply');
    expect(topKeys(low)).toEqual(['Batteries|Assembly#bess-enclosure', 'Solar|Assembly#pv-racking', 'Solar|Extraction', 'Solar|Recovery']);
    expect(topValues(low)).toEqual(['90', '30', '20', '15']);
    expect(new Set(topVerbs(low))).toEqual(new Set(['watch']));
    expect([...low.querySelectorAll('[data-testid="fence-layer"]')].map((x) => x.textContent)).toEqual([
      'Layer: Enclosures, racking, thermal management and balance of system', 'Layer: Passive racking and mounting',
    ]);
    expect(topWhy(low)[1]).toMatch(/^Watch · Large-yard case: mature, passive, .*commoditized\.$/);
    // the Medium fence: a layer that shares its step's choice (battery cells, Preserve) is the step's own row, no unit
    const med = within(sec('medium-fence')).getByTestId('medium-top-supply');
    expect(topKeys(med)).toEqual(['Batteries|Assembly', 'Batteries|Components', 'Solar|Assembly', 'Batteries|Extraction']);
    expect(med.querySelector('[data-testid="fence-layer"]')).toBeNull();
    expect(heads(within(sec('medium-prize')).getByTestId('medium-prize-table'))).not.toContain('Layer');
    // the Low fence prize table: a Layer column, each layer with its own commons and option and its own market-only cap,
    // beside its step's value (whose cap is its step's: Solar Extraction's, not the layers' steps')
    const lp = within(await openFold('low-supply-all')).getByTestId('low-prize-table');
    expect(heads(lp).slice(0, 6)).toEqual(['Node', 'Layer', 'Strategic Industrial Value', 'Market prize', 'Industrial commons', 'Future option']);
    expect(rows(lp).map((r) => [r[0], r[1], r[2], r[4]])).toEqual([
      ['Batteries · Final Assembly & Equipment', 'Enclosures, racking, thermal management and balance of systemLarge yard', '90', '50market only'],
      ['Solar · Final Assembly & Equipment', 'Passive racking and mountingLarge yard', '30', '0market only'],
      ['Solar · Extraction', 'Whole step', '20capped: market only', '17'],
      ['Solar · Recovery & Recycling', 'Whole step', '15', 'not measured'],
    ]);
  });

  it("a layer unit opens its step's drawer focused on the layer (?node=&layer=); a step entry clears the layer", async () => {
    const r = await usa();
    const high = within(sec('high-fence')).getByTestId('high-top');
    await waitFor(() => expect(high.querySelector('li[data-layer="bess-control"]')).toBeTruthy(), { timeout: 30_000 });
    fireEvent.click(within(high.querySelector('li[data-layer="bess-control"]') as HTMLElement).getByRole('button'));
    await waitFor(() => expect([nodeOf(r), layerOf(r)]).toEqual(['Batteries|Assembly', 'bess-control']), { timeout: 30_000 });
    expect(await drawerTitle()).toBe('Batteries · Final Assembly & Equipment');
    const d = screen.getByTestId('cell-drawer');
    const focused = await within(d).findByText(CONTROL);
    const li = focused.closest('li') as HTMLElement;
    await waitFor(() => expect(document.activeElement).toBe(li));
    expect(li.getAttribute('data-focused')).toBe('true');
    // the entry is pressed, not its step's own entry elsewhere
    expect(within(high.querySelector('li[data-layer="bess-control"]') as HTMLElement).getByRole('button').getAttribute('aria-pressed')).toBe('true');
    expect(within(within(sec('medium-fence')).getByTestId('medium-top-supply')).getAllByRole('button')[0].getAttribute('aria-pressed')).toBe('false');
    // a step entry opens the step itself: no layer in the link, nothing focused (the Medium fence's entry of the same step)
    const step = within(sec('medium-fence')).getByTestId('medium-top-supply').querySelector('li[data-key="Batteries|Assembly"]') as HTMLElement;
    fireEvent.click(within(step).getByRole('button'));
    await waitFor(() => expect(layerOf(r)).toBeNull(), { timeout: 30_000 });
    expect(nodeOf(r)).toBe('Batteries|Assembly');
    expect(screen.getByTestId('cell-drawer').querySelector('[data-focused="true"]')).toBeNull();
    expect(within(step).getByRole('button').getAttribute('aria-pressed')).toBe('true');
    // a shared link with a layer opens it focused
    cleanup();
    const r2 = await usa('/countries/USA?node=Batteries%7CAssembly&layer=bess-enclosure');
    const d2 = await screen.findByTestId('cell-drawer', {}, { timeout: 30_000 });
    await waitFor(() => expect(d2.querySelector('[data-focused="true"]')?.getAttribute('data-layer')).toBe('bess-enclosure'), { timeout: 30_000 });
    expect(layerOf(r2)).toBe('bess-enclosure');
  }, 120_000);
});
