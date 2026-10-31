// The Sector pages at six stages (2026-10-28, design S1-S5, C2, C3, C6, E5, L3, A2, U1) on the TS fixture:
// the "What each country should do" sections (they were "Strategic posture"; the Sector home's, with its node in ?pnode and its own basis line; every
// headline profile's, fixed to its node), the old three-stage links (Downstream is Deployment exactly;
// Upstream and Midstream go to the chain page with a notice), the copied manufacturing figures ("same
// figure as"), the Allied Industrial Base nodes joined on their six-stage cells, and no three-stage word
// on screen.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { buildSectorFixture, fixtureSource } from '../__fixtures__/build.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { setPostureStore } from '../../posture/store.ts';
import { fixtureSource as aibFixtureSource, SYNTHETIC_DIR as AIB_DIR } from '../../aib/__tests__/fixture.ts';
import { AibStore, setAibStoreForTests } from '../../aib/data/store.ts';

const restore: (() => void)[] = [];

beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q),
    media: q, onchange: null,
    addEventListener: () => undefined, removeEventListener: () => undefined, addListener: () => undefined, removeListener: () => undefined, dispatchEvent: () => false,
  })) as unknown as typeof window.matchMedia;
  window.scrollTo = (() => undefined) as typeof window.scrollTo;
  Element.prototype.scrollIntoView = function () {} as typeof Element.prototype.scrollIntoView;
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
  setSectorStoreForTests(new SectorStore(fixtureSource(buildSectorFixture())));
  setPostureStore(fixtureStore());
  setAibStoreForTests(new AibStore(aibFixtureSource(AIB_DIR)));
});

afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
  setPostureStore(null);
  setAibStoreForTests(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const openGroups = async () => {
  for (const b of await screen.findAllByRole('button', { name: 'Show' }, { timeout: 30_000 })) fireEvent.click(b);
};
const LEGACY = /upstream|midstream|downstream|three stages|three-stage|paper’s three|paper's three/i;

describe('the Sector home: a "What each country should do" section (design L3)', () => {
  it('sits after the sector grid with its own basis line; the lede points to it and no longer says the page has no recommendation', async () => {
    renderAt('/sector');
    // the first screen states the strategic question (the Home card's) before any method
    expect(await h1('Where are the real bottlenecks and opportunities?')).toBeTruthy();
    expect(screen.queryByText(/MEASURED DATA|DESCRIPTIVE DATA/)).toBeNull();
    const main = document.querySelector('main')?.textContent ?? '';
    expect(main).toContain(
      'Follow a technology from extraction through deployment and recovery. See who has capability, where supply is concentrated, which stages matter strategically, and where countries should build, partner or stay open.',
    );
    expect(main).toContain(
      'One section is a recommendation instead: What each country should do, each economy’s strategic choice at a step: secure it, preserve a strength, catch up, rely on partners, deploy faster or keep it open.',
    );
    expect(main).not.toMatch(/Strategic posture/);
    const section = document.getElementById('posture') as HTMLElement;
    expect(within(section).getByRole('heading', { level: 2, name: 'What each country should do' })).toBeTruthy();
    expect(section.textContent).toContain('Which countries should secure the step, preserve capability, catch up, partner, deploy faster or keep the market open?');
    expect(section.textContent).toMatch(/Country strategy · fixed thresholds; the dials and the scenario do not change it/);
    expect(section.textContent).toMatch(/Unlike the measured values on the rest of these pages, this is a recommendation/);
    // after the sector grid, before the minerals
    const order = [...document.querySelectorAll('#atlas-h, #posture, #minerals-h')].map((x) => x.id);
    expect(order).toEqual(['atlas-h', 'posture', 'minerals-h']);
    // the card inside, with its node selects and its default node (Batteries — Refining, design E6)
    const card = await within(section).findByTestId('sector-posture-hero', {}, { timeout: 10_000 });
    expect((within(card).getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement).value).toBe('Refining');
    expect(within(card).getByRole('heading', { level: 3 })).toBeTruthy();
    // the lede's link lands on the section
    expect(screen.getByRole('link', { name: 'What each country should do' }).getAttribute('href')).toBe('#posture');
    expect(document.querySelector('main')?.textContent ?? '').not.toMatch(LEGACY);
  });

  it('the card speaks the six verbs: Competitive Position against need, marks by verb, the prize in words; every stage is offered, Deployment & Use on deployment need (design 5)', async () => {
    const router = renderAt('/sector?pnode=Batteries%7CComponents');
    await h1('Where are the real bottlenecks and opportunities?');
    const section = document.getElementById('posture') as HTMLElement;
    const card = await within(section).findByTestId('sector-posture-hero', {}, { timeout: 10_000 });
    expect(within(card).getByRole('heading', { level: 3, name: "Every economy's choice at one supply-chain step" })).toBeTruthy();
    expect(section.textContent).toMatch(/recommends one choice: Secure, Preserve, Catch Up, Partner, Accelerate or Watch/);
    // the takeaway names the groups ("led by" past three), the counts line gives the six verbs first
    expect(within(card).getByTestId('posture-takeaway').textContent).toBe(
      'In battery cell manufacturing, 2 economies should catch up from a credible base (Bland and Aland); and 1 economy should rely on trusted partners for it (Cland).',
    );
    expect(within(card).getByTestId('sector-strategy-counts').textContent).toMatch(/^0 Secure · 0 Preserve · 2 Catch Up · 1 Partner · 0 Accelerate · 0 Watch \(0 High fence · 3 Medium fence · 0 Low fence\)$/);
    expect(within(card).getByTestId('posture-reading').textContent).toMatch(
      /Across: how strong it is today \(Competitive Position\s*, ranked among the main producers of this step\); up: its supply-chain vulnerability\s*\. The colour is the choice, grouped by fence\./,
    );
    expect(within(card).getByTestId('posture-reading').textContent).not.toMatch(/contenders|node/);
    // the two sides of the allied division of labor: who is strongest, who needs a partner
    expect(within(card).getByTestId('sector-strategy-sides').textContent).toMatch(/^Who has the strongest capability\? .+\. Which countries need a partner\? Cland \(their choice is Partner\)\.$/);
    const chart = within(card).getByTestId('sector-strategy-scatter');
    expect([...chart.querySelectorAll('[data-verb]')].map((g) => [g.getAttribute('data-iso3'), g.getAttribute('data-verb')]).sort()).toEqual([['AAA', 'catch_up'], ['BBB', 'catch_up'], ['CCC', 'partner']]);
    expect(chart.querySelector('[data-testid="line-cp"]')).toBeTruthy();
    expect(within(card).getByTestId('sector-strategy-prize').textContent).toContain('2 of 3 at or above the bar of 60');
    expect(within(card).getByTestId('sector-strategy-legend').textContent).toContain('Medium fence:Preserve,Catch Up,Partner');
    // every stage is offered, by its display name; Deployment & Use is no longer disabled
    const stage = within(card).getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement;
    expect([...stage.options].map((o) => [o.value, o.textContent, o.disabled])).toEqual([
      ['Extraction', 'Extraction', false], ['Refining', 'Refining & Processing', false], ['Components', 'Component Manufacturing', false],
      ['Assembly', 'Final Assembly & Equipment', false], ['Deployment', 'Deployment & Use', false], ['Recovery', 'Recovery & Recycling', false],
    ]);
    fireEvent.change(stage, { target: { value: 'Deployment' } });
    await waitFor(() => expect(new URLSearchParams(router.state.location.search).get('pnode')).toBe('Batteries|Deployment'));
    await waitFor(() => expect(within(card).getByTestId('sector-strategy-scatter').getAttribute('data-stage')).toBe('Deployment'));
    expect(within(card).getByTestId('posture-takeaway').textContent).toBe('In battery storage, 2 economies should deploy faster (Bland and Aland); and 1 economy should watch rather than accelerate deployment (need below the bar for acceleration).');
    // Deployment & Use has no Partner: no supply and demand sides
    expect(within(card).queryByTestId('sector-strategy-sides')).toBeNull();
    expect(within(card).getByTestId('sector-strategy-scatter').querySelector('[data-testid="line-cp"]')).toBeNull();
    // a mark opens that economy's Country Strategy Map on this node (design D8)
    fireEvent.click(within(card).getByTestId('sector-strategy-scatter').querySelector('[data-iso3="BBB"]') as Element);
    await waitFor(() => expect(router.state.location.pathname).toBe('/countries/BBB'), { timeout: 20_000 });
    expect([new URLSearchParams(router.state.location.search).get('node'), router.state.location.hash]).toEqual(['Batteries|Deployment', '#strategy']);
  });

  it('an old three-stage ?pnode is reported: Downstream is now Deployment and is shown', async () => {
    renderAt('/sector?pnode=Batteries%7CDownstream');
    await h1('Where are the real bottlenecks and opportunities?');
    const notice = await screen.findByTestId('posture-node-ignored', {}, { timeout: 10_000 });
    expect(notice.textContent).toContain('pnode=Batteries|Downstream (Downstream is now Deployment; showing Batteries — Deployment)');
    expect(within(document.getElementById('posture') as HTMLElement).getByTestId('sector-strategy-scatter').getAttribute('data-stage')).toBe('Deployment');
  });

  it('the Data notes say the strategic choices are the one recommendation, at fixed thresholds', async () => {
    renderAt('/sector');
    await h1('Where are the real bottlenecks and opportunities?');
    const bar = screen.getByTestId('sector-bar');
    fireEvent.click(within(bar).getAllByText(/^Data notes \(/)[0]);
    expect(within(bar).getAllByText(/except the “What each country should do” sections: a recommendation at fixed thresholds/).length).toBeGreaterThan(0);
    expect(screen.queryByText(/they show measured values with no weights and no scores\.$/)).toBeNull();
  });
});

describe('a sector profile: the "What each country should do" section fixed to its node (design L3)', () => {
  it('Batteries — Refining: the section after the lenses, in the rail, the card on this node with no selects', async () => {
    renderAt('/sector/batteries/refining');
    expect(await h1('Batteries — Refining')).toBeTruthy();
    const section = await screen.findByRole('region', { name: 'What each country should do' }, { timeout: 20_000 });
    expect(section.id).toBe('posture');
    expect(within(section).getByRole('heading', { level: 2, name: 'What each country should do' })).toBeTruthy();
    expect(section.textContent).toMatch(/Country strategy · fixed thresholds/);
    expect(await within(section).findByRole('heading', { level: 3, name: "Every economy's choice in Batteries — Refining & Processing" }, { timeout: 10_000 })).toBeTruthy();
    expect(within(section).queryByRole('combobox')).toBeNull();
    // the section follows the lenses and comes before the groups (Advanced & evidence is always there); the rail lists it second
    const overview = document.getElementById('overview') as HTMLElement;
    expect(overview.compareDocumentPosition(section) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(section.compareDocumentPosition(document.getElementById('evidence') as HTMLElement) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    const rail = screen.getByRole('navigation', { name: 'Sections' });
    expect(within(rail).getByRole('link', { name: 'What each country should do' }).getAttribute('href')).toBe('#posture');
  });

  it('a link to #posture lands there; Deployment is allowed on its profile; a non-headline sector has no posture section', async () => {
    renderAt('/sector/solar/deployment#posture');
    expect(await h1('Solar — Deployment')).toBeTruthy();
    const section = await screen.findByRole('region', { name: 'What each country should do' }, { timeout: 20_000 });
    expect(await within(section).findByRole('heading', { level: 3, name: "Every economy's choice in Solar — Deployment & Use" }, { timeout: 10_000 })).toBeTruthy();
    // Deployment & Use is plotted on deployment need: Accelerate or Watch, never Partner
    const chart = await within(section).findByTestId('sector-strategy-scatter', {}, { timeout: 10_000 });
    expect(chart.getAttribute('data-stage')).toBe('Deployment');
    expect(within(section).getByTestId('posture-reading').textContent).toContain('deployment need');
    expect([...chart.querySelectorAll('[data-verb]')].every((g) => ['accelerate', 'watch', 'secure'].includes(g.getAttribute('data-verb') ?? ''))).toBe(true);
    expect(within(section).getByTestId('sector-strategy-rule').textContent).toMatch(/^At Deployment & Use the choice reads deployment need only/);
    cleanup();
    renderAt('/sector/semiconductors/components');
    expect(await h1('Semiconductors — Components')).toBeTruthy();
    await screen.findByRole('navigation', { name: 'Sections' }, { timeout: 20_000 });
    expect(document.getElementById('posture')).toBeNull();
    expect(within(screen.getByRole('navigation', { name: 'Sections' })).queryByRole('link', { name: 'What each country should do' })).toBeNull();
  });
});

describe('old three-stage links (design U1)', () => {
  it('Downstream is Deployment exactly: path, economy and hash kept, with a notice', async () => {
    const r = renderAt('/sector/batteries/downstream#investment');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries/deployment'), { timeout: 20_000 });
    expect(r.state.location.hash).toBe('#investment');
    expect(await screen.findByText('Batteries — Downstream is now Batteries — Deployment.', {}, { timeout: 10_000 })).toBeTruthy();
    cleanup();
    const r2 = renderAt('/sector/solar/downstream/USA');
    await waitFor(() => expect(r2.state.location.pathname).toBe('/sector/solar/deployment/USA'), { timeout: 20_000 });
  });

  it('Upstream and Midstream split: the technology chain page, with a notice naming the stages that replaced them', async () => {
    const r = renderAt('/sector/batteries/midstream?c=USA#trade');
    await waitFor(() => expect(r.state.location.pathname).toBe('/sector/batteries'), { timeout: 20_000 });
    expect(r.state.location.search).toBe('?c=USA');
    expect(await screen.findByText(/^Batteries — Midstream is no longer a stage: it is now mostly Components and Assembly \(some refined materials are under Refining\)\./, {}, { timeout: 10_000 })).toBeTruthy();
    expect(await h1('Batteries: the supply chain')).toBeTruthy();
    cleanup();
    const r2 = renderAt('/sector/oil/upstream/RUS');
    await waitFor(() => expect(r2.state.location.pathname).toBe('/sector/oil'), { timeout: 20_000 });
    expect(await screen.findByText(/^Oil — Upstream is no longer a stage: it is now Extraction, Refining and Recovery\./, {}, { timeout: 10_000 })).toBeTruthy();
  });
});

describe('copied figures, the AIB nodes and the words on screen', () => {
  it('Batteries — Components and — Assembly: the manufacturing capex and capacity shares say "same figure as" the other stage (design C2)', async () => {
    renderAt('/sector/batteries/components');
    expect(await h1('Batteries — Components')).toBeTruthy();
    await openGroups();
    const notes = await screen.findAllByTestId('same-figure-note', {}, { timeout: 20_000 });
    expect(notes.length).toBeGreaterThanOrEqual(1);
    for (const n of notes) expect(n.textContent).toBe('Same figure as Batteries — Assembly: the source gives one figure for the technology’s manufacturing, not one per stage. Never add the two; totals count it once.');
    cleanup();
    renderAt('/sector/batteries/assembly');
    expect(await h1('Batteries — Assembly')).toBeTruthy();
    await openGroups();
    const back = await screen.findAllByTestId('same-figure-note', {}, { timeout: 20_000 });
    expect(back[0].textContent).toMatch(/^Same figure as Batteries — Components:/);
  });

  it('the Allied Industrial Base nodes join the six-stage cell on their own cells (design A2), with their stage and technologies', async () => {
    renderAt('/sector/batteries/refining');
    expect(await h1('Batteries — Refining')).toBeTruthy();
    await openGroups();
    const block = await waitFor(() => {
      const el = document.getElementById('aib-nodes');
      if (!el) throw new Error('no AIB block yet');
      return el;
    }, { timeout: 20_000 });
    expect(within(block).getByRole('columnheader', { name: /Supply-chain stage/ })).toBeTruthy();
    // the Refining node of the fixture (its yfCells: batteries-refining); its three-stage cell (batteries-midstream) is not read
    const row = within(block).getByRole('row', { name: /Synthetic graphite/ });
    expect(row.textContent).toMatch(/Refining/);
    expect(row.textContent).toMatch(/Batteries/);
    // only the node of this cell (the Extraction node, Natural graphite, sits on batteries-extraction)
    expect([...block.querySelectorAll('tbody tr')].map((tr) => tr.querySelector('td')?.textContent?.trim())).toEqual(['Synthetic graphite (anode-grade, spherical)']);
    // every node's stage is this cell's: no stage note; no three-stage words
    expect(within(block).queryByTestId('aib-nodes-stages')).toBeNull();
    expect(block.textContent).not.toMatch(LEGACY);
  });

  it('no three-stage word on the chain, profile, Explore and Concentration pages', async () => {
    for (const [path, title] of [
      ['/sector/batteries', 'Batteries: the supply chain'],
      ['/sector/batteries/refining', 'Batteries — Refining'],
      ['/sector/concentration', 'Where are the chokepoints?'],
      ['/sector/explore/trade?tech=Batteries&stage=Assembly', 'Trade'],
    ] as const) {
      cleanup();
      renderAt(path);
      expect(await h1(title)).toBeTruthy();
      await waitFor(() => expect(document.querySelector('main')?.textContent ?? '').not.toMatch(/Loading the sector/), { timeout: 20_000 });
      expect(document.querySelector('main')?.textContent ?? '', path).not.toMatch(LEGACY);
    }
  });
});
