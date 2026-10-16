// Sectoral Compacts views on the synthetic fixture (AIB_DATA_DIR runs them on another bundle): every route
// renders under the shared shell (the landing, a candidate compact, a bilateral one without investment,
// About, an unknown id, a lower-case id, the alias); the fourth tab is current with its views nav; chart
// cards draw SVG and every one has a table twin and a CSV; no bare acronym in headings, legends, column
// headers, buttons, labels or tags; the module-level screens: no data, a broken document (Retry), a newer
// export (Reload); Home's fourth card.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { MODULE_CARDS } from '../../shell/Home.tsx';
import type { AibDoc } from '../contract.ts';
import { AibLoadError, AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { resetLastCompact } from '../nav.ts';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, readManifest } from './fixture.ts';

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

function useSource(edit?: (d: AibDoc) => void): void {
  source = fixtureSource(FIXTURE_DIR, edit);
  setAibStoreForTests(new AibStore(source));
}

function useFailing(src: AibSource): void {
  source = Object.assign(src, { calls: [] as string[] });
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

// 30 s: the first render compiles every lazy route module (slow under load)
const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const main = () => document.querySelector('main') as HTMLElement;
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);
const tabs = () => screen.getByRole('navigation', { name: 'Modules' });
const views = () => screen.getByRole('navigation', { name: 'Sectoral Compacts views' });

/** Open every table twin of the page; the number of cards that have one. */
function openTwins(): number {
  const buttons = within(main()).queryAllByRole('button', { name: 'Table' });
  for (const b of buttons) fireEvent.click(b);
  return buttons.length;
}

describe.skipIf(!haveFixture())('Sectoral Compacts routes', () => {
  const doc = readDoc();
  const first = doc.compacts[0];

  it('landing: the definition, a card per compact with its strip, every node by class; the fourth tab is current', async () => {
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(/^Compacts \| Sectoral Compacts \| Allied Industrial Base Explorer/));
    expect(screen.getByText(/variable-geometry coalition/)).toBeTruthy();
    expect(screen.getByText(/derived from the model/)).toBeTruthy();
    // the fourth tab, current; the views nav the module registered (with the first compact)
    const t = within(tabs()).getAllByRole('link');
    expect(t.map((a) => a.textContent)).toEqual(['Sector Insights', 'Strategic Insights', 'Country Insights', 'Sectoral Compacts']);
    expect(t[3].getAttribute('aria-current')).toBe('true');
    await waitFor(() => expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(['Compacts', 'Compact', 'About & data']));
    expect(within(views()).getByRole('link', { name: 'Compact' }).getAttribute('href')).toBe(`/compacts/${first.id}`);
    expect(within(views()).getByRole('link', { name: 'Compacts' }).getAttribute('aria-current')).toBe('page');
    // the cards
    const region = screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` });
    const cards = within(region).getAllByRole('article');
    expect(cards).toHaveLength(doc.compacts.length);
    expect(within(cards[0]).getByRole('link', { name: first.name }).getAttribute('href')).toBe(`/compacts/${first.id}`);
    expect(within(cards[0]).getByText(first.archetype)).toBeTruthy();
    expect(within(cards[0]).getByRole('list', { name: `${first.name}: demand served at each step` })).toBeTruthy();
    expect(within(cards[0]).getAllByRole('listitem')).toHaveLength(4);
    // the classification overview: every class present has its group; nodes link to their compact
    const nodes = screen.getByRole('region', { name: 'How every sub-sector is classified' });
    for (const cls of new Set(doc.nodes.map((n) => n.cls))) expect(within(nodes).getAllByText(cls).length).toBeGreaterThan(0);
    const linked = doc.nodes.find((n) => n.compact)!;
    expect(within(nodes).getAllByRole('link', { name: linked.compact! }).length).toBeGreaterThan(0);
    expect(main().querySelectorAll('figure svg').length).toBeGreaterThan(0);
    // the class legend: all five classes, each with its own mark (three hues, hollow, hatched)
    const legend = within(nodes).getByRole('list', { name: 'Node classes' });
    expect(within(legend).getAllByRole('listitem').map((li) => li.textContent)).toEqual(['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA']);
    const marks = within(legend).getAllByRole('listitem').map((li) => (li.querySelector('span[aria-hidden]') as HTMLElement).getAttribute('style'));
    expect(new Set(marks).size).toBe(5);
    noAcronyms();
    expect(openTwins()).toBe(1);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(1);
    expect(within(main()).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    noAcronyms();
    // the document is loaded once, with the manifest
    expect(source.calls).toEqual(['manifest.json', readManifest().files.compacts]);
  });

  it('landing: the technology filter writes tech and narrows the nodes', async () => {
    const r = renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const tech = doc.nodes[0].techs[0];
    fireEvent.change(screen.getByRole('combobox', { name: 'Technology' }), { target: { value: tech } });
    await waitFor(() => expect(r.state.location.search).toBe(`?tech=${encodeURIComponent(tech).replace(/%20/g, '+')}`));
    const n = doc.nodes.filter((x) => x.techs.includes(tech)).length;
    expect(screen.getByText(`${n} ${n === 1 ? 'node' : 'nodes'} of ${doc.nodes.length}`)).toBeTruthy();
  });

  it('a compact: the six sections in order, charts drawn, a table twin and a CSV on every card, no bare acronym', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(new RegExp(`^${first.id} .* \\| Sectoral Compacts \\|`)));
    const h2 = screen.getAllByRole('heading', { level: 2 }).map((h) => h.textContent);
    const order = ['Compact overview', 'Why it exists', 'Node-by-node structure', 'Partnership network', 'Coverage comparison', 'How resilience changes as countries join', 'Reading these results'];
    expect(h2.filter((h) => order.includes(h ?? ''))).toEqual(order);
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Compact' }).getAttribute('aria-current')).toBe('page'));
    // the role matrix: the eight roles as columns, an economy per row, its name (never a bare code)
    const roles = screen.getByRole('region', { name: 'Members and their roles' });
    const heads = within(roles).getAllByRole('columnheader').map((h) => h.textContent);
    expect(heads).toEqual(['Economy', 'Demand anchor', 'Specialized supplier', 'Resource supplier', 'Capacity-building / investment host', 'Technology partner', 'Bridge partner', 'Associate partner', 'Supplier']);
    expect(within(roles).getAllByRole('rowheader').length).toBe(new Set(first.roles.map((x) => x.iso3)).size);
    // why it exists: the five tiles
    const why = screen.getByRole('region', { name: 'The case for coordination' });
    for (const t of ['Shock exposure', 'Coordination dividend', 'Residual gap after trade', 'Residual gap after investment', 'Concentration']) expect(within(why).getByText(t)).toBeTruthy();
    // node by node: one item per node, its class, the data-quality disclosure
    const nodes = screen.getByRole('region', { name: "Each node's coverage and treatment" });
    expect(within(nodes).getAllByRole('heading', { level: 4 }).map((h) => h.textContent)).toEqual(first.nodes.map((id) => doc.nodes.find((n) => n.id === id)!.label));
    expect(within(nodes).getAllByText(/^Data quality:/).length).toBe(first.nodes.length);
    // the charts: Sankeys, the coverage bars and the coalition curve all draw SVG
    expect(within(screen.getByRole('region', { name: 'Trade relationships' })).getByRole('img', { name: /exporter to importer/ })).toBeTruthy();
    if (first.investment.length) expect(within(screen.getByRole('region', { name: 'Investment relationships' })).getByRole('img', { name: /investor to host/ })).toBeTruthy();
    expect(within(screen.getByRole('region', { name: 'Demand served at each step' })).getAllByRole('img').length).toBeGreaterThan(0);
    expect(within(screen.getByRole('region', { name: 'Coverage as members join' })).getAllByRole('img').length).toBeGreaterThan(0);
    expect(main().querySelectorAll('figure svg').length).toBeGreaterThanOrEqual(5);
    noAcronyms();
    const cards = openTwins();
    expect(cards).toBe(9);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(9);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(cards);
    noAcronyms();
  });

  it('every compact renders its six sections and opens its table twins without a bare acronym', async () => {
    for (const c of doc.compacts) {
      renderAt(`/compacts/${c.id}`);
      expect(await h1(c.name)).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: 'How resilience changes as countries join' })).toBeTruthy();
      noAcronyms();
      expect(openTwins()).toBe(9);
      noAcronyms();
      cleanup();
    }
  });

  it('a compact: the type, the class mix and secondary class, compact vs fragmented action and the global optimum', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    const design = screen.getByRole('region', { name: "Included nodes and the compact's design" });
    if (first.classMix) {
      const mix = within(design).getByTestId('class-mix');
      for (const part of first.classMix.split('; ')) expect(mix.textContent).toContain(part);
    }
    if (first.secondaryClass) expect(within(design).getByTestId('secondary-class').textContent).toContain(first.secondaryClass);
    const why = screen.getByRole('region', { name: 'The case for coordination' });
    const m = first.metrics;
    if (typeof m.anchorFragmented === 'number' && typeof m.anchorCompact === 'number') {
      expect(within(why).getByText('Compact vs fragmented action')).toBeTruthy();
      expect(within(why).getByText(new RegExp(`${(m.anchorFragmented * 100).toFixed(1)}% through existing relationships, ${(m.anchorCompact * 100).toFixed(1)}% under`))).toBeTruthy();
    }
    const cov = screen.getByRole('region', { name: 'Demand served at each step' });
    if (typeof m.globalOptimum === 'number') {
      expect(within(cov).getByRole('list', { name: 'Coverage steps' }).textContent).toContain('If every economy coordinated');
      fireEvent.click(within(cov).getByRole('button', { name: 'Table' }));
      expect(within(cov).getByText('If every economy coordinated (reference)')).toBeTruthy();
    }
    noAcronyms();
  });

  it('a "compact unnecessary" group is shown, with a muted tag and its explanation, on the card and the page', async () => {
    const u = doc.compacts.find((c) => c.type.startsWith('compact unnecessary'));
    if (!u) return;
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const card = screen.getAllByRole('article').find((a) => within(a).queryByRole('link', { name: u.name }))!;
    const tag = within(card).getByText('Compact unnecessary');
    expect(tag.getAttribute('data-tone')).toBe('unnecessary');
    expect(within(card).getByText(/^Existing relationships do nearly as well/)).toBeTruthy();
    cleanup();
    renderAt(`/compacts/${u.id}`);
    expect(await h1(u.name)).toBeTruthy();
    expect(screen.getAllByText('Compact unnecessary').length).toBeGreaterThanOrEqual(2);
    expect(screen.getByRole('region', { name: 'Why it exists' }).textContent).toMatch(/existing relationships, not a new coalition, carry most of the response/);
    noAcronyms();
  });

  it('the specialization-resilience frontier: a node selector, the readings, a table twin and a CSV', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    const card = screen.getByRole('region', { name: 'Specialization and resilience by supplier limit' });
    const withFrontier = first.nodes.map((id) => doc.nodes.find((n) => n.id === id)!).filter((n) => n.frontier.length);
    if (!withFrontier.length) return;
    // the plot draws after its width is measured
    await waitFor(() => expect(within(card).getAllByRole('img').length).toBeGreaterThan(0));
    expect(within(card).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    const target = withFrontier[withFrontier.length - 1];
    if (withFrontier.length > 1) {
      fireEvent.change(within(card).getByRole('combobox', { name: 'Sub-sector' }), { target: { value: target.id } });
      await waitFor(() => expect(within(card).getByText(new RegExp(`^${target.label.replace(/[&()]/g, '.')}: the tightest limit`))).toBeTruthy());
    }
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    const table = within(card).getByRole('table');
    expect(within(table).getAllByRole('columnheader').map((h) => h.textContent)).toEqual([
      'Supplier-share limit', 'Coverage', 'Coverage if the largest supplier is also lost', 'Specialization score', 'Largest optimized supplier', 'Its share',
    ]);
    expect(within(table).getAllByRole('row')).toHaveLength(target.frontier.length + 1);
    noAcronyms();
  });

  it('a compact: the node filter scopes the network cards', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    const nodeId = first.trade[first.trade.length - 1].node;
    const label = doc.nodes.find((n) => n.id === nodeId)!.label;
    fireEvent.change(screen.getByRole('combobox', { name: 'Node' }), { target: { value: nodeId } });
    const nTrade = first.trade.filter((e) => e.node === nodeId).length;
    const nInv = first.investment.filter((e) => e.node === nodeId).length;
    await waitFor(() => expect(screen.getByText(`${nTrade} trade relationship${nTrade === 1 ? '' : 's'}, ${nInv} investment relationship${nInv === 1 ? '' : 's'}`)).toBeTruthy());
    expect(within(screen.getByRole('region', { name: 'Trade relationships' })).getByText(new RegExp(`Exporter to importer, ${label.replace(/[&()]/g, '.')}`))).toBeTruthy();
  });

  it('a compact without investment relationships says so; an unreached top target is explained', async () => {
    const c = doc.compacts.find((x) => !x.investment.length);
    if (!c) return;
    renderAt(`/compacts/${c.id.toLowerCase()}`);
    expect(await h1(c.name)).toBeTruthy();
    expect(within(screen.getByRole('region', { name: 'Investment relationships' })).getByText(/No investment relationships/)).toBeTruthy();
    if (!c.coalition.reached99) expect(screen.getByText(/The highest target is not reached/)).toBeTruthy();
    noAcronyms();
  });

  it('about: the method chain, the classes, the data and the thresholds from the data', async () => {
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    for (const h of ['What a Sectoral Compact is', 'The method in brief', 'The node classes', 'Data and settings', 'The full method']) expect(screen.getByRole('heading', { level: 2, name: h })).toBeTruthy();
    expect(screen.getByText(String(doc.meta.tradeYear))).toBeTruthy();
    for (const k of Object.keys(doc.meta.thresholds)) expect(screen.getByText(k)).toBeTruthy();
    expect(screen.getByText('docs/aib_methodology.md')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'About & data' }).getAttribute('aria-current')).toBe('page'));
    noAcronyms();
  });

  it('an unknown compact id: the landing with a notice; deeper paths go to the compact; the alias', async () => {
    const r = renderAt('/compacts/NOPE-9');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(screen.getByText("No compact with the id 'NOPE-9'; showing all compacts.")).toBeTruthy();
    expect(r.state.location.pathname).toBe('/compacts');
    cleanup();
    const r2 = renderAt(`/compacts/${first.id}/extra?x=1`);
    expect(await h1(first.name)).toBeTruthy();
    expect(r2.state.location.pathname + r2.state.location.search).toBe(`/compacts/${first.id}?x=1`);
    cleanup();
    const r3 = renderAt('/sectoral-compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(r3.state.location.pathname).toBe('/compacts');
  });

  it('no data: the module-level notice with the developer hint; the shell and the nav stay', async () => {
    useFailing({
      manifest: async () => {
        throw new AibLoadError('NO_DATA', 'data/aib/manifest.json does not exist (HTTP 404).');
      },
      json: async () => ({}),
    });
    renderAt('/compacts/about');
    expect(await screen.findByText('No compacts data', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/scripts\/84_export_aib_data\.R/)).toBeTruthy();
    expect(within(tabs()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('true');
    expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(['Compacts', 'About & data']);
    expect(screen.getByRole('contentinfo')).toBeTruthy();
    noAcronyms();
  });

  it('a broken document: the error notice; Retry loads again', async () => {
    let broken = true;
    useSource((d) => {
      if (broken) delete (d as unknown as Record<string, unknown>).compacts;
    });
    renderAt('/compacts');
    expect(await screen.findByText('The compacts data could not be loaded', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/does not match the contract \(compacts: expected an array\)/)).toBeTruthy();
    broken = false;
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(await h1('Sectoral Compacts')).toBeTruthy();
  });

  it('a newer export asks for a reload', async () => {
    useFailing({ manifest: async () => ({ ...readManifest(), version: 2 }), json: async () => ({}) });
    renderAt('/compacts');
    expect(await screen.findByText('Please reload', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Reload' })).toBeTruthy();
  });
});

describe('Home', () => {
  it('has the fourth card: Sectoral Compacts, its tag, its links and its button', async () => {
    expect(MODULE_CARDS.map((c) => c.id)).toEqual(['sector', 'strategic', 'country', 'aib']);
    const card = MODULE_CARDS[3];
    expect(card).toMatchObject({ title: 'Sectoral Compacts', tag: 'COORDINATION DESIGN', button: 'Open Sectoral Compacts' });
    expect(card.links.every((l) => l.to.startsWith('/compacts'))).toBe(true);
  });
});
