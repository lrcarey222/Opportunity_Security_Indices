// Sectoral Compacts views on the synthetic fixture (AIB_DATA_DIR runs them on another bundle): every route
// renders under the shared shell (the landing, a candidate compact, a bilateral one without investment,
// About, an unknown id, a lower-case id, the alias); the fourth tab is current with its views nav; chart
// cards draw SVG and every one has a table twin and a CSV; no bare acronym in headings, legends, column
// headers, buttons, labels or tags; the methodology: the strategic-scope filter of the node list, policy
// needs and the physical-capability warning wherever a class is shown (the headline, each variant, each
// growth bound), the growth-bound flag on the landing and the compact page, the node detail's variants,
// growth-bound sensitivity and N-1 readings, the exact coalition with its tie-break, greedy twin and six
// tiers (what would replace each member, from the data), an unreached target's widest set, About's
// settings and rules; an older bundle without the methodology fields still renders; the module-level
// screens: no data, a broken document (Retry), a newer export (Reload); Home's fourth card.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { MODULE_CARDS } from '../../shell/Home.tsx';
import type { AibDoc, AibNode } from '../contract.ts';
import { AibLoadError, AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { resetLastCompact } from '../nav.ts';
import { peekEngineClient, setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, readManifest, stripMethodology } from './fixture.ts';

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

/** A client whose worker never answers: the engine stays 'starting' for good (as in app.test). */
function neverReadyClient(): EngineClient {
  return new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/');
}

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
const views = () => screen.getByRole('navigation', { name: 'Allied Industrial Base views' });
const AIB_VIEWS = ['Overview', 'Partnerships', 'Sectoral Compacts', 'Country roles'];

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
    await waitFor(() => expect(document.title).toMatch(/^Compacts \| Allied Industrial Base \| Allied Industrial Base Explorer/));
    expect(screen.getByText(/variable-geometry coalition/)).toBeTruthy();
    expect(screen.getByText(/derived from the model/)).toBeTruthy();
    // the Allied Industrial Base tab, current; the views nav the module registered (IA refactor)
    const t = within(tabs()).getAllByRole('link');
    expect(t.map((a) => a.textContent)).toEqual(['Sectors', 'Countries', 'Allied Industrial Base', 'About']);
    expect(t[2].getAttribute('aria-current')).toBe('true');
    await waitFor(() => expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(AIB_VIEWS));
    expect(within(views()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page');
    // the cards
    const region = screen.getByRole('region', { name: `The compacts (${doc.compacts.length})` });
    const cards = within(region).getAllByRole('article');
    expect(cards).toHaveLength(doc.compacts.length);
    expect(within(cards[0]).getByRole('link', { name: first.name }).getAttribute('href')).toBe(`/compacts/${first.id}`);
    // the slim card (IA refactor): only why it exists, main nodes, key countries, the policy need, the resilience effect, View compact
    expect(within(cards[0]).getAllByRole('term').map((t) => t.textContent)).toEqual(['Why it exists', 'Main nodes', 'Key countries', 'Coalition', 'Policy need', 'Resilience effect']);
    expect(within(cards[0]).getByRole('link', { name: `View compact: ${first.name}` }).getAttribute('href')).toBe(`/compacts/${first.id}`);
    expect(within(cards[0]).getByRole('list', { name: `${first.name}: demand served at each step` })).toBeTruthy();
    expect(within(cards[0]).getAllByRole('listitem')).toHaveLength(4);
    // the strategic map first, the full node table behind "Explore all nodes"
    const map = screen.getByRole('region', { name: 'Where compacts are justified' });
    expect(within(map).getByText('Observed / modelled facts')).toBeTruthy();
    expect(within(map).getByRole('img', { name: /^Strategic map of / })).toBeTruthy();
    const explore = screen.getByText(/^Explore all nodes \(/).closest('details') as HTMLDetailsElement;
    expect(explore.open).toBe(false);
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
    expect(openTwins()).toBe(2);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(2);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(2);
    noAcronyms();
    // the document is loaded once, with the manifest
    expect(source.calls).toEqual(['manifest.json', readManifest().files.compacts]);
  });

  it('landing: the technology filter writes tech and narrows the nodes (of the strategic scope)', async () => {
    const r = renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const inScope = doc.nodes.filter((x) => x.inStrategicScope !== false);
    const nOut = doc.nodes.length - inScope.length;
    const tech = inScope[0].techs[0];
    fireEvent.change(screen.getByRole('combobox', { name: 'Technology' }), { target: { value: tech } });
    await waitFor(() => expect(r.state.location.search).toBe(`?tech=${encodeURIComponent(tech).replace(/%20/g, '+')}`));
    const n = inScope.filter((x) => x.techs.includes(tech)).length;
    expect(screen.getByText(`${n} ${n === 1 ? 'node' : 'nodes'} of ${inScope.length}${nOut ? ` in the strategic scope (${nOut} outside it not shown)` : ''}`)).toBeTruthy();
  });

  it('landing: the run settings; out-of-scope nodes hidden until asked for; policy needs and warnings on the node chips', async () => {
    const out = doc.nodes.filter((x) => x.inStrategicScope === false);
    if (!out.length || !doc.meta.demandScope) return;
    const r = renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const run = screen.getByTestId('run-settings');
    // "coalition" demand is the allied core and partner economies, not a compact's coalition
    expect(run.textContent).toMatch(/Demand scope: the allied core and partner economies of the coalition table \(\d+ economies\), not a compact's coalition/);
    expect(run.textContent).toMatch(/Candidate partners: any economy in the trade data may add or redirect supply/);
    expect(run.textContent).toMatch(/hold semantics \(the shocked economy keeps buying\), production-constrained supply/);
    expect(run.textContent).toMatch(/Strategic scope: electro-industrial/);
    expect(run.textContent).toMatch(/share of the demand scope's baseline traded demand/);
    const nodes = screen.getByRole('region', { name: 'How every sub-sector is classified' });
    for (const n of out) expect(within(nodes).queryByText(n.label)).toBeNull();
    // the warning is a visible tag on the chip, never only a tooltip
    const warned = doc.nodes.find((x) => x.warning && x.inStrategicScope !== false)!;
    const chip = within(nodes).getByText(warned.label).closest('li') as HTMLElement;
    expect(within(chip).getByTestId('physical-warning').textContent).toContain('PHYSICAL CAPABILITY NOT VALIDATED');
    // policy needs where they say more than the class
    const multi = doc.nodes.find((x) => (x.policyNeeds?.length ?? 0) > 1 && x.inStrategicScope !== false)!;
    const mchip = within(nodes).getByText(multi.label).closest('li') as HTMLElement;
    expect(within(mchip).getByTestId('policy-needs').textContent).toBe(`Policy needs: ${[...multi.policyNeeds!].join(' + ')}`);
    noAcronyms();
    // include the nodes outside the scope (the filter sits over the strategic map and drives both): the URL keeps it, they appear marked
    fireEvent.click(within(screen.getByRole('region', { name: 'Where compacts are justified' })).getByRole('checkbox', { name: 'Include nodes outside the strategic scope' }));
    await waitFor(() => expect(r.state.location.search).toBe('?scope=all'));
    // (a label can recur across technologies, or name a technology too)
    for (const n of out) {
      const lis = within(nodes).getAllByText(n.label).map((el) => el.closest('li'));
      expect(lis.some((li) => /outside the strategic scope/.test(li?.textContent ?? '')), n.id).toBe(true);
    }
    expect(screen.getByText(`${doc.nodes.length} nodes of ${doc.nodes.length}`)).toBeTruthy();
    noAcronyms();
  });

  it('landing: the growth-bound flag on the node chips and in the table, worded from the headline class; nodes outside a compact open their variants in place', async () => {
    renderAt('/compacts?scope=all');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    const nodes = screen.getByRole('region', { name: 'How every sub-sector is classified' });
    // (a label can recur across technologies: the per-chip checks take nodes whose label is unique)
    const unique = (n: AibNode) => doc.nodes.filter((x) => x.label === n.label).length === 1;
    const chipOf = (label: string) => within(nodes).getAllByText(label).map((el) => el.closest('li') as HTMLElement).find((li) => li.className.includes('hairline'))!;
    // every flagged node says so on its chip, starting from its headline class (none in a bundle without flags)
    const flagged = doc.nodes.filter((n) => n.coordinateSensitiveToGrowth);
    for (const n of flagged.filter(unique)) {
      const flag = within(chipOf(n.label)).getByTestId('growth-flag').textContent ?? '';
      expect(flag, n.id).toMatch(new RegExp(`^Headline ${n.cls}; .*: the class depends on the existing-relationship growth bound\\.$`));
      if (n.cls !== 'COORDINATE') expect(flag, n.id).toMatch(/COORDINATE (\(PHYSICAL CAPABILITY NOT VALIDATED\) )?only at /);
      // a class named under other bounds carries its own physical-capability flag
      const otherWarned = (n.growth ?? []).some((g) => g.cls !== n.cls && g.warning === true);
      expect(flag.includes('PHYSICAL CAPABILITY NOT VALIDATED'), n.id).toBe(otherWarned);
    }
    expect(within(nodes).queryAllByTestId('growth-flag')).toHaveLength(flagged.length);
    // a node outside every compact: its class under the other variants and growth bounds, each flagged class tagged
    const outside = doc.nodes.filter((n) => !n.compact && n.variants && Object.keys(n.variants).length && unique(n));
    const loose = outside.find((n) => Object.values(n.variantWarnings ?? {}).some(Boolean)) ?? outside[0];
    if (loose) {
      const chip = chipOf(loose.label);
      const t = within(chip).getByRole('table', { name: /the class under each variant/ });
      const flaggedKeys = Object.entries(loose.variantWarnings ?? {}).filter(([, w]) => w).length;
      expect(within(t).queryAllByTestId('variant-physical-warning').length, loose.id).toBeGreaterThanOrEqual(flaggedKeys);
    }
    // nodes in a compact link to it instead
    const inCompact = doc.nodes.find((n) => n.compact && unique(n));
    if (inCompact) expect(within(chipOf(inCompact.label)).queryByRole('table')).toBeNull();
    // the table twin has the flag as a column
    fireEvent.click(within(nodes).getByRole('button', { name: 'Table' }));
    const table = within(nodes).getAllByRole('table').find((x) => within(x).queryByRole('columnheader', { name: 'Class depends on the growth bound' }))!;
    expect(table).toBeTruthy();
    noAcronyms();
  });

  it('a compact: the four questions in order, method details closed, charts drawn, a table twin and a CSV on every card, no bare acronym', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(new RegExp(`^${first.id} .* \\| Allied Industrial Base \\|`)));
    // the four questions (IA refactor), then the method details (closed) with the members-joining curve and the reading notes
    const h2 = screen.getAllByRole('heading', { level: 2 }).map((h) => h.textContent);
    const order = ['Why this compact?', 'Who and what?', 'What does coordination change?', 'What should governments do?', 'How resilience changes as countries join', 'Reading these results'];
    expect(h2.filter((h) => order.includes(h ?? ''))).toEqual(order);
    const h3 = screen.getAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    for (const s of ['Coverage comparison', 'Partnership network', 'Node-by-node structure', 'Treatment node by node']) expect(h3).toContain(s);
    expect((screen.getByText(/^Method details:/).closest('details') as HTMLDetailsElement).open).toBe(false);
    expect(screen.getByTestId('why-answer').textContent).toMatch(/^The shock removes \d+% of the compact's demand/);
    expect(within(main()).getAllByText('Observed / modelled facts').length).toBeGreaterThan(0);
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page'));
    // the role matrix: the tier and the eight roles as columns, an economy per row, its name (never a bare code)
    const roles = screen.getByRole('region', { name: 'Economies and their roles' });
    const heads = within(roles).getAllByRole('columnheader').map((h) => h.textContent);
    expect(heads).toEqual(['Economy', 'Coalition tier', 'Demand anchor', 'Specialized supplier', 'Resource supplier', 'Capacity-building / investment host', 'Technology partner', 'Bridge partner', 'Associate partner', 'Supplier']);
    expect(within(roles).getAllByRole('rowheader').length).toBe(new Set(first.roles.map((x) => x.iso3)).size);
    // each economy opens its Country profile at "Role in the Allied Industrial Base"
    const iso = first.roles[0].iso3;
    expect(within(roles).getAllByRole('link').some((a) => a.getAttribute('href') === `/countries/${iso}#aib-role`)).toBe(true);
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
    expect(cards).toBe(11);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(11);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(cards);
    noAcronyms();
  });

  it('every compact renders its four questions and opens its table twins without a bare acronym', async () => {
    for (const c of doc.compacts) {
      renderAt(`/compacts/${c.id}`);
      expect(await h1(c.name)).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: 'What should governments do?' })).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: 'How resilience changes as countries join' })).toBeTruthy();
      noAcronyms();
      expect(openTwins()).toBe(11);
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
    expect(within(card).getByText(/Existing relationships do nearly as well/)).toBeTruthy();
    cleanup();
    renderAt(`/compacts/${u.id}`);
    expect(await h1(u.name)).toBeTruthy();
    expect(screen.getAllByText('Compact unnecessary').length).toBeGreaterThanOrEqual(2);
    expect(screen.getByRole('region', { name: 'Why this compact?' }).textContent).toMatch(/existing relationships, not a new coalition, carry most of the response/);
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
      'Supplier-share limit', 'Coverage', 'N-1 re-optimized (largest remaining supplier removed, trade re-optimized)', 'Supplier removed',
      'Instantaneous N-1 retained coverage (coverage × (1 − largest share))', 'Specialization score', 'Largest optimized supplier', 'Its share',
    ]);
    expect(within(table).getAllByRole('row')).toHaveLength(target.frontier.length + 1);
    noAcronyms();
  });

  it('a compact: the exact minimum coalition, the method and tie-break, the tiers with what would replace a member', async () => {
    renderAt(`/compacts/${first.id}`);
    expect(await h1(first.name)).toBeTruthy();
    const card = screen.getByRole('region', { name: 'The coalition and its tiers' });
    if (!first.coalition.method) return;
    expect(within(card).getByText(`Exact minimum coalition: ${first.coalition.members.length} economies. Method: ${first.coalition.method}.`)).toBeTruthy();
    const lead = within(card).getByTestId('coalition-method').textContent ?? '';
    expect(lead).toMatch(/^The members are the exact minimum coalition: the fewest economies/);
    // the tie-break among minimum-size coalitions, and whether it was solved
    expect(lead).toContain('among coalitions of that size the exact program keeps the one with the widest margin over the target in its weakest node, then the one that adds the most supply');
    if (first.coalition.robustTieBreakSolved === false) expect(lead).toMatch(/that tie-break hit its time limit/);
    else expect(lead).not.toMatch(/time limit/);
    const tiers = within(card).getByTestId('coalition-tiers');
    for (const label of ['Core coordinating members', 'Critical supply partners']) expect(within(tiers).getByText(label)).toBeTruthy();
    const names = doc.countries as Record<string, string>;
    const nm = (iso: string) => names[iso] ?? iso;
    const roleOf = (iso: string) => first.roles.find((r) => r.iso3 === iso && r.substitutesNeeded !== undefined) ?? first.roles.find((r) => r.iso3 === iso);
    // a critical supply partner: worded from substitutesNeeded (several economies together, or none), never from single substitutes
    for (const iso of first.coalition.criticalPartners ?? []) {
      const li = within(tiers).getByText(nm(iso)).closest('li') as HTMLElement;
      const n = roleOf(iso)?.substitutesNeeded;
      const want =
        typeof n === 'number'
          ? `no single other economy can replace it: without it, the coalition needs ${n} other economies together to reach the 95% target`
          : n === null
            ? 'without it, no coalition reaches the 95% target'
            : 'no single other economy can replace it';
      expect(li.textContent, iso).toContain(want);
    }
    // a core member with single substitutes: any one of them could replace it
    const core = first.roles.filter((r) => r.tier === 'core coordinating member' && (r.substitutes ?? []).length);
    for (const r of core) {
      const li = within(tiers).getByText(nm(r.iso3)).closest('li') as HTMLElement;
      const subs = r.substitutes!.map(nm);
      expect(li.textContent, r.iso3).toContain(subs.length === 1 ? `${subs[0]} could replace its supply` : `any one of ${subs.slice(0, -1).join(', ')} or ${subs[subs.length - 1]}`);
    }
    for (const iso of [...(first.coalition.criticalExternal ?? []), ...(first.coalition.external ?? [])]) expect(within(tiers).getAllByText(nm(iso)).length).toBeGreaterThan(0);
    if ((first.coalition.criticalExternal ?? []).length) expect(within(tiers).getByText('Critical external dependencies')).toBeTruthy();
    if ((first.coalition.criticalExternalMarginal ?? []).length) {
      expect(within(tiers).getByText('Critical external dependencies (marginal)')).toBeTruthy();
      const inline = within(tiers).getByTestId('inline-tier');
      for (const iso of first.coalition.criticalExternalMarginal!) expect(inline.textContent).toContain(nm(iso));
    }
    // the role matrix carries each economy's tier; externals are not members and never read as associates
    const roles = screen.getByRole('region', { name: 'Economies and their roles' });
    for (const iso of first.coalition.criticalExternal ?? []) {
      const tier = first.roles.find((r) => r.iso3 === iso)?.tier;
      if (tier !== 'critical external dependency') continue;
      const row = within(roles).getByRole('rowheader', { name: nm(iso) }).closest('tr') as HTMLElement;
      expect(row.textContent).toContain('Critical external dependency');
    }
    // the table twin: single substitutes and the economies needed to replace each member, from the data
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    const table = within(card).getByRole('table');
    const heads = within(table).getAllByRole('columnheader').map((h) => h.textContent);
    expect(heads).toEqual(['Economy', 'Coalition tier', 'Critical', 'Needed with the coalition fixed', 'Single substitutes (any one could replace it)', 'Economies needed to replace it', 'Roles']);
    const col = (h: string) => heads.indexOf(h);
    for (const tr of within(table).getAllByRole('row').slice(1)) {
      const cells = within(tr).getAllByRole('cell').map((td) => td.textContent ?? '');
      const iso = Object.keys(names).find((k) => names[k] === cells[0]) ?? cells[0];
      const r = roleOf(iso);
      if (!r) continue;
      const subs = (r.substitutes ?? []).map(nm);
      expect(cells[col('Single substitutes (any one could replace it)')], iso).toBe(subs.length ? subs.join(', ') : '–');
      if (r.tier === 'critical supply partner') expect(cells[col('Single substitutes (any one could replace it)')], `${iso} has no single substitute`).toBe('–');
      if (r.member && r.substitutesNeeded !== undefined) expect(cells[col('Economies needed to replace it')], iso).toBe(r.substitutesNeeded === null ? 'none suffices' : String(r.substitutesNeeded));
      if (typeof r.looCritical === 'boolean') expect(cells[col('Needed with the coalition fixed')], iso).toBe(r.looCritical ? 'yes' : 'no');
    }
    noAcronyms();
  });

  it('a compact whose greedy coalition differs shows it beside the exact one; an unreached middle target is never called exact', async () => {
    const differs = doc.compacts.find((c) => (c.coalition.greedy ?? []).length && c.coalition.greedy!.join() !== c.coalition.members.join());
    if (differs) {
      renderAt(`/compacts/${differs.id}`);
      expect(await h1(differs.name)).toBeTruthy();
      const g = screen.getByTestId('greedy-coalition').textContent ?? '';
      expect(g).toMatch(/^A greedy heuristic .* reaches the 95% target with \d+ economies: .* It is a different coalition/);
      // a greedy set of the same size: the tie-break is the exact program's, never credited to the heuristic
      expect(g).not.toMatch(/same size: among/);
      if (differs.coalition.greedy!.length === differs.coalition.members.length && /^exact/i.test(differs.coalition.method ?? '')) {
        expect(g).toMatch(
          differs.coalition.robustTieBreakSolved === false
            ? /of the same size; the exact program's tie-break among coalitions of that size hit its time limit/
            : /of the same size; among coalitions of that size the exact program keeps the one with the widest margin/,
        );
      }
      cleanup();
    }
    const unreached = doc.compacts.find((c) => !c.coalition.t95.length && c.coalition.method);
    if (unreached) {
      renderAt(`/compacts/${unreached.id}`);
      expect(await h1(unreached.name)).toBeTruthy();
      const card = screen.getByRole('region', { name: 'The coalition and its tiers' });
      // the members are the widest greedy set, never the exact minimum; no member is called critical
      const n = unreached.coalition.members.length || unreached.roles.filter((r) => r.member).length;
      expect(within(card).getByText(new RegExp(`^Widest coalition found \\(the 95% target is not reached\\): ${n} econom`))).toBeTruthy();
      expect(within(card).queryByText(/exact minimum coalition/i)).toBeNull();
      expect(within(card).getByTestId('coalition-method').textContent).toMatch(/the members are the widest coalition the greedy path found .* none is called a critical supply partner/);
      const tiers = within(card).getByTestId('coalition-tiers');
      expect(within(tiers).getByText(/^No coalition reaches the 95% target: the members are the widest coalition/)).toBeTruthy();
      expect(within(tiers).queryByText('Critical supply partners')).toBeNull();
      noAcronyms();
    }
  });

  it('a compact: policy needs, the visible physical-capability warning, the growth flag, the variants and the N-1 readings of each node', async () => {
    const c = doc.compacts.find((x) => x.nodes.some((id) => doc.nodes.find((n) => n.id === id)!.warning)) ?? first;
    renderAt(`/compacts/${c.id}`);
    expect(await h1(c.name)).toBeTruthy();
    const ns = c.nodes.map((id) => doc.nodes.find((n) => n.id === id)!);
    // the compact-level notice names the warned nodes
    const warned = ns.filter((n) => n.warning);
    if (warned.length) {
      const notice = screen.getByTestId('compact-physical-warnings').closest('[role="status"]') as HTMLElement;
      expect(notice.textContent).toContain('PHYSICAL CAPABILITY NOT VALIDATED');
      for (const n of warned) expect(notice.textContent).toContain(n.label);
    }
    const card = screen.getByRole('region', { name: "Each node's coverage and treatment" });
    for (const n of ns) {
      const item = within(card).getByRole('heading', { level: 4, name: n.label }).closest('li') as HTMLElement;
      if (n.policyNeeds) expect(within(item).getByTestId('policy-needs').textContent).toBe(`Policy needs: ${n.policyNeeds.length ? n.policyNeeds.join(' + ') : 'none'}`);
      // the warning is a visible tag with the physical validation beside it, not only a tooltip
      if (n.warning) {
        expect(within(item).getByTestId('physical-warning').textContent).toContain(n.warning);
        expect(within(item).getAllByText(n.physicalValidation!).length).toBeGreaterThan(0);
      } else expect(within(item).queryByTestId('physical-warning')).toBeNull();
      if (n.physicalValidation) expect(within(item).getByTestId('physical-validation').textContent).toContain(n.physicalValidation);
      if (n.coordinateSensitiveToGrowth) {
        expect(within(item).getByTestId('growth-flag').textContent).toMatch(new RegExp(`^Headline ${n.cls}; .*: the class depends on the existing-relationship growth bound\\.$`));
      } else expect(within(item).queryByTestId('growth-flag')).toBeNull();
      if (n.variants) {
        const line = within(item).getByTestId('constraint-classes');
        expect(line.textContent).toMatch(/^production-constrained .*; trade-only /);
        // the warning beside each constraint class that carries it
        const vw = n.variantWarnings && !Array.isArray(n.variantWarnings) ? n.variantWarnings : {};
        const want = ['production_constrained', 'trade_only'].filter((k) => vw[k] === true && n.variants![k] !== null).length;
        expect(within(line).queryAllByTestId('variant-physical-warning'), n.id).toHaveLength(want);
      }
      if (typeof n.n1Reoptimized === 'number') expect(within(item).getByTestId('node-n1').textContent).toContain(`N-1 re-optimized ${(n.n1Reoptimized * 100).toFixed(1)}%`);
      // the growth-bound table: one row per bound, the headline marked
      if (n.growth?.length) {
        const t = within(item).getByRole('table', { name: /existing relationships under each growth bound/ });
        expect(within(t).getAllByRole('row')).toHaveLength(n.growth.length + 1);
        expect(t.textContent).toContain('2x (up to 2× its observed size), headline');
      }
      if (n.variants) {
        const t = within(item).getByRole('table', { name: /the class under each variant/ });
        expect(t.textContent).toContain('Trade-only');
        // every flagged variant's class carries the warning tag in its row
        const vw = n.variantWarnings && !Array.isArray(n.variantWarnings) ? n.variantWarnings : {};
        for (const tr of within(t).getAllByRole('row').slice(1)) {
          const setting = within(tr).getAllByRole('cell')[1].textContent ?? '';
          if (/Trade-only/.test(setting)) expect(within(tr).queryAllByTestId('variant-physical-warning'), `${n.id} trade-only`).toHaveLength(vw.trade_only ? 1 : 0);
        }
        expect(within(t).queryAllByTestId('variant-physical-warning').length, n.id).toBeGreaterThanOrEqual(Object.values(vw).filter(Boolean).length);
      }
      if (n.growth?.length) {
        const t = within(item).getByRole('table', { name: /existing relationships under each growth bound/ });
        expect(within(t).queryAllByTestId('variant-physical-warning'), n.id).toHaveLength(n.growth.filter((g) => g.warning === true && g.cls !== null).length);
      }
    }
    noAcronyms();
  });

  it('a compact: a node whose headline class is validated still shows the warning beside a variant class that carries it', async () => {
    const c = doc.compacts.find((x) =>
      x.nodes.some((id) => {
        const n = doc.nodes.find((y) => y.id === id)!;
        return !n.warning && Object.values(n.variantWarnings && !Array.isArray(n.variantWarnings) ? n.variantWarnings : {}).some(Boolean);
      }),
    );
    if (!c) return;
    const n = c.nodes.map((id) => doc.nodes.find((y) => y.id === id)!).find((y) => !y.warning && Object.values(y.variantWarnings ?? {}).some(Boolean))!;
    renderAt(`/compacts/${c.id}`);
    expect(await h1(c.name)).toBeTruthy();
    const card = screen.getByRole('region', { name: "Each node's coverage and treatment" });
    const item = within(card).getByRole('heading', { level: 4, name: n.label }).closest('li') as HTMLElement;
    expect(within(item).queryByTestId('physical-warning')).toBeNull();
    expect(within(item).getAllByTestId('variant-physical-warning').length).toBeGreaterThan(0);
    expect(within(item).getByText(/^Class under other assumptions and growth bounds/).textContent).toMatch(/physical capability not validated under \d+ other setting/);
    // the node table twin names the settings
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    expect(within(card).getByRole('columnheader', { name: 'Warning under other settings' })).toBeTruthy();
    noAcronyms();
  });

  it('an older bundle without the methodology fields still renders the landing and every compact', async () => {
    useSource((d) => void stripMethodology(d));
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(screen.getByText(/Candidate universe:/)).toBeTruthy();
    expect(screen.queryByRole('checkbox', { name: 'Include nodes outside the strategic scope' })).toBeNull();
    expect(screen.queryAllByTestId('physical-warning')).toEqual([]);
    expect(screen.queryAllByTestId('policy-needs')).toEqual([]);
    noAcronyms();
    cleanup();
    for (const c of doc.compacts) {
      renderAt(`/compacts/${c.id}`);
      expect(await h1(c.name)).toBeTruthy();
      expect(screen.getByRole('region', { name: 'The coalition and its tiers' })).toBeTruthy();
      expect(screen.queryByTestId('growth-flag')).toBeNull();
      expect(screen.queryByTestId('greedy-coalition')).toBeNull();
      expect(openTwins()).toBe(11);
      noAcronyms();
      cleanup();
    }
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    expect(screen.getByTestId('about-scope').textContent).toMatch(/This export predates the split/);
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
    await waitFor(() => expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(AIB_VIEWS));
    noAcronyms();
  });

  it('about: demand scope vs candidate partners, hold vs release, the supply constraint, the warning rule, exact vs greedy and the tiers, the two N-1 readings, the growth bound and the strategic scope', async () => {
    if (!doc.meta.demandScope) return;
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    for (const h of [
      'Demand scope and candidate partners', 'Shock semantics: hold and release', 'Supply constraint: production-constrained and trade-only', 'Policy needs and the physical-capability warning',
      'Coalitions: exact, greedy and the tiers', 'Resilience: true and instantaneous N-1', 'The existing-relationship growth bound', 'Strategic scope',
    ]) {
      expect(screen.getByRole('heading', { level: 2, name: h })).toBeTruthy();
    }
    const scope = screen.getByTestId('about-scope').textContent ?? '';
    expect(scope).toContain(
      `the demand scope is the allied core and partner economies of the coalition table (${doc.meta.nDemandScope} economies), not a compact's coalition: the importers whose lost supply is restored`,
    );
    expect(scope).toContain('the candidate partners are global: any economy in the trade data may add or redirect supply');
    expect(screen.getByText('Hold (headline)')).toBeTruthy();
    expect(screen.getByText('Release (sensitivity)')).toBeTruthy();
    expect(screen.getByText('Production-constrained (headline)')).toBeTruthy();
    expect(screen.getByText('Trade-only (sensitivity)')).toBeTruthy();
    const nWarned = doc.nodes.filter((n) => n.warning).length;
    expect(screen.getByTestId('about-warning').textContent).toMatch(new RegExp(`marks a LOW INTERVENTION or COORDINATE class .* In this export it marks ${nWarned} node`));
    for (const t of ['Core coordinating members', 'Critical supply partners', 'Associate partners', 'Critical external dependencies', 'Critical external dependencies (marginal)', 'External dependencies']) {
      expect(screen.getByText(t)).toBeTruthy();
    }
    // the final rules in words: production-constrained decides who can supply, never a trade value; the tie-break; single substitutes
    const page = main().textContent ?? '';
    expect(page).toContain('physical evidence decides who can supply');
    expect(page).not.toMatch(/production share × world trade|larger of what it already ships/);
    expect(page).toContain('the widest margin over the target in its weakest node, then the one that adds the most supply');
    expect(page).toContain("A member's substitutes are single substitutes: any one of them");
    expect(page).toMatch(/A node that is COORDINATE under some bounds and another class under others is flagged on the landing/);
    const nOut = doc.nodes.filter((n) => n.inStrategicScope === false).length;
    expect(screen.getByTestId('about-strategic').textContent).toContain(`In this export ${nOut} of ${doc.nodes.length} nodes are outside the scope`);
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
    expect(within(tabs()).getByRole('link', { name: 'Allied Industrial Base' }).getAttribute('aria-current')).toBe('true');
    expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(AIB_VIEWS);
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

  it('country roles (IA refactor): the role matrix in the brief order, one row per economy with its anchor, each linking to its Country profile; Country roles current', async () => {
    renderAt('/aib/roles#role-' + doc.compacts[0].coalition.members[0]);
    expect(await h1('Country roles')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Country roles' }).getAttribute('aria-current')).toBe('page'));
    const iso = doc.compacts[0].coalition.members[0];
    const link = within(main()).getAllByRole('link').find((a) => a.getAttribute('href') === `/countries/${iso}#aib-role`);
    expect(link).toBeTruthy();
    const matrix = screen.getByRole('region', { name: 'Roles by economy' });
    expect(within(matrix).getAllByRole('columnheader').map((h) => h.textContent)).toEqual([
      'Economy', 'Resource supplier', 'Specialized supplier', 'Investment host', 'Demand anchor', 'Bridge', 'Technology partner', 'Other supplier', 'Compact participation',
    ]);
    const row = document.getElementById(`role-${iso}`) as HTMLElement;
    expect(row.className).toContain('bg-signal/10');
    expect(within(row).getAllByRole('link').some((a) => a.getAttribute('href') === `/compacts/${doc.compacts[0].id}`)).toBe(true);
    expect(within(main()).getByText('Observed / modelled facts')).toBeTruthy();
    noAcronyms();
    expect(openTwins()).toBe(1);
    noAcronyms();
  });

  it('overview (IA refactor): node counts by treatment, the highest severity and dividends, the three question cards; never starts the engine', async () => {
    setEngineClient(null);
    renderAt('/aib');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Overview' }).getAttribute('aria-current')).toBe('page'));
    const tiles = screen.getByRole('list', { name: 'Nodes by treatment' });
    const items = within(tiles).getAllByRole('listitem');
    ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION'].forEach((cls, i) => {
      const n = doc.nodes.filter((x) => x.cls === cls).length;
      expect(items[i].textContent).toContain(cls);
      expect(items[i].textContent).toContain(`${n} ${n === 1 ? 'node' : 'nodes'}`);
    });
    expect(within(screen.getByRole('list', { name: 'Nodes with the highest shock exposure' })).getAllByRole('listitem').length).toBeGreaterThan(0);
    expect(within(screen.getByRole('list', { name: 'Nodes with the largest coordination dividend' })).getAllByRole('listitem').length).toBeGreaterThan(0);
    const hrefs = within(screen.getByRole('region', { name: 'Who coordinates, and how' })).getAllByRole('link').map((a) => a.getAttribute('href'));
    expect(hrefs).toEqual(['/aib/partnerships', '/compacts', '/aib/roles']);
    expect(within(main()).getAllByText('Observed / modelled facts').length).toBeGreaterThan(0);
    expect(peekEngineClient()).toBeNull();
    noAcronyms();
  });

  it('partnerships (IA refactor): Trade by default with the coordinated relationships; the toggle and the filters write the URL; Investment and Development', async () => {
    setEngineClient(neverReadyClient());
    const r = renderAt('/aib/partnerships');
    expect(await h1('Partnerships')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Partnerships' }).getAttribute('aria-current')).toBe('page'));
    const trade = screen.getByRole('region', { name: 'Coordinated trade relationships' });
    const edge = doc.compacts.flatMap((c) => c.trade).find((e) => e.usd && e.usd > 0 && e.from !== e.to)!;
    // the Sankey draws once its width is measured: wait for it (slow under a loaded full run)
    expect(await within(trade).findByRole('img', { name: /exporter to importer/ }, { timeout: 15_000 })).toBeTruthy();
    expect(screen.getByRole('region', { name: 'Specialized suppliers' })).toBeTruthy();
    expect(screen.getAllByText('Observed / modelled facts').length).toBeGreaterThan(0);
    expect(screen.getAllByText(/Preference-adjusted priorities · NEIS baseline/).length).toBeGreaterThan(0);
    // the economy filter
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: edge.from } });
    await waitFor(() => expect(r.state.location.search).toBe(`?country=${edge.from}`));
    fireEvent.click(within(trade).getByRole('button', { name: 'Table' }));
    for (const a of within(trade).getAllByRole('link').filter((x) => x.getAttribute('href')?.endsWith('#partnerships'))) expect(a.getAttribute('href')).toMatch(/^\/countries\/[A-Z]{3}#partnerships$/);
    // Investment
    fireEvent.click(screen.getByRole('radio', { name: 'Investment' }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('mode')).toBe('investment'));
    expect(await screen.findByRole('region', { name: 'Where new capacity buys most resilience' })).toBeTruthy();
    expect(screen.getByRole('region', { name: 'Investment hosts' })).toBeTruthy();
    // Development: the capacity gaps (facts) without waiting for the engine
    fireEvent.click(screen.getByRole('radio', { name: 'Development' }));
    expect(await screen.findByRole('region', { name: 'Capacity gaps' })).toBeTruthy();
    noAcronyms();
    setEngineClient(null);
  });

  it('a compact deep link to a method anchor opens the method details', async () => {
    renderAt(`/compacts/${first.id}#coalition`);
    expect(await h1(first.name)).toBeTruthy();
    expect((screen.getByText(/^Method details:/).closest('details') as HTMLDetailsElement).open).toBe(true);
  });

  it('/aib/<unknown> goes to the overview with a notice', async () => {
    const router = renderAt('/aib/nothing');
    await waitFor(() => expect(router.state.location.pathname).toBe('/aib'), { timeout: 30_000 });
    expect(await screen.findByText(/That Allied Industrial Base page does not exist/, undefined, { timeout: 30_000 })).toBeTruthy();
  });

  it('a newer export asks for a reload', async () => {
    useFailing({ manifest: async () => ({ ...readManifest(), version: 2 }), json: async () => ({}) });
    renderAt('/compacts');
    expect(await screen.findByText('Please reload', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Reload' })).toBeTruthy();
  });
});

describe('Home', () => {
  it('has the third entry card: the Allied Industrial Base, its links and its button', async () => {
    expect(MODULE_CARDS.map((c) => c.id)).toEqual(['sector', 'country', 'aib']);
    const card = MODULE_CARDS[2];
    expect(card).toMatchObject({ title: 'Explore the Allied Industrial Base', button: 'Explore AIB' });
    expect(card.links.every((l) => l.to.startsWith('/compacts') || l.to.startsWith('/aib'))).toBe(true);
  });
});
