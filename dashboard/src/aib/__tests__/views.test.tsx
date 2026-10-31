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
// screens: no data, a broken document (Retry), a newer export (Reload); Home's fourth card; the diversification
// scenario (2026-10-19): its page (totals, the chart, the table with its toggle and filters, the links), the
// overview pointer, About, the node block per status on a compact page and the landing, "not run" in the
// variant tables, and an older bundle without it.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { MODULE_CARDS } from '../../shell/Home.tsx';
import { AIB_PHYSICAL_WARNING, AIB_VERSION, countryNamesOf, diversifyShockIdOf, variantsOf, type AibCompact, type AibDoc, type AibNode } from '../contract.ts';
import { diversifyRow, diversifyRows, diversifyTotals, diversifyWords, hasDiversify, orderDiversify, rowStatusLabel, totalsWords } from '../derive/diversify.ts';
import { countryRolesOf, everyShock, everyShockLabel, fmtPts, scenarioRoleSource } from '../derive/network.ts';
import { hasScenarios } from '../derive/scenarios.ts';
import { HHI_SHORT, HHI_WORDS } from '../derive/settings.ts';
import { CHART_ROWS, MI_QUESTION, MI_TABLE_TITLE } from '../views/AibDiversifyView.tsx';
import { BASIS_WORDS } from '../views/basis.tsx';
import { classInfo, parseClassMix, policyNeedsWords } from '../derive/classes.ts';
import { MAP_CARD_TITLE, MAP_SECTION_TITLE } from '../views/CompactsIndexView.tsx';
import { COALITION_TITLE, COMPACT_VS_TODAY, NO_COMPACT_NEEDED, ROLES_TITLE } from '../views/compact/OverviewSections.tsx';
import { COVERAGE_SECTION_TITLE } from '../views/compact/ResilienceSections.tsx';
import { NODE_SECTION_TITLE } from '../views/compact/NodeSection.tsx';
import { ROLES_PAGE_TITLE } from '../views/AibRolesView.tsx';
import { AIB_CLASS_HEADING, AIB_MEASURE_DEFINITION, COMPACT, STATE } from '../../content/vocabulary.ts';
import { cellLabel } from '../../lib/format.ts';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import { AibLoadError, AibStore, setAibStoreForTests, type AibSource } from '../data/store.ts';
import { compactTitle, fmtUsd, listWords } from '../derive/format.ts';
import { osiCellPick } from '../derive/partnerships.ts';
import {
  cellStagesOfNodes, NOT_IN_BASKET, nodeInCell, nodeInTech, nodeStagesOf, nodeWhere, osiTechsOfNode, profiledCellsOfNode, taxonomyOf, techKeysOfNode, technologiesOfNodes,
} from '../derive/taxonomy.ts';
import { resetLastCompact } from '../nav.ts';
import { peekEngineClient, setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, readManifest, stripDiversify, stripMethodology, toMasterTaxonomy } from './fixture.ts';

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
const AIB_VIEWS = ['Overview', 'Partnerships', 'Sectoral Compacts', 'Country roles', 'Managed Interdependence'];

/** Open every table twin of the page; the number of cards that have one. */
function openTwins(): number {
  const buttons = within(main()).queryAllByRole('button', { name: 'Table' });
  for (const b of buttons) fireEvent.click(b);
  return buttons.length;
}

describe.skipIf(!haveFixture())('Sectoral Compacts routes', () => {
  const doc = readDoc();
  const first = doc.compacts[0];
  const tx = taxonomyOf(doc.meta);
  // the page titles come from the compacts' structured fields (never the exported name)
  const titleOf = (c: AibCompact, d: AibDoc = doc) => compactTitle(c, d.nodes);
  // the bundle in its master (OSI basket) form: a no-op on a master bundle
  const docM = toMasterTaxonomy(structuredClone(doc));

  it('landing: the definition, a card per compact with its strip, every node by class; the fourth tab is current', async () => {
    renderAt('/compacts/workstreams?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(/^Workstreams \| Allied Industrial Base \| Allied Industrial Base Explorer/));
    // the page's question first, then the plain definition (the NEIS copy refresh)
    expect(screen.getByTestId('page-question').textContent).toMatch(/act together\?$/);
    expect(screen.getByText(/groups of related supply-chain steps that need similar partners/)).toBeTruthy();
    expect(screen.getByText(/come out of the analysis, a stress test of today’s trade network, not a fixed list/)).toBeTruthy();
    // the Allied Industrial Base tab, current; the views nav the module registered (IA refactor)
    const t = within(tabs()).getAllByRole('link');
    expect(t.map((a) => a.textContent)).toEqual(['Sectors', 'Countries', 'Allied Industrial Base', 'About']);
    expect(t[2].getAttribute('aria-current')).toBe('true');
    await waitFor(() => expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(AIB_VIEWS));
    expect(within(views()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page');
    // the cards
    const region = screen.getByRole('region', { name: `The workstreams (${doc.compacts.length})` });
    const cards = within(region).getAllByRole('article');
    expect(cards).toHaveLength(doc.compacts.length);
    expect(within(cards[0]).getByRole('link', { name: titleOf(first) }).getAttribute('href')).toBe(`/compacts/${first.id}?scn=decoupling`);
    // the slim card (IA refactor): only why it exists, main nodes, key countries, the policy need, the resilience effect, View compact
    expect(within(cards[0]).getAllByRole('term').map((t) => t.textContent)).toEqual(['Why it exists', 'Main steps', 'Key countries', 'Coalition', 'Policy need', 'Resilience effect']);
    expect(within(cards[0]).getByRole('link', { name: `View compact workstream: ${titleOf(first)}` }).getAttribute('href')).toBe(`/compacts/${first.id}?scn=decoupling`);
    expect(within(cards[0]).getByRole('list', { name: `${titleOf(first)}: demand served under each scenario` })).toBeTruthy();
    expect(within(cards[0]).getAllByRole('listitem')).toHaveLength(3);
    // the strategic map first, the full table of steps behind "Explore every supply-chain step"
    const map = screen.getByRole('region', { name: MAP_SECTION_TITLE });
    expect(within(map).getByText(BASIS_WORDS.facts.label)).toBeTruthy();
    expect(within(map).getByRole('img', { name: /^Map of / })).toBeTruthy();
    expect(within(map).getByRole('region', { name: MAP_CARD_TITLE })).toBeTruthy();
    const explore = screen.getByText(/^Explore every supply-chain step \(/).closest('details') as HTMLDetailsElement;
    expect(explore.open).toBe(false);
    // the classification overview: every class present has its group; nodes link to their compact
    const nodes = screen.getByRole('region', { name: AIB_CLASS_HEADING });
    // each treatment in plain words, never the data's upper-case code
    for (const cls of new Set(doc.nodes.map((n) => n.cls))) expect(within(nodes).getAllByText(classInfo(cls).label).length).toBeGreaterThan(0);
    const linked = doc.nodes.find((n) => n.compact)!;
    expect(within(nodes).getAllByRole('link', { name: linked.compact! }).length).toBeGreaterThan(0);
    expect(main().querySelectorAll('figure svg').length).toBeGreaterThan(0);
    // the class legend: all five classes, each with its own mark (three hues, hollow, hatched)
    const legend = within(nodes).getByRole('list', { name: 'What each supply-chain step needs' });
    expect(within(legend).getAllByRole('listitem').map((li) => li.textContent)).toEqual(['Coordinate trade', 'Build capacity', 'Manage specialization', 'Low intervention', 'Not enough evidence']);
    const marks = within(legend).getAllByRole('listitem').map((li) => (li.querySelector('span[aria-hidden]') as HTMLElement).getAttribute('style'));
    expect(new Set(marks).size).toBe(5);
    noAcronyms();
    expect(openTwins()).toBe(3);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(3);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(3);
    noAcronyms();
    // the document is loaded once, with the manifest
    expect(source.calls).toEqual(['manifest.json', readManifest().files.compacts]);
  });

  it('landing: the OSI technology and stage filters write tech and stage and narrow the nodes (of the strategic scope)', async () => {
    const r = renderAt('/compacts/workstreams?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    const inScope = doc.nodes.filter((x) => x.inStrategicScope !== false);
    const nOut = doc.nodes.length - inScope.length;
    const scopeWords = nOut ? ` in the strategic scope (${nOut} outside it not shown)` : '';
    // a technology a node shares with another (a node in several technologies is counted under each)
    const tech = technologiesOfNodes(inScope).find((t) => t !== NOT_IN_BASKET && inScope.some((x) => nodeInTech(x, t) && osiTechsOfNode(x).length > 1)) ?? technologiesOfNodes(inScope)[0];
    fireEvent.change(screen.getByRole('combobox', { name: 'Technology' }), { target: { value: tech } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('tech')).toBe(tech));
    const inTech = inScope.filter((x) => nodeInTech(x, tech));
    expect(screen.getByText(`${inTech.length} ${inTech.length === 1 ? 'supply-chain step' : 'supply-chain steps'} of ${inScope.length}${scopeWords}`)).toBeTruthy();
    // the stage choices are the stages of that technology's cells, in chain order (never node.stage)
    const stageSel = screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement;
    expect(Array.from(stageSel.options).map((o) => o.value)).toEqual(['', ...cellStagesOfNodes(inScope, tech)]);
    const stage = nodeStagesOf(inTech[0], tech)[0];
    fireEvent.change(stageSel, { target: { value: stage } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('stage')).toBe(stage));
    const both = inScope.filter((x) => nodeInCell(x, tech, stage)).length;
    expect(screen.getByText(`${both} ${both === 1 ? 'supply-chain step' : 'supply-chain steps'} of ${inScope.length}${scopeWords}`)).toBeTruthy();
    // a dictionary sector or a three-stage stage in an old link matches no option and is dropped
    cleanup();
    renderAt('/compacts/workstreams?scn=decoupling&tech=Magnets&stage=Midstream');
    expect(await h1('Workstreams')).toBeTruthy();
    expect((screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement).value).toBe('');
    expect((screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement).value).toBe('');
    expect(screen.getByText(`${inScope.length} supply-chain steps of ${inScope.length}${scopeWords}`)).toBeTruthy();
  });

  it('landing: nodes by OSI technology x stage (2026-10-29): the Technology options, each card names its technologies and stages, nodes say technologies · stage, the table has Node, Technologies, Stage and OSI cells', async () => {
    renderAt('/compacts#nodes');
    expect(await h1('Workstreams')).toBeTruthy();
    const select = screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement;
    // "All technologies", then the OSI technologies of the nodes' cells (no index groups), the basket-less last
    const pool = doc.nodes.filter((n) => n.inStrategicScope !== false);
    expect(Array.from(select.options).map((o) => o.value)).toEqual(['', ...technologiesOfNodes(pool)]);
    expect(select.options[0].textContent).toBe('All technologies');
    expect(select.querySelectorAll('optgroup')).toHaveLength(0);
    expect(screen.queryByRole('combobox', { name: 'Sector' })).toBeNull();
    // no dictionary sector is an option (unless it is also an OSI technology's name)
    const osi = new Set(technologiesOfNodes(pool));
    for (const n of pool) for (const t of n.techs.filter((x) => !osi.has(x))) expect(Array.from(select.options).some((o) => o.value === t), t).toBe(false);
    // the run line names the source of the nodes and how they are shown
    if (tx.dictionary) expect(screen.getByTestId('run-settings').textContent).toMatch(/Supply-chain steps: electro-industrial dictionary( \((core|extended) scope\))?, shown by OSI technology × stage\./);
    // every card names its OSI technologies and the stages of its nodes' cells (none when no node has one)
    for (const c of doc.compacts) {
      const card = screen.getAllByRole('article').find((a) => within(a).queryByRole('link', { name: titleOf(c) }))!;
      const own = c.nodes.map((id) => doc.nodes.find((n) => n.id === id)!).filter(Boolean);
      const stages = cellStagesOfNodes(own);
      expect(within(card).getByTestId('compact-taxonomy').textContent).toBe(
        `Technologies: ${technologiesOfNodes(own).join(', ')}${stages.length ? ` · ${stages.length === 1 ? 'Stage' : 'Stages'}: ${stages.join(', ')}` : ''}`,
      );
    }
    // a node chip: where its cells are (technologies · stage)
    const nodes = screen.getByRole('region', { name: AIB_CLASS_HEADING });
    const n0 = pool.find((n) => osiTechsOfNode(n).length > 1 && doc.nodes.filter((x) => x.label === n.label).length === 1) ?? pool[0];
    const chip = within(nodes).getByText(n0.label).closest('li') as HTMLElement;
    expect(chip.textContent).toContain(nodeWhere(n0));
    const bare = pool.find((n) => !n.yfCells.length && doc.nodes.filter((x) => x.label === n.label).length === 1);
    if (bare) expect((within(nodes).getByText(bare.label).closest('li') as HTMLElement).textContent).toContain(NOT_IN_BASKET);
    if (bare) expect((within(nodes).getByText(bare.label).closest('li') as HTMLElement).textContent).not.toContain(`${NOT_IN_BASKET} · `);
    fireEvent.click(within(nodes).getByRole('button', { name: 'Table' }));
    const heads = within(nodes).getAllByRole('columnheader').map((h) => h.textContent);
    expect(heads.slice(0, 4)).toEqual(['Supply-chain step', 'Technologies', 'Stage', 'OSI cells']);
    expect(heads).not.toContain('Index');
    // no Upstream / Midstream / Downstream, no index anywhere on the page
    expect(main().textContent).not.toMatch(/Upstream|Midstream|Downstream|three-stage|index > sector/);
    noAcronyms();
  });

  it('landing: the run settings; out-of-scope nodes hidden until asked for; policy needs and warnings on the node chips', async () => {
    // a configured strategic scope: the master form of the bundle (the dictionary's scope is "all")
    useSource((d) => void toMasterTaxonomy(d));
    const doc = docM;
    const out = doc.nodes.filter((x) => x.inStrategicScope === false);
    if (!out.length || !doc.meta.demandScope) return;
    const r = renderAt('/compacts/workstreams?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    const run = screen.getByTestId('run-settings');
    // "coalition" demand is the allied core and partner economies, not a compact's coalition
    expect(run.textContent).toMatch(/Whose demand is protected: the allied core and partner economies of the coalition table \(\d+ economies\), not a compact's coalition/);
    expect(run.textContent).toMatch(/Who may supply: any economy in the trade data may add or redirect supply/);
    expect(run.textContent).toMatch(/hold semantics \(the removed supplier keeps buying\), production-constrained supply/);
    expect(run.textContent).toMatch(/Strategic scope: electro-industrial/);
    expect(run.textContent).toMatch(/share of the demand scope's baseline traded demand/);
    const nodes = screen.getByRole('region', { name: AIB_CLASS_HEADING });
    for (const n of out) expect(within(nodes).queryByText(n.label)).toBeNull();
    // the warning is a visible tag on the chip, never only a tooltip
    const warned = doc.nodes.find((x) => x.warning && x.inStrategicScope !== false)!;
    const chip = within(nodes).getByText(warned.label).closest('li') as HTMLElement;
    expect(within(chip).getByTestId('physical-warning').textContent).toContain('PHYSICAL CAPABILITY NOT VALIDATED');
    // policy needs where they say more than the class
    const multi = doc.nodes.find((x) => (x.policyNeeds?.length ?? 0) > 1 && x.inStrategicScope !== false)!;
    const mchip = within(nodes).getByText(multi.label).closest('li') as HTMLElement;
    expect(within(mchip).getByTestId('policy-needs').textContent).toBe(`Actions needed: ${policyNeedsWords(multi.policyNeeds)}`);
    expect(within(mchip).getByTestId('policy-needs').textContent).not.toMatch(/[A-Z]{5,}/);
    noAcronyms();
    // include the nodes outside the scope (the filter sits over the strategic map and drives both): the URL keeps it, they appear marked
    fireEvent.click(within(screen.getByRole('region', { name: MAP_SECTION_TITLE })).getByRole('checkbox', { name: 'Include steps outside the strategic scope' }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('scope')).toBe('all'));
    // (a label can recur across technologies, or name a technology too)
    for (const n of out) {
      const lis = within(nodes).getAllByText(n.label).map((el) => el.closest('li'));
      expect(lis.some((li) => /outside the strategic scope/.test(li?.textContent ?? '')), n.id).toBe(true);
    }
    expect(screen.getByText(`${doc.nodes.length} supply-chain steps of ${doc.nodes.length}`)).toBeTruthy();
    noAcronyms();
  });

  it('landing: the growth-bound flag on the node chips and in the table, worded from the headline class; nodes outside a compact open their variants in place', async () => {
    renderAt('/compacts/workstreams?scope=all&scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    const nodes = screen.getByRole('region', { name: AIB_CLASS_HEADING });
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
    const table = within(nodes).getAllByRole('table').find((x) => within(x).queryByRole('columnheader', { name: 'Treatment depends on how far existing trade can grow' }))!;
    expect(table).toBeTruthy();
    noAcronyms();
  });

  it('a compact: the four questions in order, method details closed, charts drawn, a table twin and a CSV on every card, no bare acronym', async () => {
    renderAt(`/compacts/${first.id}?scn=decoupling`);
    expect(await h1(titleOf(first))).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(new RegExp(`^${first.id} .* \\| Allied Industrial Base \\|`)));
    // the four questions (IA refactor), then the method details (closed) with the members-joining curve and the reading notes
    const h2 = screen.getAllByRole('heading', { level: 2 }).map((h) => h.textContent);
    const order = ['Why this workstream?', COMPACT.sectionTitles.what, COMPACT.sectionTitles.together, COMPACT.sectionTitles.next, 'How resilience changes as countries join', 'Reading these results'];
    expect(h2.filter((h) => order.includes(h ?? ''))).toEqual(order);
    const h3 = screen.getAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    for (const s of [COVERAGE_SECTION_TITLE, 'Partnership network', NODE_SECTION_TITLE, AIB_CLASS_HEADING]) expect(h3).toContain(s);
    expect((screen.getByText(/^Method details:/).closest('details') as HTMLDetailsElement).open).toBe(false);
    expect(screen.getByTestId('why-answer').textContent).toMatch(/^The disruption removes \d+% of the compact's demand/);
    expect(within(main()).getAllByText(BASIS_WORDS.facts.label).length).toBeGreaterThan(0);
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Sectoral Compacts' }).getAttribute('aria-current')).toBe('page'));
    // the role matrix: the tier and the ten roles as columns, an economy per row, its name (never a bare code)
    const roles = screen.getByRole('region', { name: ROLES_TITLE });
    const heads = within(roles).getAllByRole('columnheader').map((h) => h.textContent);
    expect(heads).toEqual(['Economy', 'Coalition tier', 'Demand anchor', 'Specialized supplier', 'Resource supplier', 'Scaling host', 'Specialty host', 'Development host', 'Technology partner', 'Bridge partner', 'Associate partner', 'Supplier']);
    expect(within(roles).getAllByRole('rowheader').length).toBe(new Set(first.roles.map((x) => x.iso3)).size);
    // each economy opens its Country profile at "Role in the Allied Industrial Base"
    const iso = first.roles[0].iso3;
    expect(within(roles).getAllByRole('link').some((a) => a.getAttribute('href') === `/countries/${iso}?scn=decoupling#aib-role`)).toBe(true);
    // why it exists: the five tiles
    const why = screen.getByRole('region', { name: 'The case for coordination' });
    for (const t of ['Supply exposed to the disruption', 'Coordination dividend', 'Remaining gap after trade', 'Remaining gap after new capacity', 'Concentration']) expect(within(why).getByText(t)).toBeTruthy();
    // node by node: one item per node, its class, the data-quality disclosure
    const nodes = screen.getByRole('region', { name: "Each step's coverage and treatment" });
    expect(within(nodes).getAllByRole('heading', { level: 4 }).map((h) => h.textContent)).toEqual(first.nodes.map((id) => doc.nodes.find((n) => n.id === id)!.label));
    expect(within(nodes).getAllByText(/^Data quality:/).length).toBe(first.nodes.length);
    // the charts: Sankeys, the coverage bars and the coalition curve all draw SVG
    expect(within(screen.getByRole('region', { name: 'Trade relationships' })).getByRole('img', { name: /exporter to importer/ })).toBeTruthy();
    // who invests where (the investment simulation's flows for the compact's nodes; the older compacts' card without them)
    const firstFlows = (doc.investmentFlows?.pairs ?? []).filter((p) => p.rank === 1 && first.nodes.includes(p.node));
    if (doc.investmentFlows && firstFlows.length) {
      const inv = screen.getByRole('region', { name: 'Who invests where' });
      expect(within(inv).getByRole('img', { name: /^Investor to host flows/ })).toBeTruthy();
      expect(within(inv).getByTestId('modelled-note').textContent).toMatch(/does not move with the Perspective dials/);
      expect(screen.getByTestId('investment-step').textContent).toMatch(/^Trade [+] investment: where new capacity would help most/);
    } else if (first.investment.length) expect(within(screen.getByRole('region', { name: 'Investment relationships' })).getByRole('img', { name: /investor to host/ })).toBeTruthy();
    expect(within(screen.getByRole('region', { name: 'Demand served at each step' })).getAllByRole('img').length).toBeGreaterThan(0);
    expect(within(screen.getByRole('region', { name: 'Coverage as members join' })).getAllByRole('img').length).toBeGreaterThan(0);
    expect(main().querySelectorAll('figure svg').length).toBeGreaterThanOrEqual(5);
    noAcronyms();
    const cards = openTwins();
    expect(cards).toBe(12);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(12);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(cards);
    noAcronyms();
  });

  it('every compact renders its four questions and opens its table twins without a bare acronym', async () => {
    for (const c of doc.compacts) {
      renderAt(`/compacts/${c.id}?scn=decoupling`);
      expect(await h1(titleOf(c))).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: COMPACT.sectionTitles.next })).toBeTruthy();
      expect(screen.getByRole('heading', { level: 2, name: 'How resilience changes as countries join' })).toBeTruthy();
      noAcronyms();
      expect(openTwins()).toBe(12);
      noAcronyms();
      // the summaries, captions and aria-labels name the compact by its display title, never by the exported
      // name (built from the dictionary sectors: provenance only, design A1)
      if (c.name !== titleOf(c)) {
        const said = [document.body.textContent ?? '', ...[...document.querySelectorAll('[aria-label], [title]')].flatMap((e) => [e.getAttribute('aria-label') ?? '', e.getAttribute('title') ?? ''])];
        expect(said.filter((s) => s.includes(c.name))).toEqual([]);
        expect(screen.getAllByRole('table', { name: `${titleOf(c)}: node by node` }).length).toBe(1);
      }
      cleanup();
    }
  });

  it('a compact: the type, the class mix and secondary class, compact vs fragmented action and the global optimum', async () => {
    renderAt(`/compacts/${first.id}?scn=decoupling`);
    expect(await h1(titleOf(first))).toBeTruthy();
    const design = screen.getByRole('region', { name: "The supply-chain steps covered and the compact's design" });
    if (first.classMix) {
      const mix = within(design).getByTestId('class-mix');
      // each treatment in plain words with its share (the data's codes are the keys)
      for (const part of parseClassMix(first.classMix)) expect(mix.textContent).toContain(`${classInfo(part.cls).label} ${Math.round(part.share * 100)}%`);
    }
    if (first.secondaryClass) expect(within(design).getByTestId('secondary-class').textContent).toContain(classInfo(first.secondaryClass).label);
    const why = screen.getByRole('region', { name: 'The case for coordination' });
    const m = first.metrics;
    if (typeof m.anchorFragmented === 'number' && typeof m.anchorCompact === 'number') {
      expect(within(why).getByText(COMPACT_VS_TODAY)).toBeTruthy();
      expect(within(why).getByText(new RegExp(`${(m.anchorFragmented * 100).toFixed(1)}% through existing relationships, ${(m.anchorCompact * 100).toFixed(1)}% with`))).toBeTruthy();
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
    renderAt('/compacts/workstreams?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    const card = screen.getAllByRole('article').find((a) => within(a).queryByRole('link', { name: titleOf(u) }))!;
    const tag = within(card).getByText(NO_COMPACT_NEEDED);
    expect(tag.getAttribute('data-tone')).toBe('unnecessary');
    expect(within(card).getByText(/Existing relationships do nearly as well/)).toBeTruthy();
    cleanup();
    renderAt(`/compacts/${u.id}?scn=decoupling`);
    expect(await h1(titleOf(u))).toBeTruthy();
    expect(screen.getAllByText(NO_COMPACT_NEEDED).length).toBeGreaterThanOrEqual(2);
    expect(screen.getByRole('region', { name: 'Why this workstream?' }).textContent).toMatch(/existing relationships, not a new coalition, carry most of the response/);
    noAcronyms();
  });

  it('the specialization-resilience frontier: a node selector, the readings, a table twin and a CSV', async () => {
    renderAt(`/compacts/${first.id}?scn=decoupling`);
    expect(await h1(titleOf(first))).toBeTruthy();
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
      'Instantaneous N-1 retained coverage (coverage × (1 − largest share))', 'Specialization score', 'Largest supplier after coordinated trade', 'Its share',
    ]);
    expect(within(table).getAllByRole('row')).toHaveLength(target.frontier.length + 1);
    noAcronyms();
  });

  it('a compact: the exact minimum coalition, the method and tie-break, the tiers with what would replace a member', async () => {
    renderAt(`/compacts/${first.id}?scn=decoupling`);
    expect(await h1(titleOf(first))).toBeTruthy();
    const card = screen.getByRole('region', { name: COALITION_TITLE });
    if (!first.coalition.method) return;
    expect(within(card).getByText(`Exact minimum coalition: ${first.coalition.members.length} countries. ${AIB_MEASURE_DEFINITION.minimumCoalition} Method: ${first.coalition.method}.`)).toBeTruthy();
    const lead = within(card).getByTestId('coalition-method').textContent ?? '';
    expect(lead).toMatch(/^The members are the smallest group of partners that meets the target: the fewest countries/);
    // the tie-break among minimum-size coalitions, and whether it was solved
    expect(lead).toContain('among coalitions of that size the exact search keeps the one with the widest margin over the target at its weakest step, then the one that adds the most supply');
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
    // an economy shows in one tier only: one that is also an associate (e.g. a development host the
    // extended coalition adds) is listed there, not among the marginal external dependencies
    const marginal = (first.coalition.criticalExternalMarginal ?? []).filter((iso) =>
      first.roles.filter((r) => r.iso3 === iso).every((r) => r.tier === undefined || r.tier === null || r.tier === 'critical external dependency (marginal)'));
    if (marginal.length) {
      expect(within(tiers).getByText('Critical external dependencies (marginal)')).toBeTruthy();
      const inline = within(tiers).getByTestId('inline-tier');
      for (const iso of marginal) expect(inline.textContent).toContain(nm(iso));
    }
    // the role matrix carries each economy's tier; externals are not members and never read as associates
    const roles = screen.getByRole('region', { name: ROLES_TITLE });
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
      renderAt(`/compacts/${differs.id}?scn=decoupling`);
      expect(await h1(titleOf(differs))).toBeTruthy();
      const g = screen.getByTestId('greedy-coalition').textContent ?? '';
      expect(g).toMatch(/^A greedy heuristic .* reaches the 95% target with \d+ economies: .* It is a different coalition/);
      // a greedy set of the same size: the tie-break is the exact program's, never credited to the heuristic
      expect(g).not.toMatch(/same size: among/);
      if (differs.coalition.greedy!.length === differs.coalition.members.length && /^exact/i.test(differs.coalition.method ?? '')) {
        expect(g).toMatch(
          differs.coalition.robustTieBreakSolved === false
            ? /of the same size; the exact program's tie-break among coalitions of that size hit its time limit/
            : /of the same size; among coalitions of that size the exact search keeps the one with the widest margin/,
        );
      }
      cleanup();
    }
    const unreached = doc.compacts.find((c) => !c.coalition.t95.length && c.coalition.method);
    if (unreached) {
      renderAt(`/compacts/${unreached.id}?scn=decoupling`);
      expect(await h1(titleOf(unreached))).toBeTruthy();
      const card = screen.getByRole('region', { name: COALITION_TITLE });
      // the members are the widest greedy set, never the exact minimum; no member is called critical
      const n = unreached.coalition.members.length || unreached.roles.filter((r) => r.member).length;
      expect(within(card).getByText(new RegExp(`^Widest coalition found \\(the 95% target is not reached\\): ${n} countr`))).toBeTruthy();
      expect(within(card).queryByText(/exact minimum coalition/i)).toBeNull();
      expect(within(card).getByTestId('coalition-method').textContent).toMatch(/the members are the widest coalition found by adding partners one at a time .* none is called a critical supply partner/);
      const tiers = within(card).getByTestId('coalition-tiers');
      expect(within(tiers).getByText(/^No coalition reaches the 95% target: the members are the widest coalition/)).toBeTruthy();
      expect(within(tiers).queryByText('Critical supply partners')).toBeNull();
      noAcronyms();
    }
  });

  it('a compact: policy needs, the visible physical-capability warning, the growth flag, the variants and the N-1 readings of each node', async () => {
    const c = doc.compacts.find((x) => x.nodes.some((id) => doc.nodes.find((n) => n.id === id)!.warning)) ?? first;
    renderAt(`/compacts/${c.id}?scn=decoupling`);
    expect(await h1(titleOf(c))).toBeTruthy();
    const ns = c.nodes.map((id) => doc.nodes.find((n) => n.id === id)!);
    // the compact-level notice names the warned nodes
    const warned = ns.filter((n) => n.warning);
    if (warned.length) {
      const notice = screen.getByTestId('compact-physical-warnings').closest('[role="status"]') as HTMLElement;
      expect(notice.textContent).toContain('PHYSICAL CAPABILITY NOT VALIDATED');
      for (const n of warned) expect(notice.textContent).toContain(n.label);
    }
    const card = screen.getByRole('region', { name: "Each step's coverage and treatment" });
    for (const n of ns) {
      const item = within(card).getByRole('heading', { level: 4, name: n.label }).closest('li') as HTMLElement;
      if (n.policyNeeds) expect(within(item).getByTestId('policy-needs').textContent).toBe(`Actions needed: ${policyNeedsWords(n.policyNeeds)}`);
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
    expect(await h1(titleOf(c))).toBeTruthy();
    const card = screen.getByRole('region', { name: "Each step's coverage and treatment" });
    const item = within(card).getByRole('heading', { level: 4, name: n.label }).closest('li') as HTMLElement;
    expect(within(item).queryByTestId('physical-warning')).toBeNull();
    expect(within(item).getAllByTestId('variant-physical-warning').length).toBeGreaterThan(0);
    expect(within(item).getByText(/^Class under other assumptions and growth bounds/).textContent).toMatch(/physical capability not validated under \d+ other setting/);
    // the node table twin names the settings
    fireEvent.click(within(card).getByRole('button', { name: 'Table' }));
    expect(within(card).getByRole('columnheader', { name: 'Warning under other settings' })).toBeTruthy();
    noAcronyms();
  });

  it('an older bundle without the methodology fields (nor the dictionary: OSI technologies) still renders the landing and every compact', async () => {
    useSource((d) => void stripMethodology(d));
    const old = stripMethodology(structuredClone(doc));
    renderAt('/compacts');
    expect(await h1('Sectoral Compacts')).toBeTruthy();
    expect(screen.getByText(/Who may supply:/)).toBeTruthy();
    expect(screen.queryByRole('checkbox', { name: 'Include nodes outside the strategic scope' })).toBeNull();
    expect(screen.queryAllByTestId('physical-warning')).toEqual([]);
    expect(screen.queryAllByTestId('policy-needs')).toEqual([]);
    // the master words: technologies, one plain list, no index
    const select = screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement;
    expect(select.options[0].textContent).toBe('All technologies');
    expect(select.querySelectorAll('optgroup')).toHaveLength(0);
    expect(screen.getAllByTestId('compact-taxonomy').map((x) => (x.textContent ?? '').slice(0, 14))).toEqual(old.compacts.map(() => 'Technologies: '));
    expect(screen.getByTestId('run-settings').textContent).not.toMatch(/dictionary/);
    noAcronyms();
    cleanup();
    for (const c of old.compacts) {
      renderAt(`/compacts/${c.id}`);
      expect(await h1(titleOf(c, old))).toBeTruthy();
      expect(screen.queryByTestId('compact-indices')).toBeNull();
      expect(screen.getByRole('region', { name: COALITION_TITLE })).toBeTruthy();
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
    renderAt(`/compacts/${first.id}?scn=decoupling`);
    expect(await h1(titleOf(first))).toBeTruthy();
    const nodeId = first.trade[first.trade.length - 1].node;
    const label = doc.nodes.find((n) => n.id === nodeId)!.label;
    fireEvent.change(screen.getByRole('combobox', { name: 'Supply-chain step' }), { target: { value: nodeId } });
    const nTrade = first.trade.filter((e) => e.node === nodeId).length;
    const nInv = doc.investmentFlows
      ? doc.investmentFlows.pairs.filter((p) => p.rank === 1 && p.node === nodeId && first.nodes.includes(p.node)).length
      : first.investment.filter((e) => e.node === nodeId).length;
    const invWord = doc.investmentFlows ? 'investment flow' : 'investment relationship';
    await waitFor(() => expect(screen.getByText(`${nTrade} trade relationship${nTrade === 1 ? '' : 's'}, ${nInv} ${invWord}${nInv === 1 ? '' : 's'}`)).toBeTruthy());
    expect(within(screen.getByRole('region', { name: 'Trade relationships' })).getByText(new RegExp(`Exporter to importer, ${label.replace(/[&()]/g, '.')}`))).toBeTruthy();
  });

  it('a compact without investment relationships says so; an unreached top target is explained', async () => {
    const flows = doc.investmentFlows;
    const c = flows ? doc.compacts.find((x) => !flows.pairs.some((p) => p.rank === 1 && x.nodes.includes(p.node))) : doc.compacts.find((x) => !x.investment.length);
    if (!c) return;
    renderAt(`/compacts/${c.id.toLowerCase()}?scn=decoupling`);
    expect(await h1(titleOf(c))).toBeTruthy();
    if (flows) {
      expect(within(screen.getByRole('region', { name: 'Who invests where' })).getByText(/No simulated investment flow/)).toBeTruthy();
      expect(screen.getByTestId('investment-step').textContent).toMatch(/no investment flow is simulated/);
    } else expect(within(screen.getByRole('region', { name: 'Investment relationships' })).getByText(/No investment relationships/)).toBeTruthy();
    if (!c.coalition.reached99) expect(screen.getByText(/The highest target is not reached/)).toBeTruthy();
    noAcronyms();
  });

  it('about: the method chain, the classes, the data and the thresholds from the data', async () => {
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    // the plain narrative first (what a compact is, the scenarios, the analysis in plain steps, what each step needs), the technical method in its disclosure
    for (const h of ['What a Sectoral Compact is', 'How the scenarios work', 'How the analysis works', AIB_CLASS_HEADING, 'Managed Interdependence', 'The method step by step', 'Data and settings', 'The full method'])
      expect(screen.getByRole('heading', { level: 2, name: h })).toBeTruthy();
    expect(screen.getByTestId('page-question').textContent).toBe('What is a Sectoral Compact, and how does the analysis find one?');
    expect(within(screen.getByTestId('about-plain-method')).getAllByRole('listitem')).toHaveLength(11);
    expect((screen.getByTestId('about-technical') as HTMLDetailsElement).open).toBe(false);
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
      'Whose demand is protected, and who may supply', 'Does the removed supplier keep buying? Hold and release', 'Who can supply more: production-constrained and trade-only',
      'Actions needed and the weak-production-evidence warning', 'The smallest coalition: exact, greedy and the tiers', 'Losing the largest supplier: true and instantaneous N-1',
      'How far existing trade can grow', 'Strategic scope',
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
    for (const t of ['Core coordinating members', 'Critical supply partners', 'Prospective hosts', 'Associate partners', 'Critical external dependencies', 'Critical external dependencies (marginal)', 'External dependencies']) {
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

  it('about: where the nodes come from (the dictionary: groups of HS6 codes, the six stages from the item type, its scope, primary rows; shown by OSI technology x stage through the six-stage crosswalk; the dictionary sectors as provenance, no index list); the master words for a master bundle', async () => {
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Where the supply-chain steps (nodes) come from' })).toBeTruthy();
    const about = screen.getByTestId('about-taxonomy').textContent ?? '';
    const page = main().textContent ?? '';
    if (tx.dictionary) {
      expect(about).toMatch(/electro-industrial HS6 dictionary/);
      expect(about).not.toMatch(/index > sector/);
      // each node at one of the six stages, from its codes' item type; none at Deployment
      expect(about).toMatch(/six supply-chain stages the OSI uses \(Extraction, Refining, Components, Assembly, Deployment, Recovery\)/);
      expect(about).toMatch(/a processed material Refining/);
      expect(about).toMatch(/No node is at Deployment/);
      expect(about).toMatch(/primary row/);
      if (tx.scope === 'core') expect(about).toMatch(/the core scope: .*; the extended scope \(.*\) is a sensitivity/);
      expect(about).toMatch(/it has no fossil-fuel nodes/);
      // how the pages show the nodes: OSI technology x stage, as equals, the basket-less apart
      const osi = screen.getByTestId('about-osi-cells').textContent ?? '';
      expect(osi).toMatch(/^The pages show each node by OSI technology and stage/);
      expect(osi).toMatch(/all of them as equals/);
      expect(osi).toContain(`is shown as ${NOT_IN_BASKET} and gets neutral partner scores`);
      expect(osi).toMatch(/read a node through those six-stage cells/);
      // the dictionary's sectors and indices: one provenance paragraph, no index list
      expect(screen.getByTestId('about-dictionary-sectors').textContent).toMatch(/the CSVs keep each node's dictionary sector as a dictionary_sector column/);
      expect(screen.queryByTestId('about-indices')).toBeNull();
      // the strategic scope: never an unconditional fossil-fuel statement
      const st = screen.getByTestId('about-strategic').textContent ?? '';
      expect(st).not.toMatch(/fossil-fuel supply chains among them/);
      if (doc.meta.strategicScope === 'all') expect(st).toMatch(/^All nodes: compacts can be formed from every node of the electro-industrial dictionary/);
      const scopeWord = tx.scope ? ` (${tx.scope} scope)` : '';
      expect(screen.getByTestId('about-nodes-source').textContent).toBe(`Electro-industrial dictionary${scopeWord}; ${doc.nodes.length} ${doc.nodes.length === 1 ? 'node' : 'nodes'}`);
    }
    // no three-stage words anywhere on the page
    expect(page).not.toMatch(/Upstream|Midstream|Downstream|three-stage|three stages/);
    noAcronyms();
    cleanup();
    // the master form: the OSI basket, no dictionary words, no index list
    useSource((d) => void toMasterTaxonomy(d));
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    expect(screen.getByTestId('about-taxonomy').textContent).toMatch(/sub-sectors of the OSI technology basket/);
    expect(screen.getByTestId('about-taxonomy').textContent).toMatch(/at one stage \(Extraction, Refining, Components, Assembly, Deployment, Recovery\)/);
    expect(screen.getByTestId('about-taxonomy').textContent).toMatch(/read for each node's own technology and stage\.$/);
    expect(screen.queryByTestId('about-indices')).toBeNull();
    expect(screen.queryByTestId('about-dictionary-sectors')).toBeNull();
    expect(screen.getByTestId('about-nodes-source').textContent).toBe(`OSI basket; ${docM.nodes.length} ${docM.nodes.length === 1 ? 'node' : 'nodes'}`);
    // fossil fuels are named only where nodes outside the scope are fossil-fuel ones (none in the fixture's master form)
    const fossil = docM.nodes.some((n) => n.inStrategicScope === false && n.techs.some((t) => /^(coal|oil|gas|natural gas)$/i.test(t)));
    expect(/fossil-fuel supply chains among them/.test(screen.getByTestId('about-strategic').textContent ?? '')).toBe(fossil);
    expect(main().textContent).not.toMatch(/Upstream|Midstream|Downstream/);
    noAcronyms();
  });

  it('a compact: its OSI technologies in the design and the lede, each node by technologies · stage with its OSI cells linking to their Sector profiles, no index', async () => {
    const c = doc.compacts.find((x) => x.nodes.some((id) => (doc.nodes.find((n) => n.id === id)?.yfCells.length ?? 0) > 1)) ?? first;
    const own = c.nodes.map((id) => doc.nodes.find((n) => n.id === id)!).filter(Boolean);
    renderAt(`/compacts/${c.id}`);
    expect(await h1(titleOf(c))).toBeTruthy();
    expect(screen.queryByTestId('compact-indices')).toBeNull();
    const design = screen.getByRole('region', { name: "The supply-chain steps covered and the compact's design" });
    const terms = within(design).getAllByRole('term').map((t) => t.textContent);
    expect(terms).toContain('Technologies');
    for (const t of ['Sectors', 'Index', 'Indices']) expect(terms).not.toContain(t);
    expect(within(design).getByTestId('compact-technologies').textContent).toBe(technologiesOfNodes(own).join(', '));
    // the lede names the OSI technologies of the nodes' cells, never the dictionary's sectors
    const techs = [...new Set(own.flatMap((n) => osiTechsOfNode(n)))].sort((a, b) => a.localeCompare(b, 'en'));
    const lede = (screen.getByRole('heading', { level: 1 }).closest('header') ?? main()).textContent ?? '';
    if (techs.length) expect(lede).toContain(`: ${listWords(techs)}`);
    expect(lede).not.toMatch(/ sectors? \(ind/);
    const card = screen.getByRole('region', { name: "Each step's coverage and treatment" });
    for (const n of own) {
      const item = within(card).getByRole('heading', { level: 4, name: n.label }).closest('li') as HTMLElement;
      expect(item.textContent).toContain(nodeWhere(n));
      // its OSI cells, each linking to its six-stage Sector profile
      const cellsLine = within(item).getByTestId('node-osi-cells');
      for (const p of profiledCellsOfNode(n)) expect(within(cellsLine).getByRole('link', { name: p.label }).getAttribute('href')).toBe(p.href);
      if (!n.yfCells.length) expect(cellsLine.textContent).toMatch(/none: none of its codes is in the OSI technology basket/);
    }
    fireEvent.click(within(design).getByRole('button', { name: 'Table' }));
    expect(within(design).getAllByRole('columnheader').map((h) => h.textContent).slice(0, 4)).toEqual(['Supply-chain step', 'Technologies', 'Stage', 'OSI cells']);
    expect(main().textContent).not.toMatch(/Upstream|Midstream|Downstream/);
    noAcronyms();
  });

  it('partnerships: the Technology filter maps to six-stage OSI cells through the crosswalk; a node with no basket code gets a notice; old links are dropped', async () => {
    setEngineClient(neverReadyClient());
    // a technology of a node with basket cells; prefer a node in several technologies
    const mapped = doc.nodes.filter((n) => n.yfCells.length);
    const trap = mapped.find((n) => osiTechsOfNode(n).length > 1) ?? mapped[0];
    const tech = osiTechsOfNode(trap)[0];
    const pick = osiCellPick(doc.nodes, { country: null, tech, stage: null, node: null })!;
    const r = renderAt(`/aib/partnerships?tech=${encodeURIComponent(tech)}`);
    expect(await h1('Partnerships')).toBeTruthy();
    // the filter is "Technology", one plain list of OSI technologies; the node filter is "Node"
    const select = screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement;
    expect(select.value).toBe(tech);
    expect(select.querySelectorAll('optgroup')).toHaveLength(0);
    expect(Array.from(select.options).map((o) => o.value)).toEqual(['', ...technologiesOfNodes(doc.nodes)]);
    expect(screen.getByRole('combobox', { name: 'Supply-chain step' })).toBeTruthy();
    expect(screen.queryByRole('combobox', { name: 'Sector' })).toBeNull();
    // the Stage filter: the stages of the chosen technology's cells, in chain order (no Deployment: no node has a cell there)
    const stageOptions = [...(screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement).options].map((o) => o.value);
    expect(stageOptions).toEqual(['', ...cellStagesOfNodes(doc.nodes, tech)]);
    expect(stageOptions.slice(1).every((s) => (SUPPLY_STAGES as readonly string[]).includes(s))).toBe(true);
    if (!doc.nodes.some((n) => nodeStagesOf(n).includes('Deployment'))) expect(stageOptions).not.toContain('Deployment');
    // the Partnership Opportunity card names the six-stage OSI cell it reads, of the filter's technology
    const po = await screen.findByRole('region', { name: 'Partnership Opportunity' });
    const note = within(po).getByTestId('osi-cell-note').textContent ?? '';
    expect(note).toContain(`Partner indices for the OSI cell ${pick.cell!.tech} – ${pick.cell!.stage}`);
    expect(pick.cell!.tech).toBe(tech);
    expect((SUPPLY_STAGES as readonly string[]).includes(pick.cell!.stage)).toBe(true);
    if (pick.nodes.length === 1) expect(note).toContain(`the codes of ${pick.nodes[0].label} sit in`);
    expect(within(po).queryByTestId('three-stage-note')).toBeNull();
    expect(main().textContent).not.toMatch(/Upstream|Midstream|Downstream|three-stage|three stages/);
    // Investment for one economy: the factory projects and positions read the same OSI cell
    fireEvent.click(screen.getByRole('radio', { name: 'Investment' }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('mode')).toBe('investment'));
    expect(within(await screen.findByRole('region', { name: 'Friendshore partners' })).getByTestId('osi-cell-note').textContent).toContain(`OSI cell ${pick.cell!.tech} – ${pick.cell!.stage}`);
    const economy = doc.compacts.flatMap((c) => c.roles.map((x) => x.iso3))[0];
    if (economy) {
      cleanup();
      renderAt(`/aib/partnerships?tech=${encodeURIComponent(tech)}&mode=investment&country=${economy}`);
      expect(await h1('Partnerships')).toBeTruthy();
      const inv = (await screen.findByTestId('osi-cell-investment-note')).textContent ?? '';
      expect(inv).not.toMatch(/^Partner indices/);
      // CM-FDI: the Components and Assembly cells of four technologies, the same projects on both (said once)
      const cmfdi = ['Solar', 'Wind', 'Batteries', 'Electric Vehicles'].includes(pick.cell!.tech) && ['Components', 'Assembly'].includes(pick.cell!.stage);
      if (cmfdi) {
        expect(inv).toMatch(new RegExp(`^Factory projects are kept by OSI cell: shown for ${pick.cell!.tech} – ${pick.cell!.stage}`));
        expect(inv).toContain('Components and Assembly carry the same factory projects');
      } else {
        expect(inv).toMatch(new RegExp(`^The direct-investment positions below are economy-wide\\. Factory projects are kept by OSI cell and recorded only for the Components and Assembly cells of .* ${pick.cell!.tech} – ${pick.cell!.stage}, .*has none`));
      }
      expect(inv).not.toMatch(/Midstream|three stages/);
      expect(main().textContent).toMatch(/the factory projects instead \(shown below when the selection's OSI cell is one of those\)/);
      expect(main().textContent).not.toMatch(/choose one of those sectors/);
    }
    cleanup();
    // the nodes with no basket code: their own option, a clear notice, no silent fallback to a cell
    if (doc.nodes.some((n) => !n.yfCells.length)) {
      renderAt(`/aib/partnerships?tech=${encodeURIComponent(NOT_IN_BASKET)}&mode=development`);
      expect(await h1('Partnerships')).toBeTruthy();
      expect((screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement).value).toBe(NOT_IN_BASKET);
      expect(await screen.findByRole('region', { name: 'Capacity gaps' })).toBeTruthy();
      cleanup();
      renderAt(`/aib/partnerships?tech=${encodeURIComponent(NOT_IN_BASKET)}`);
      expect(await h1('Partnerships')).toBeTruthy();
      const card = await screen.findByRole('region', { name: 'Partnership Opportunity' });
      const notice = within(card).getByTestId('osi-cell-notice').textContent ?? '';
      expect(notice).toMatch(/Partner indices are kept by OSI technology × stage, and /);
      expect(notice).toMatch(/partner scores are neutral/);
      expect(within(card).queryByTestId('osi-cell-note')).toBeNull();
    }
    cleanup();
    // old links: a three-stage stage (?stage=Midstream) and a dictionary sector (?tech=<sector>) name no option: dropped
    const sector = doc.nodes.flatMap((n) => n.techs).find((t) => !technologiesOfNodes(doc.nodes).includes(t)) ?? 'Nonesuch';
    renderAt(`/aib/partnerships?stage=Midstream&tech=${encodeURIComponent(sector)}`);
    expect(await h1('Partnerships')).toBeTruthy();
    expect((screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement).value).toBe('');
    expect((screen.getByRole('combobox', { name: 'Technology' }) as HTMLSelectElement).value).toBe('');
    noAcronyms();
    setEngineClient(null);
  });

  it('a master-taxonomy bundle (OSI technologies) still renders the landing, a compact, the partnerships and About', async () => {
    useSource((d) => void toMasterTaxonomy(d));
    setEngineClient(neverReadyClient());
    renderAt('/compacts/workstreams?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    expect(screen.getByRole('combobox', { name: 'Technology' })).toBeTruthy();
    expect(screen.getAllByTestId('compact-taxonomy')).toHaveLength(docM.compacts.length);
    noAcronyms();
    cleanup();
    const c = docM.compacts[0];
    renderAt(`/compacts/${c.id}?scn=decoupling`);
    expect(await h1(titleOf(c, docM))).toBeTruthy();
    expect(within(screen.getByRole('region', { name: "The supply-chain steps covered and the compact's design" })).getAllByRole('term').map((t) => t.textContent)).toContain('Technologies');
    expect(screen.queryByTestId('compact-indices')).toBeNull();
    expect(openTwins()).toBe(12);
    noAcronyms();
    cleanup();
    // a technology and stage: the six-stage OSI cell of that technology the matching nodes' codes sit in
    const n = docM.nodes.find((x) => x.yfCells.length && x.inStrategicScope !== false)!;
    const t0 = osiTechsOfNode(n)[0];
    const mPick = osiCellPick(docM.nodes, { country: null, tech: t0, stage: n.stage, node: null })!;
    expect([mPick.cell!.tech, mPick.cell!.stage]).toEqual([t0, n.stage]);
    renderAt(`/aib/partnerships?tech=${encodeURIComponent(t0)}&stage=${n.stage}`);
    expect(await h1('Partnerships')).toBeTruthy();
    expect((screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement).value).toBe(n.stage);
    const po = await screen.findByRole('region', { name: 'Partnership Opportunity' });
    expect(within(po).getByTestId('osi-cell-note').textContent).toContain(`OSI cell ${t0} – ${mPick.cell!.stage}`);
    cleanup();
    // every technology of a multi-technology node (rare earths: Electric Vehicles and Wind) reads its own cell
    const multi = docM.nodes.find((x) => osiTechsOfNode(x).length > 1);
    if (multi) {
      for (const t of osiTechsOfNode(multi)) {
        const cell = osiCellPick(docM.nodes, { country: null, tech: t, stage: multi.stage, node: null })!.cell!;
        expect(cell.tech).toBe(t);
        renderAt(`/aib/partnerships?tech=${encodeURIComponent(t)}&stage=${multi.stage}`);
        expect(await h1('Partnerships')).toBeTruthy();
        const card = await screen.findByRole('region', { name: 'Partnership Opportunity' });
        const note = within(card).getByTestId('osi-cell-note').textContent ?? '';
        expect(note).toContain(`Partner indices for the OSI cell ${t} – ${cell.stage}`);
        for (const o of osiTechsOfNode(multi).filter((x) => x !== t)) expect(note).not.toContain(`${o} – ${cell.stage}`);
        cleanup();
      }
    }
    noAcronyms();
    setEngineClient(null);
  });

  it('an unknown compact id: the Workstreams index with a notice, the scenario kept; deeper paths go to the compact; an SC- id goes to the strategic compact; the alias', async () => {
    const r = renderAt('/compacts/NOPE-9?scn=decoupling');
    expect(await h1('Workstreams')).toBeTruthy();
    expect(screen.getByText("No workstream with the id 'NOPE-9'; showing all workstreams.")).toBeTruthy();
    expect(r.state.location.pathname + r.state.location.search).toBe('/compacts/workstreams?scn=decoupling');
    cleanup();
    const r2 = renderAt(`/compacts/${first.id}/extra?x=1`);
    expect(await h1(titleOf(first))).toBeTruthy();
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
    expect(await screen.findByText(STATE.aibUnavailable, undefined, { timeout: 30_000 })).toBeTruthy();
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
    expect(await screen.findByText('The Allied Industrial Base results could not be loaded', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/does not match the contract \(compacts: expected an array\)/)).toBeTruthy();
    broken = false;
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(await h1('Sectoral Compacts')).toBeTruthy();
  });

  it('country roles (IA refactor): the role matrix in the brief order, one row per economy with its anchor, each linking to its Country profile; Country roles current', async () => {
    renderAt('/aib/roles?scn=decoupling#role-' + doc.compacts[0].coalition.members[0]);
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Country roles' }).getAttribute('aria-current')).toBe('page'));
    const iso = doc.compacts[0].coalition.members[0];
    const link = within(main()).getAllByRole('link').find((a) => a.getAttribute('href') === `/countries/${iso}?scn=decoupling#aib-role`);
    expect(link).toBeTruthy();
    const matrix = screen.getByRole('region', { name: 'Roles by economy' });
    expect(within(matrix).getAllByRole('columnheader').map((h) => h.textContent)).toEqual([
      'Economy', 'Resource / Supplier', 'Specialized supplier', 'Investment host', 'Investor', 'Demand anchor / Buyer', 'Bridge', 'Compact participation',
    ]);
    expect(screen.getByRole('heading', { level: 2, name: 'How roles are read' })).toBeTruthy();
    expect(within(main()).getAllByTestId('modelled-note')[0].textContent).toMatch(/does not move with the Perspective dials/);
    // what "every shock" sums: the supply shocks (labelled Decoupling's where the bundle has the scenarios), and the diversification scenario named apart where the export counts it
    const everyNote = hasScenarios(doc) ? screen.getByTestId('every-shock-note').textContent : within(main()).getAllByTestId('modelled-note')[0].textContent;
    expect(everyNote).toContain((hasScenarios(doc) ? everyShockLabel(doc) : everyShock(doc)).note);
    if (everyShock(doc).diversify) expect(everyNote).toContain('and the diversification scenario, which removes no supplier');
    const row = document.getElementById(`role-${iso}`) as HTMLElement;
    expect(row.className).toContain('bg-signal/10');
    expect(within(row).getAllByRole('link').some((a) => a.getAttribute('href') === `/compacts/${doc.compacts[0].id}?scn=decoupling`)).toBe(true);
    expect(within(main()).getByText(hasScenarios(doc) ? new RegExp(`^${BASIS_WORDS.model.label} · `) : BASIS_WORDS.facts.label)).toBeTruthy();
    // with today's network (the Baseline file): who supplies whom and the roles gained or lost, each with a twin
    const withToday = hasScenarios(doc) && !!readManifest().files.scenarios?.baseline;
    if (withToday) expect(await within(main()).findByRole('region', { name: /^Who supplies whom/ }, { timeout: 15_000 })).toBeTruthy();
    noAcronyms();
    expect(openTwins()).toBe(withToday ? 3 : 1);
    noAcronyms();
  });

  it('overview (IA refactor): node counts by treatment, the highest severity and dividends, the three question cards; never starts the engine', async () => {
    setEngineClient(null);
    renderAt('/aib?scn=decoupling');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Overview' }).getAttribute('aria-current')).toBe('page'));
    const tiles = screen.getByRole('list', { name: 'Supply-chain steps by treatment' });
    const items = within(tiles).getAllByRole('listitem');
    ['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION'].forEach((cls, i) => {
      const n = doc.nodes.filter((x) => x.cls === cls).length;
      expect(items[i].textContent).toContain(classInfo(cls).label);
      expect(items[i].textContent).not.toContain(cls);
      expect(items[i].textContent).toContain(`${n} ${n === 1 ? 'step' : 'steps'}`);
    });
    expect(within(screen.getByRole('list', { name: 'Steps with the most supply exposed to the disruption' })).getAllByRole('listitem').length).toBeGreaterThan(0);
    expect(within(screen.getByRole('list', { name: 'Steps where coordination recovers most' })).getAllByRole('listitem').length).toBeGreaterThan(0);
    const hrefs = within(screen.getByRole('region', { name: 'Who does what in the allied network' })).getAllByRole('link').map((a) => a.getAttribute('href'));
    expect(hrefs).toEqual(['/aib/partnerships?scn=decoupling', '/compacts?scn=decoupling', '/aib/roles?scn=decoupling']);
    expect(within(main()).getAllByText(BASIS_WORDS.facts.label).length).toBeGreaterThan(0);
    expect(peekEngineClient()).toBeNull();
    noAcronyms();
  });

  it('partnerships (IA refactor): Trade by default with the coordinated relationships; the toggle and the filters write the URL; Investment and Development', async () => {
    setEngineClient(neverReadyClient());
    const r = renderAt('/aib/partnerships');
    expect(await h1('Partnerships')).toBeTruthy();
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Partnerships' }).getAttribute('aria-current')).toBe('page'));
    const trade = await screen.findByRole('region', { name: 'Coordinated trade relationships' }, { timeout: 15_000 });
    const edge = doc.compacts.flatMap((c) => c.trade).find((e) => e.usd && e.usd > 0 && e.from !== e.to)!;
    // the Sankey draws once its width is measured: wait for it (slow under a loaded full run)
    expect(await within(trade).findByRole('img', { name: /exporter to importer/ }, { timeout: 15_000 })).toBeTruthy();
    expect(screen.getByRole('region', { name: 'Specialized suppliers' })).toBeTruthy();
    // the three lens tags (2026-10-19: Today, OSI recommends, AIB model)
    expect(screen.getAllByText('Today · observed').length).toBeGreaterThan(0);
    expect(screen.getAllByText(/OSI recommends · NEIS defaults/).length).toBeGreaterThan(0);
    expect(screen.getAllByText(new RegExp(`^${BASIS_WORDS.model.label}`)).length).toBeGreaterThan(0);
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

  it('partnerships, investment (2026-10-17): investor-to-host flows of the best-ranked hosts, every ranked host as alternatives, the economy as investor or host; no original network design', async () => {
    const flows = doc.investmentFlows;
    if (!flows) return;
    setEngineClient(neverReadyClient());
    const r = renderAt('/aib/partnerships?scn=decoupling');
    expect(await h1('Partnerships')).toBeTruthy();
    expect(screen.queryByText(/allied production network/)).toBeNull();
    fireEvent.click(screen.getByRole('radio', { name: 'Investment' }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('mode')).toBe('investment'));
    const card = await screen.findByRole('region', { name: 'Who invests where' });
    expect(await within(card).findByRole('img', { name: /^Investor to host flows/ }, { timeout: 15_000 })).toBeTruthy();
    const best = flows.pairs.filter((p) => p.rank === 1);
    const items = () => within(within(card).getByTestId('flow-list')).getAllByRole('listitem');
    expect(items()).toHaveLength(Math.min(8, best.length));
    expect(within(card).getByTestId('modelled-note').textContent).toMatch(/does not move with the Perspective dials/);
    fireEvent.click(within(card).getByRole('radio', { name: 'Every ranked host' }));
    await waitFor(() => expect(items()).toHaveLength(Math.min(8, flows.pairs.length)));
    expect(within(card).getAllByText(/2nd of 5/).length).toBeGreaterThan(0);
    // one economy: as host (its new capacity serves investors) or as investor
    const host = best[0].host;
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: host } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('country')).toBe(host));
    const card2 = screen.getByRole('region', { name: 'Who invests where' });
    fireEvent.click(within(card2).getByRole('radio', { name: 'As investor' }));
    const asInvestor = flows.pairs.filter((p) => p.investor === host);
    if (!asInvestor.length) await waitFor(() => expect(within(card2).getByText(/No simulated investment flow/)).toBeTruthy());
    fireEvent.click(within(card2).getByRole('radio', { name: 'As host' }));
    await waitFor(() => expect(within(card2).getAllByRole('link', { name: doc.countries[host] }).length).toBeGreaterThan(0));
    noAcronyms();
    setEngineClient(null);
  });

  it('country roles (2026-10-17): a role is named at the floor, minor below it; "Show" keeps one role, named first and largest first', async () => {
    const net = doc.network;
    if (!net) return;
    renderAt('/aib/roles?scn=decoupling');
    expect(await h1(ROLES_PAGE_TITLE)).toBeTruthy();
    const cellOf = (iso: string, col: number) => (document.getElementById(`role-${iso}`) as HTMLElement).querySelectorAll('td')[col];
    // columns: 0 supplier, 1 specialized, 2 host, 3 investor, 4 buyer, 5 bridge, 6 participation
    const named = net.countries.find((c) => c.buyer.headline >= 0.01)!;
    // the cell leads with its headline value as the page formats it (one decimal below 10 points)
    expect(cellOf(named.iso3, 4).textContent!.startsWith(fmtPts(named.buyer.headline))).toBe(true);
    // below the floor and not named by a compact either (a compact's capacity-building host is named whatever its network value);
    // Decoupling reads the headline only (2026-10-19): minor where the headline shock contributes below the floor
    const src = scenarioRoleSource(doc, 'decoupling');
    const minor = net.countries.find((c) => c.host.headline > 0 && c.host.headline < 0.01 && countryRolesOf(doc, countryNamesOf(doc), c.iso3, src).cells.host.level === 'minor');
    if (minor) expect(cellOf(minor.iso3, 2).textContent).toMatch(/^minor/);
    fireEvent.change(screen.getByRole('combobox', { name: 'Show' }), { target: { value: 'investor' } });
    const investors = net.countries.filter((c) => c.investor.headline > 0).sort((a, b) => b.investor.headline - a.investor.headline);
    await waitFor(() => expect(within(screen.getByTestId('role-matrix')).getAllByRole('rowheader').map((h) => h.textContent)).toEqual(investors.map((c) => doc.countries[c.iso3] ?? c.iso3)));
    noAcronyms();
  });

  it('a compact deep link to a method anchor opens the method details', async () => {
    renderAt(`/compacts/${first.id}#coalition`);
    expect(await h1(titleOf(first))).toBeTruthy();
    expect((screen.getByText(/^Method details:/).closest('details') as HTMLDetailsElement).open).toBe(true);
  });

  it('/aib/<unknown> goes to the overview with a notice', async () => {
    const router = renderAt('/aib/nothing');
    await waitFor(() => expect(router.state.location.pathname).toBe('/aib'), { timeout: 30_000 });
    expect(await screen.findByText(/That Allied Industrial Base page does not exist/, undefined, { timeout: 30_000 })).toBeTruthy();
  });

  it('a newer export asks for a reload', async () => {
    useFailing({ manifest: async () => ({ ...readManifest(), version: AIB_VERSION + 1 }), json: async () => ({}) });
    renderAt('/compacts');
    expect(await screen.findByText('Please reload', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Reload' })).toBeTruthy();
  });
});

describe.skipIf(!haveFixture())('the diversification scenario (2026-10-19)', () => {
  const doc = readDoc();
  const names = countryNamesOf(doc);
  const has = hasDiversify(doc);
  const shockId = diversifyShockIdOf(doc.meta);
  const rows = diversifyRows(doc.nodes, shockId);
  const ran = orderDiversify(rows.filter((r) => r.ran));
  const totals = diversifyTotals(doc.nodes);
  const byId = new Map(doc.nodes.map((n) => [n.id, n]));
  const titleOf = (c: AibCompact) => compactTitle(c, doc.nodes);
  const DIVERSIFIED = 'diversified to the benchmark';
  const chartIds = () => within(screen.getByRole('list', { name: /^Supply-chain steps diversified to their benchmark/ })).getAllByRole('listitem').map((li) => li.getAttribute('data-node'));
  const tableCard = () => screen.getByRole('region', { name: MI_TABLE_TITLE });
  const bodyRows = () => within(tableCard()).getAllByRole('row').filter((tr) => tr.closest('tbody'));

  it('the page: the question, the rule and the totals; the chart of the nodes diversified, largest capacity gap first; the table; Diversification current', async () => {
    if (!has) return;
    renderAt('/aib/diversification');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    await waitFor(() => expect(document.title).toMatch(/^Managed Interdependence \| Allied Industrial Base \| Allied Industrial Base Explorer/));
    await waitFor(() => expect(within(views()).getByRole('link', { name: 'Managed Interdependence' }).getAttribute('aria-current')).toBe('page'));
    expect(within(views()).getAllByRole('link').map((a) => a.textContent)).toEqual(AIB_VIEWS);
    expect(screen.getByTestId('page-question').textContent).toBe(MI_QUESTION);
    expect(screen.getByText(/The scenario removes no supplier: where a supply-chain step is more concentrated than comparable goods, it caps the share/)).toBeTruthy();
    expect(screen.getByTestId('diversify-rule').textContent).toMatch(/not a forecast; dollar values are \d{4} trade values/);
    // concentration in plain words; the cap is the one that WOULD bring a node down to its benchmark, in the allied market
    expect(screen.getByTestId('diversify-rule').textContent).toContain(HHI_SHORT);
    expect(screen.getByTestId('diversify-rule').textContent).toContain('by more than a small tolerance, gets the supplier-share cap that would bring its concentration down to the benchmark, applied to the allied market');
    expect(screen.getByTestId('diversify-totals').textContent).toContain(totalsWords(totals));
    expect(screen.getByTestId('diversify-totals').textContent).not.toMatch(/more concentrated than it\b|did not optimize|Most often capped/);
    if (totals.nodes > totals.assessed) expect(screen.getByTestId('diversify-totals').textContent).toContain('the trade analysis was not run for');
    const tiles = within(screen.getByRole('list', { name: 'Managed Interdependence totals' })).getAllByRole('listitem');
    expect(tiles).toHaveLength(4);
    expect(tiles[0].textContent).toContain(`${totals.above} of ${totals.withBenchmark}`);
    expect(tiles[1].textContent).toContain(fmtUsd(totals.shiftUsd));
    expect(tiles[2].textContent).toContain(fmtUsd(totals.tradeUsd));
    expect(tiles[3].textContent).toContain(fmtUsd(totals.gapUsd));
    expect(screen.getByTestId('diversify-capped').textContent).toContain(names[totals.capped[0].iso3] ?? totals.capped[0].iso3);
    expect(screen.getAllByText(/Most often capped/)).toHaveLength(1);
    // the axis reads as the rows do: current, then benchmark; the capped suppliers are labelled
    expect(screen.getByTestId('diversify-axes').textContent).toContain('Concentration (HHI): current → benchmark');
    // the chart: the nodes where the scenario ran, largest gap first; each with its concentration and its split
    expect(chartIds()).toEqual(ran.slice(0, CHART_ROWS).map((r) => r.node.id));
    const items = within(screen.getByRole('list', { name: /^Supply-chain steps diversified to their benchmark/ })).getAllByRole('listitem');
    for (const li of items) {
      expect(within(li).getByTestId('hhi-dumbbell').getAttribute('aria-label')).toMatch(/concentration \(HHI\) [\d.]+ against a benchmark of [\d.]+/);
      expect(within(li).getByTestId('demand-split').getAttribute('aria-label')).toMatch(/still supplied \d+% · recovered through trade \d+% · requires new capacity \d+% of allied demand$/);
      const r = ran.find((x) => x.node.id === li.getAttribute('data-node'))!;
      if (r.d.capped.length) expect(li.textContent).toContain(`· capped: ${names[r.d.capped[0]] ?? r.d.capped[0]}`);
    }
    if (ran.length > CHART_ROWS) {
      fireEvent.click(screen.getByRole('button', { name: `Show all ${ran.length} steps` }));
      expect(chartIds()).toHaveLength(ran.length);
    }
    expect(within(screen.getByRole('list', { name: 'Chart marks' })).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      'Current concentration', 'Benchmark concentration', 'Still supplied under the cap', 'Recovered through trade', 'Requires new capacity',
    ]);
    expect(within(main()).getByTestId('modelled-note').textContent).toMatch(/does not move with the Perspective dials/);
    // the table: by default the nodes diversified, in the same order
    expect(bodyRows()).toHaveLength(Math.min(25, ran.length));
    expect(within(bodyRows()[0]).getAllByRole('cell')[0].textContent).toBe(ran[0].node.label);
    expect(within(main()).getAllByRole('button', { name: 'Download CSV' })).toHaveLength(2);
    noAcronyms();
    // the chart has a table twin; the table card is one
    expect(openTwins()).toBe(1);
    expect(within(main()).getAllByRole('table')).toHaveLength(2);
    noAcronyms();
  });

  it('the toggle includes the nodes not diversified, with their status and without the run columns; the filters write the URL and apply to the chart and the table', async () => {
    if (!has) return;
    const r = renderAt('/aib/diversification');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    fireEvent.click(within(tableCard()).getByRole('switch', { name: 'Include steps not diversified' }));
    await waitFor(() => expect(r.state.location.search).toBe('?all=1'));
    expect(bodyRows()).toHaveLength(Math.min(25, rows.length));
    // a node the scenario did not diversify: its status, a dash in each run column
    const other = orderDiversify(rows).find((x) => !x.ran)!;
    const search = within(tableCard()).queryByRole('searchbox');
    if (search) fireEvent.change(search, { target: { value: other.node.label } });
    const headers = within(tableCard()).getAllByRole('columnheader').map((h) => h.textContent);
    const row = bodyRows().find((tr) => within(tr).getAllByRole('cell')[0].textContent === other.node.label)!;
    const cell = (h: string) => within(row).getAllByRole('cell')[headers.indexOf(h)].textContent;
    expect(cell('Status')).toBe(rowStatusLabel(other));
    expect(headers).toContain('Largest new or expanded relationships');
    expect(headers).not.toContain('Key new relationships');
    for (const h of ['Supply to shift (USD)', 'Coverage after existing relationships', 'Coverage after coordinated trade', 'Capacity gap (USD)', 'What it needs under the scenario', 'Best-placed countries to add capacity']) expect(cell(h), h).toBe('–');
    if (search) fireEvent.change(search, { target: { value: '' } });
    // the chart still shows only the nodes diversified
    expect(chartIds()).toEqual(ran.slice(0, CHART_ROWS).map((x) => x.node.id));
    // the Technology and Stage filters (no Index filter): a node in several technologies passes each
    expect(screen.queryByRole('combobox', { name: 'Index' })).toBeNull();
    const tech = techKeysOfNode(ran[0].node)[0];
    fireEvent.change(screen.getByRole('combobox', { name: 'Technology' }), { target: { value: tech } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('tech')).toBe(tech));
    expect(chartIds()).toEqual(ran.filter((x) => nodeInTech(x.node, tech)).slice(0, CHART_ROWS).map((x) => x.node.id));
    expect(bodyRows().length).toBe(Math.min(25, rows.filter((x) => nodeInTech(x.node, tech)).length));
    // the stage choices are the stages of that technology's cells (never node.stage)
    const stageSel = screen.getByRole('combobox', { name: 'Stage' }) as HTMLSelectElement;
    expect(Array.from(stageSel.options).map((o) => o.value)).toEqual(['', ...cellStagesOfNodes(rows.map((x) => x.node), tech)]);
    const stage = nodeStagesOf(ran[0].node, tech)[0];
    fireEvent.change(stageSel, { target: { value: stage } });
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('stage')).toBe(stage));
    expect(chartIds()).toEqual(ran.filter((x) => nodeInCell(x.node, tech, stage)).slice(0, CHART_ROWS).map((x) => x.node.id));
    // the place column: technologies · stage
    expect(within(tableCard()).getAllByRole('columnheader').map((h) => h.textContent)).toContain('Technologies · stage');
    noAcronyms();
    // an old ?index= / ?sector= link is dropped
    cleanup();
    renderAt(`/aib/diversification?index=A&sector=${encodeURIComponent(ran[0].node.techs[0])}`);
    expect(await h1('Managed Interdependence')).toBeTruthy();
    expect(chartIds()).toEqual(ran.slice(0, CHART_ROWS).map((x) => x.node.id));
  });

  it('the class under the scenario carries its physical-capability flag in the table; a node just above its benchmark names the tolerance', async () => {
    if (!has) return;
    const warned = ran.filter((r) => r.clsWarning === true);
    const near = orderDiversify(rows).filter((r) => r.nearBenchmark);
    const r = renderAt(`/aib/diversification${near.length ? '?all=1' : ''}`);
    expect(await h1('Managed Interdependence')).toBeTruthy();
    if (near.length) await waitFor(() => expect(r.state.location.search).toBe('?all=1'));
    const search = within(tableCard()).queryByRole('searchbox');
    const headers = within(tableCard()).getAllByRole('columnheader').map((h) => h.textContent);
    const rowOf = (label: string) => {
      if (search) fireEvent.change(search, { target: { value: label } });
      return bodyRows().find((tr) => within(tr).getAllByRole('cell')[0].textContent === label)!;
    };
    for (const w of warned.slice(0, 3)) {
      const tr = rowOf(w.node.label);
      const td = within(tr).getAllByRole('cell')[headers.indexOf('What it needs under the scenario')];
      expect(td.textContent, w.node.id).toContain(AIB_PHYSICAL_WARNING);
      expect(within(td).getAllByTestId('variant-physical-warning'), w.node.id).toHaveLength(1);
    }
    const plain = ran.find((x) => x.clsWarning === false);
    if (plain) expect(within(rowOf(plain.node.label)).getAllByRole('cell')[headers.indexOf('What it needs under the scenario')].textContent).not.toContain(AIB_PHYSICAL_WARNING);
    for (const n of near.slice(0, 2)) expect(within(rowOf(n.node.label)).getAllByRole('cell')[headers.indexOf('Status')].textContent).toBe('Already as diversified as its benchmark (above it by less than a small tolerance)');
    if (!process.env.AIB_DATA_DIR) expect([warned.map((x) => x.node.id), near.map((x) => x.node.id)]).toEqual([['wind-down-towers'], ['bat-up-graphite']]);
    noAcronyms();
  });

  it("the CSVs carry the scenario's shock id, also in a bundle whose nodes carry the scenario without meta.diversify", async () => {
    if (!has) return;
    useSource((d) => {
      delete d.meta.diversify;
    });
    const blobs: Blob[] = [];
    const cu = URL.createObjectURL;
    const ru = URL.revokeObjectURL;
    URL.createObjectURL = ((b: Blob) => (blobs.push(b), 'blob:test')) as typeof URL.createObjectURL;
    URL.revokeObjectURL = (() => undefined) as typeof URL.revokeObjectURL;
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);
    try {
      renderAt('/aib/diversification');
      expect(await h1('Managed Interdependence')).toBeTruthy();
      fireEvent.click(within(tableCard()).getByRole('button', { name: 'Download CSV' }));
      await waitFor(() => expect(blobs).toHaveLength(1));
      // (jsdom's Blob has no text())
      const text = await new Promise<string>((res, rej) => {
        const fr = new FileReader();
        fr.onload = () => res(fr.result as string);
        fr.onerror = () => rej(fr.error);
        fr.readAsText(blobs[0]);
      });
      const [head, first] = text.replace(/^\uFEFF/, '').split(/\r?\n/);
      const cols = head.split(',');
      expect(cols).toContain('class_under_scenario_warning');
      expect(cols).toContain('largest_new_or_expanded_relationships');
      expect(cols).not.toContain('key_relationships');
      // the run columns come last (a node label may hold a quoted comma): read the shock from the end
      const fields = first.split(',');
      const shock = fields[fields.length - (cols.length - cols.indexOf('shock'))];
      expect(shock).toBe(diversifyShockIdOf({}));
      expect(shock).not.toBe(doc.meta.headlineShock);
    } finally {
      URL.createObjectURL = cu;
      URL.revokeObjectURL = ru ?? ((() => undefined) as typeof URL.revokeObjectURL);
      click.mockRestore();
    }
  });

  it('each node links to its compact (node section) and to the Sector Profile of the OSI cells its codes sit in', async () => {
    if (!has) return;
    renderAt('/aib/diversification');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    const list = screen.getByRole('list', { name: /^Supply-chain steps diversified to their benchmark/ });
    const shown = ran.slice(0, CHART_ROWS);
    const li = (id: string) => within(list).getAllByRole('listitem').find((x) => x.getAttribute('data-node') === id)!;
    const inCompact = shown.find((x) => x.node.compact);
    if (inCompact) expect(within(li(inCompact.node.id)).getByRole('link', { name: inCompact.node.compact! }).getAttribute('href')).toBe(`/compacts/${encodeURIComponent(inCompact.node.compact!)}#nodes`);
    const withCell = shown.find((x) => profiledCellsOfNode(x.node).length);
    if (withCell) {
      // the six-stage Sector profiles it opens, each labelled as that profile ("Batteries – Refining")
      const cs = profiledCellsOfNode(withCell.node);
      for (const c of cs) {
        expect(c.label).toBe(cellLabel(c.tech, c.stage));
        expect(within(li(withCell.node.id)).getByRole('link', { name: c.label }).getAttribute('href')).toBe(c.href);
        expect(c.href).toBe(`/sector/${c.slug.slice(0, -(c.stage.length + 1))}/${c.stage.toLowerCase()}`);
      }
      expect(li(withCell.node.id).textContent).toContain(cs.length === 1 ? ' · Sector profile: ' : ' · Sector profiles: ');
      expect(li(withCell.node.id).textContent).not.toMatch(/three stages|Upstream|Midstream|Downstream/);
    }
    const noCell = shown.find((x) => !profiledCellsOfNode(x.node).length);
    if (noCell) expect(li(noCell.node.id).textContent).not.toContain('Sector profile');
    expect(inCompact || withCell).toBeTruthy();
  });

  it('the overview points to the page; About describes the scenario with this export\'s counts', async () => {
    if (!has) return;
    renderAt('/aib');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    expect(within(screen.getByTestId('diversify-pointer')).getByRole('link', { name: /How Managed Interdependence works/ }).getAttribute('href')).toBe('/aib/diversification');
    cleanup();
    renderAt('/compacts/about#diversification');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Managed Interdependence' })).toBeTruthy();
    const about = screen.getByTestId('about-diversify');
    expect(about.textContent).toContain(HHI_WORDS);
    expect(about.textContent).toContain('falling back to the other nodes of its electro-industrial dictionary sector');
    expect(about.textContent).toContain("no supplier may then ship more than the cap times the allied market's demand");
    expect(about.textContent).toContain('its own purchases stay protected');
    expect(about.textContent).toContain('with the cap holding for every supplier in both');
    expect(about.textContent).toContain('not a forecast');
    expect(about.textContent).toContain(`${totals.above} of the ${totals.withBenchmark} supply-chain steps with a benchmark are more concentrated than their benchmark by more than a small tolerance`);
    expect(about.textContent).not.toMatch(/more concentrated than it\b/);
    expect(within(about).getByRole('link', { name: /Every node in the scenario/ }).getAttribute('href')).toBe('/aib/diversification');
    noAcronyms();
  });

  it('a compact page: "Diversification scenario" under each node that has it, worded per status; the class under the scenario reads "not run" where it did not run', async () => {
    if (!has) return;
    // the compacts that together show the most statuses (two at most)
    const statusesOf = (c: AibCompact) => new Set(c.nodes.map((id) => byId.get(id)?.diversify?.status).filter((s): s is string => !!s));
    const first = [...doc.compacts].sort((a, b) => statusesOf(b).size - statusesOf(a).size)[0];
    const second = [...doc.compacts].filter((c) => c !== first).sort((a, b) => [...statusesOf(b)].filter((s) => !statusesOf(first).has(s)).length - [...statusesOf(a)].filter((s) => !statusesOf(first).has(s)).length)[0];
    const seen = new Set<string>();
    for (const c of [first, second].filter(Boolean)) {
      cleanup();
      renderAt(`/compacts/${c.id}#nodes`);
      expect(await h1(titleOf(c))).toBeTruthy();
      const nodes = c.nodes.map((id) => byId.get(id)).filter((n): n is AibNode => !!n);
      const withD = nodes.filter((n) => n.diversify);
      const blocks = screen.queryAllByTestId('diversify-block');
      expect(blocks.map((b) => b.getAttribute('data-status'))).toEqual(withD.map((n) => n.diversify!.status));
      blocks.forEach((b, i) => {
        const row = diversifyRow(withD[i], shockId)!;
        seen.add(row.d.status);
        for (const w of diversifyWords(row, names, { coverage: false })) expect(b.textContent).toContain(w);
        expect(within(b).getByRole('heading', { name: /^Managed Interdependence · / }).textContent).toBe(`Managed Interdependence · ${rowStatusLabel(row).toLowerCase()}`);
        expect(b.textContent).not.toMatch(/than its sector|diversified as its sector/);
        // the class under the scenario names its physical-capability flag where it carries one
        if (row.clsWarning) expect(b.textContent).toContain(`(${AIB_PHYSICAL_WARNING})`);
        expect(within(b).queryByTestId('demand-split')).toBeNull();
        expect(within(b.closest('li') as HTMLElement).getByTestId('node-scenarios').querySelectorAll('[data-scenario]')).toHaveLength(3);
        expect(within(b).getByRole('link', { name: /How Managed Interdependence works/ }).getAttribute('href')).toBe('/aib/diversification');
      });
      // the variant tables: the scenario's class, "not run" where it is null
      const notRun = nodes.filter((n) => 'diversify_benchmark' in variantsOf(n) && variantsOf(n).diversify_benchmark === null).length;
      expect(screen.queryAllByTestId('variant-not-run')).toHaveLength(notRun);
      noAcronyms();
    }
    if (!process.env.AIB_DATA_DIR) expect([...seen].sort()).toEqual(['already as diversified as its benchmark', DIVERSIFIED, 'within the cap in the allied market']);
  });

  it('the landing: a node outside every compact shows the block in its robustness disclosure; a node the scenario did not assess shows none', async () => {
    if (!has) return;
    renderAt('/compacts#nodes');
    expect(await h1('Workstreams')).toBeTruthy();
    const outside = doc.nodes.filter((n) => !n.compact && n.inStrategicScope !== false && n.diversify !== undefined);
    const blocks = screen.queryAllByTestId('diversify-block');
    expect(blocks.map((b) => b.getAttribute('data-status')).sort()).toEqual(outside.map((n) => n.diversify!.status).sort());
    for (const b of blocks) {
      const n = outside.find((x) => b.textContent!.includes(diversifyWords(diversifyRow(x, shockId)!, names)[0]));
      expect(n).toBeTruthy();
    }
    if (!process.env.AIB_DATA_DIR) {
      // the synthetic fixture: PV modules already as diversified, the electrolysers without a benchmark; the transformers not assessed
      expect(blocks.map((b) => b.getAttribute('data-status')).sort()).toEqual(['already as diversified as its benchmark', 'no benchmark']);
      expect(blocks.some((b) => b.textContent!.includes('Transformers'))).toBe(false);
    }
    noAcronyms();
  });

  it('an older bundle without the scenario: the page says so, the overview has no pointer, the node pages no block', async () => {
    useSource((d) => {
      stripDiversify(d);
    });
    renderAt('/aib/diversification');
    expect(await h1('Managed Interdependence')).toBeTruthy();
    expect(await screen.findByText(/This build has no Managed Interdependence results/)).toBeTruthy();
    expect(screen.queryByRole('list', { name: 'Managed Interdependence totals' })).toBeNull();
    noAcronyms();
    cleanup();
    renderAt('/aib');
    expect(await h1('Allied Industrial Base')).toBeTruthy();
    expect(screen.queryByTestId('diversify-pointer')).toBeNull();
    cleanup();
    const c = doc.compacts[0];
    renderAt(`/compacts/${c.id}#nodes`);
    expect(await h1(titleOf(c))).toBeTruthy();
    expect(screen.queryAllByTestId('diversify-block')).toHaveLength(0);
    expect(screen.queryAllByTestId('variant-not-run')).toHaveLength(0);
    cleanup();
    renderAt('/compacts/about');
    expect(await h1('About Sectoral Compacts')).toBeTruthy();
    expect(screen.getByTestId('about-diversify').textContent).toContain('This export has no Managed Interdependence scenario.');
  });
});

describe('Home', () => {
  it('has the third entry card: the Allied Industrial Base, its links and its button', async () => {
    expect(MODULE_CARDS.map((c) => c.id)).toEqual(['sector', 'country', 'aib']);
    const card = MODULE_CARDS[2];
    expect(card).toMatchObject({ title: 'Explore the Allied Industrial Base', question: 'How should allies divide the work?', button: 'Explore the Allied Industrial Base' });
    expect(card.links.every((l) => l.to.startsWith('/compacts') || l.to.startsWith('/aib'))).toBe(true);
  });
});
