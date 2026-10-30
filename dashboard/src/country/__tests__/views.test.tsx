// Country Insights views on the R-written synthetic fixture (spec_country.md A4, A5, A12): every route
// renders (landing, a core profile, an EU core member without a memo, a coalition partner, an economy
// without a Strategic Index, a territory, an economy without index data, the memo, About, an unknown
// ISO3); the memo action and memo route follow the tier and memo state (core with a memo, core without,
// stale, kept under an earlier rule with its notice, non-core); every chart card has its table twin; the URL state (cell, partner, all) round-trips
// through clicks; no bare acronym in headings, legends, column headers, buttons or labels (C3.4). Six stages (2026-10-29): the
// strategy on the posture fixture (AAA served as USA), six-stage Sector links and legends, old three-stage cells in the link (U1),
// and no three-stage word on the profile. The canonical page (2026-10-29, spec XI-XIX, XXII UI; design 4, 5b): the head (one
// generated strategy sentence, data freshness, memo and export actions, no glance tiles), seven sections in order, the old
// anchors, the Legacy OSI framework out of the primary flow (Evidence & Methodology, in a closed disclosure, the dials as
// "Explore scenarios"), the Countries roster's six-verb line and the About page's framework (the page's sections are tested
// in depth in strategy.view.test.tsx).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import { setPostureStore } from '../../posture/store.ts';
import { fixtureStore } from '../../posture/__tests__/fixture.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import type { CountryEntry, CountryManifest } from '../contract.ts';
import { CountryStore, setCountryStoreForTests, type CountrySource } from '../data/store.ts';
import { STRATEGY_SECTIONS } from '../derive/strategyPage.ts';
import { fixtureSource, haveFixture, readManifest } from './fixture.ts';

const restore: (() => void)[] = [];
let source: ReturnType<typeof fixtureSource>;

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

/** The fixture source, optionally with a changed manifest (states the fixture does not carry). */
function useSource(edit?: (m: CountryManifest) => void): void {
  const base = fixtureSource();
  if (edit) {
    const m = structuredClone(readManifest());
    edit(m);
    const src: CountrySource & { calls: string[] } = {
      calls: base.calls,
      async manifest() {
        base.calls.push('manifest.json');
        return m;
      },
      json: base.json,
      url: base.url,
    };
    source = src;
  } else source = base;
  setCountryStoreForTests(new CountryStore(source));
}

/** An engine client that never becomes ready: a page that starts the model shows its loading state, and nothing else runs. */
const neverReady = () => new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/');
let engine: EngineClient;

// the posture bundle's fixture with its economy AAA served as the United States (the strategy is tested with data; every
// other economy has no posture)
beforeEach(() => {
  useSource();
  setPostureStore(fixtureStore({ economies: { AAA: { iso3: 'USA', name: 'United States' } } }));
  engine = neverReady();
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

// 30 s: the first render compiles every lazy route module (slow under load)
const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, { timeout: 30_000 });
const main = () => document.querySelector('main') as HTMLElement;
const noAcronyms = () => expect(bareAcronyms(document.body)).toEqual([]);

/** Open every table twin of the page and return the number of cards that have one. */
function openTwins(): number {
  const buttons = within(main()).queryAllByRole('button', { name: 'Table' });
  for (const b of buttons) fireEvent.click(b);
  return buttons.length;
}

/** Open the Legacy OSI results (Evidence & Methodology -> Legacy OSI framework). */
async function openLegacy(name: string): Promise<HTMLElement> {
  fireEvent.click(screen.getByText(new RegExp(`^Show the Legacy OSI results for ${name}`)));
  return screen.findByRole('heading', { level: 3, name: 'Security and opportunity' }, { timeout: 30_000 });
}

/** Open the generic partnership material under Allied Strategy. */
async function openExplore(): Promise<void> {
  fireEvent.click(screen.getByText('Explore all partnership analysis'));
  await screen.findByRole('heading', { level: 3, name: 'Partnerships' }, { timeout: 30_000 });
}

describe.skipIf(!haveFixture())('Country Insights routes on the synthetic fixture', () => {
  it('landing: the lists, memo tags, the six-verb roster line and the reading guide; no fact sheet is fetched', async () => {
    renderAt('/countries');
    expect(await h1('Countries')).toBeTruthy();
    expect(document.title).toMatch(/^Countries \| Allied Industrial Base Explorer/);
    const core = screen.getByRole('region', { name: /Core coalition members/ });
    const usa = within(core).getByRole('link', { name: /United States/ });
    expect(usa.getAttribute('href')).toBe('/countries/USA');
    expect(within(usa).getByText('MEMO')).toBeTruthy();
    expect(within(within(core).getByRole('link', { name: /Germany/ })).queryByText('MEMO')).toBeNull();
    expect(screen.getByRole('region', { name: /Coalition partners/ })).toBeTruthy();
    expect(screen.getByRole('combobox', { name: /Choose a country/ })).toBeTruthy();
    // the roster line is the strategy's six verbs (the posture fixture's United States), not the legacy corners
    await waitFor(() => expect(within(usa).getByTestId('roster-line').textContent).toBe('3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch'), { timeout: 30_000 });
    expect(within(within(core).getByRole('link', { name: /Germany/ })).getByTestId('roster-line').textContent).toBe('No strategy map');
    expect(core.textContent).not.toMatch(/Grow \d+ · Upgrade/);
    // the reading guide leads with the six choices; the corners are the Legacy OSI framework with their mapping
    expect(within(screen.getByTestId('reading-verbs')).getAllByRole('listitem').map((li) => li.textContent?.split(':')[0])).toEqual(['Secure', 'Preserve', 'Catch Up', 'Partner', 'Accelerate', 'Watch']);
    const corners = within(screen.getByTestId('reading-corners')).getAllByRole('listitem').slice(0, 4).map((li) => li.textContent ?? '');
    expect(corners.map((t) => /conceptually close to (\w[\w ]*)\./.exec(t)?.[1])).toEqual(['Preserve', 'Catch Up', 'Partner', 'Watch']);
    expect(source.calls.every((c) => c === 'manifest.json')).toBe(true);
    noAcronyms();
  });

  it('core profile with a memo (United States): the head, the seven sections, actions, disclosures, table twins, no bare acronym', async () => {
    const r = renderAt('/countries/USA');
    expect(await h1('United States')).toBeTruthy();
    // the title effect can trail the h1 when the suite runs under load
    await waitFor(() => expect(document.title).toMatch(/United States \| Countries \| /));
    // the head: one generated strategy sentence (spec XI.2, XI.7), the data freshness, the memo and export actions
    const sentence = screen.getByTestId('strategy-sentence');
    await waitFor(() => expect(sentence.textContent).toMatch(/^The United States should preserve its strongest positions in battery pack assembly and solar module assembly; secure /), { timeout: 30_000 });
    expect(sentence.textContent).toMatch(/; catch up in battery cell manufacturing; rely on partners for lithium and nickel mining; and accelerate deployment of battery storage\.$/);
    expect(screen.getByTestId('data-freshness').textContent).toBe(
      'Data: Strategy built 29 October 2026 · trade benchmark 2022-2024 · trade momentum through Aug 2026 · country facts as of 12 October 2026.',
    );
    // the "at a glance" tiles are gone (spec XI.2): no corners, Strategic Index, partner or climate-rating tile
    for (const t of ['STRATEGIC POSTURE', 'WHERE ITS SECTORS SIT', 'TOP STRATEGIC INDEX SECTOR', 'STRONGEST PARTNER', 'CLIMATE ACTION TRACKER']) expect(screen.queryByText(t)).toBeNull();
    expect(document.getElementById('glance')?.tagName).toBe('SPAN');
    const actions = screen.getByTestId('profile-actions');
    expect(actions.id).toBe('memo');
    const memo = screen.getAllByRole('link', { name: 'GENERATE MEMO' });
    expect(memo).toHaveLength(1); // the header action only: the memo is not a section any more
    expect(within(actions).getByRole('link', { name: 'GENERATE MEMO' }).getAttribute('href')).toBe('/countries/USA/memo');
    expect(within(actions).getByText(/AI-generated from the data and checked against it; NEIS baseline as of 12 October 2026\./)).toBeTruthy();
    await waitFor(() => expect((within(actions).getByRole('button', { name: 'DOWNLOAD STRATEGY (CSV)' }) as HTMLButtonElement).disabled).toBe(false));
    expect(within(actions).getByRole('link', { name: /All data and downloads/ }).getAttribute('href')).toBe('#downloads');
    expect(screen.queryByRole('heading', { name: 'Policy memo' })).toBeNull();
    // the nav: the seven sections in the spec's order, each a section with that h2
    const nav = screen.getByRole('navigation', { name: 'Sections of this page' });
    const links = within(nav).getAllByRole('link');
    expect(links.map((a) => a.getAttribute('href'))).toEqual(['#strategy', '#need', '#prize-position', '#choices', '#policy-alignment', '#allied', '#evidence']);
    expect(links.map((a) => a.textContent)).toEqual(['Strategy', 'Need', 'Prize & Position', 'Choices', 'Policy Alignment', 'Allied Strategy', 'Evidence & Methodology']);
    for (const s of STRATEGY_SECTIONS) {
      const el = document.getElementById(s.id)!;
      expect(el.tagName, s.id).toBe('SECTION');
      expect(within(el).getAllByRole('heading', { level: 2 })[0].textContent, s.id).toBe(s.label);
    }
    // the sections in that order, the Strategy section first
    const column = document.getElementById('strategy')!.parentElement!;
    expect([...column.children].map((c) => c.id)).toEqual(STRATEGY_SECTIONS.map((s) => s.id));
    // the Country Strategy Map: the dominant visual of the Strategy section, with its counts, table twin and CSV
    const strategy = document.getElementById('strategy')!;
    expect(strategy.querySelector('[data-basis="posture"]')?.textContent).toMatch(/^Strategic position framework/);
    expect(strategy.textContent).toMatch(/then recommends one choice: Secure, Preserve, Catch Up, Partner, Accelerate or Watch/);
    const card = await within(strategy).findByRole('region', { name: 'Country Strategy Map' }, { timeout: 30_000 });
    expect(within(card).getByTestId('strategy-counts').textContent).toMatch(/3 Secure · 2 Preserve · 1 Catch Up · 1 Partner · 1 Accelerate · 3 Watch\. 3 Secure · 4 Compete · 4 Deploy · 1 not assessed\./);
    expect(within(card).queryByTestId('posture-takeaway')).toBeNull(); // the sentence is the page head's
    expect(within(card).getByRole('button', { name: 'Table' })).toBeTruthy();
    expect(within(card).getByRole('button', { name: 'Download CSV' })).toBeTruthy();
    // a node opens the page's one drawer through ?node=, linking to its six-stage Sector profile
    fireEvent.click(within(strategy).getByRole('button', { name: /^Batteries Component Manufacturing/ }));
    await waitFor(() => expect(new URLSearchParams(r.state.location.search).get('node')).toBe('Batteries|Components'));
    const drawer = await screen.findByTestId('cell-drawer', {}, { timeout: 30_000 });
    expect(within(drawer).getByRole('heading', { level: 3 }).textContent).toBe('Batteries · Component Manufacturing');
    await waitFor(() => expect(within(drawer).getByTestId('cell-drawer-sector-link').getAttribute('href')).toBe('/sector/batteries/components?c=USA#posture'), { timeout: 30_000 });
    fireEvent.click(within(drawer).getByRole('button', { name: 'Close the detail' }));
    await waitFor(() => expect(r.state.location.search).not.toMatch(/node=/));
    expect(screen.queryByTestId('cell-drawer')).toBeNull();
    // old anchors still exist (alias spans), so old links scroll to the block's new place
    for (const id of ['posture', 'glance', 'position', 'space', 'opportunities', 'sectors', 'si', 'policy', 'priorities', 'partnerships', 'partners', 'aib-role', 'memo', 'notes']) {
      expect(document.getElementById(id), id).toBeTruthy();
    }
    // the Legacy OSI framework is out of the primary flow: no Perspective control, no corners scatter, no Strategic Index list
    // above Evidence & Methodology; its sub-section sits there with the mapping, and its results are closed
    expect(screen.queryByRole('button', { name: /^Perspective:/ })).toBeNull();
    expect(screen.queryByRole('heading', { name: 'Security and opportunity' })).toBeNull();
    const legacy = document.getElementById('legacy-osi')!;
    expect(legacy.closest('section#evidence')).toBeTruthy();
    expect(within(legacy).getByRole('heading', { level: 3, name: 'Legacy OSI framework (paper baseline)' })).toBeTruthy();
    expect(within(legacy).getByText('Not a current recommendation')).toBeTruthy();
    expect(within(within(legacy).getByTestId('legacy-mapping')).getAllByRole('row').slice(1).map((tr) => tr.querySelector('td')?.textContent)).toEqual([
      '≈ Preserve', '≈ Catch Up', '≈ Partner', '≈ Watch',
    ]);
    expect(within(legacy).getByTestId('legacy-reading').textContent).toMatch(/Where its sectors sit in the four corners: Grow 36 · Upgrade 0 · Exposed 9 · Stable 13\./);
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(false);
    // the engine never starts on a visit that does not open the Legacy OSI model
    expect(engine.getSnapshot().status).toBe('starting');
    expect(engine.getSnapshot().manifest).toBeNull();
    await openLegacy('the United States');
    expect(screen.getByRole('heading', { level: 3, name: 'Explore scenarios' })).toBeTruthy();
    expect(screen.getByRole('button', { name: /^Perspective: NEIS baseline/ }).getAttribute('aria-expanded')).toBe('false');
    expect(screen.getAllByText('Observed / modelled facts', { exact: false }).length).toBeGreaterThan(0);
    expect(screen.getAllByText('Preference-adjusted priorities', { exact: false }).length).toBeGreaterThan(0);
    // Sector position rows link to the six-stage Sector profiles
    const sectors = document.getElementById('legacy-sectors')!;
    const sectorLinks = within(sectors).getAllByRole('link').map((a) => a.getAttribute('href') ?? '').filter((h) => h.startsWith('/sector/'));
    expect(sectorLinks.length).toBeGreaterThan(0);
    for (const h of sectorLinks) expect(h).toMatch(/^\/sector\/[a-z-]+\/(extraction|refining|components|assembly|deployment|recovery)\?c=USA$/);
    // six stages everywhere (design S1): no three-stage word or paper qualifier on the page
    expect(main().textContent).not.toMatch(/upstream|midstream|downstream|three-stage|paper's/i);
    // the corners scatter: the six stage shapes in its legend
    expect(within(screen.getByRole('list', { name: 'How to read the dots' })).getAllByRole('listitem').slice(0, 6).map((li) => li.textContent)).toEqual([...SUPPLY_STAGES]);
    // the top Strategic Index sector of an all-Grow country: the fallback rule (A9.5) explains its top three (behind "Why these sectors")
    fireEvent.click(screen.getByText(/^Why these sectors/));
    expect(await screen.findByText(/None of the United States' placed sectors is in the Upgrade corner/)).toBeTruthy();
    // the generic partnership material, under Allied Strategy: the supplier matrix groups its columns by the six stages
    await openExplore();
    fireEvent.click(screen.getByText(/^Who supplies the United States in each sector today/));
    const matrix = await screen.findByRole('region', { name: /^Who supplies the United States in each sector: a table that scrolls sideways/ });
    const groups = [...matrix.querySelectorAll('th[scope="colgroup"]')].map((th) => th.getAttribute('title'));
    expect(groups.length).toBeGreaterThan(1);
    expect(groups).toEqual(SUPPLY_STAGES.filter((s) => groups.includes(s)));
    // the observed-policy evidence under Policy Alignment: the industrial-policy measures card says it is context and names the
    // extract (behind a disclosure). Six stages (design K1): the NIPO run is not matched to the six stages (staged false), so the
    // card gives the economy's counts and says why, with no sector list, picker, table or CSV
    const policy = document.getElementById('policy-alignment')!;
    fireEvent.click(within(policy).getByText(/^Policy records by technology and stage, and the United States' active industrial-policy measures/));
    const nipoCard = await screen.findByRole('region', { name: 'Active industrial-policy measures' });
    expect(screen.queryByRole('heading', { name: 'Strongest active industrial-policy measures' })).toBeNull();
    expect(within(nipoCard).getByTestId('nipo-not-staged').textContent).toBe(
      "Global Trade Alert's measures are not yet matched to the six supply-chain stages, so they are counted for the whole economy and not listed by sector.",
    );
    expect(within(nipoCard).getByText('Active measures').nextElementSibling?.textContent).toBe('5');
    expect(within(nipoCard).queryByRole('combobox')).toBeNull();
    expect(within(nipoCard).queryByRole('button', { name: 'Table' })).toBeNull();
    expect(within(nipoCard).queryByRole('button', { name: 'Download CSV' })).toBeNull();
    expect(screen.getAllByText(/not part of the Policy index/).length).toBeGreaterThan(0);
    // the coverage grid: 11 technologies x the six stages
    const cov = screen.getByRole('region', { name: 'Policy records by technology and stage' });
    expect(within(within(cov).getByRole('table')).getAllByRole('columnheader').slice(1).map((h) => h.textContent)).toEqual([...SUPPLY_STAGES]);
    noAcronyms();
    // every chart card has a table twin; opening them all keeps the page free of bare acronyms
    const n = openTwins();
    expect(n).toBeGreaterThanOrEqual(8);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(n);
    noAcronyms();
    expect(source.calls.filter((c) => c.startsWith('facts-'))).toHaveLength(1);
  }, 120_000);

  it('EU core member without a memo (Germany): no memo button, being prepared, EU tags and the EU rating', async () => {
    renderAt('/countries/DEU');
    expect(await h1('Germany')).toBeTruthy();
    const actions = await screen.findByTestId('profile-actions', {}, { timeout: 30_000 });
    expect(screen.queryByRole('link', { name: 'GENERATE MEMO' })).toBeNull();
    expect(within(actions).getByText('The memo for Germany is being prepared.')).toBeTruthy();
    expect(screen.getByText('EU MEMBER')).toBeTruthy();
    // the EU's climate rating, in the observed-policy evidence under Policy Alignment
    expect(within(document.getElementById('policy-alignment')!).getAllByText(/the EU's rating/i).length).toBeGreaterThan(0);
    noAcronyms();
  });

  it('coalition partner (Brazil): the core-only memo text with a link to the list', async () => {
    renderAt('/countries/BRA');
    expect(await h1('Brazil')).toBeTruthy();
    const actions = await screen.findByTestId('profile-actions', {}, { timeout: 30_000 });
    expect(within(actions).getByText(/Memos are available for the 5 core coalition members for now\./)).toBeTruthy();
    expect(within(actions).getByRole('link', { name: /See the list/ }).getAttribute('href')).toBe('/countries#core');
    expect(screen.getByText('PARTNER')).toBeTruthy();
    noAcronyms();
  });

  it('no Strategic Index (Nigeria): the Legacy OSI notice replaces the priorities; the policy evidence says so in words', async () => {
    renderAt('/countries/NGA');
    expect(await h1('Nigeria')).toBeTruthy();
    await screen.findByRole('heading', { level: 2, name: 'Policy Alignment' }, { timeout: 30_000 });
    expect(within(screen.getByTestId('legacy-reading')).getByText(/so no Strategic Index is computed for it/)).toBeTruthy();
    expect(screen.queryByRole('heading', { name: 'Which sectors the Strategic Index puts first' })).toBeNull();
    expect(screen.getByText('POLICY: RECORDS ONLY, NO CLIMATE RATING')).toBeTruthy();
    expect(within(document.getElementById('policy-alignment')!).getByText(/Nigeria has no Climate Action Tracker rating/)).toBeTruthy();
    await openLegacy('Nigeria');
    expect(screen.getAllByText('No Strategic Index').length).toBeGreaterThan(0);
    expect(screen.getAllByText(/so no Strategic Index is computed for it/).length).toBeGreaterThan(1);
    noAcronyms();
  });

  it('territory (Puerto Rico): scores shown but not ranked (Legacy OSI); the territory tag', async () => {
    renderAt('/countries/PRI');
    expect(await h1('Puerto Rico')).toBeTruthy();
    const notice = await screen.findByText('Scores shown but not ranked', undefined, { timeout: 30_000 });
    expect(notice.closest('#legacy-osi')).toBeTruthy();
    expect(screen.getByText('TERRITORY')).toBeTruthy();
    noAcronyms();
    await openLegacy('Puerto Rico');
    openTwins();
    noAcronyms();
  });

  it('an economy without index data: the profile shell with a notice and the Sector Insights link', async () => {
    useSource((m) => {
      const e: CountryEntry = {
        iso3: 'ATA', name: 'Antarctica', tier: 'none', region: null, eu_member: false, status: 'no_data', file: null, bytes_gz: null,
        n_placed: null, legacy_corners: null, memo: null, drivers: null, iso_n3: null,
      };
      m.countries.push(e);
      m.counts.economies += 1;
    });
    renderAt('/countries/ATA');
    expect(await h1('Antarctica')).toBeTruthy();
    expect(screen.getByText('No index data')).toBeTruthy();
    expect(screen.getByRole('link', { name: /raw data in Sectors/ }).getAttribute('href')).toMatch(/^\/sector\?c=ATA/);
    expect(source.calls.some((c) => c.startsWith('facts-'))).toBe(false);
    noAcronyms();
  });

  it('unknown ISO3: the landing with a route notice', async () => {
    renderAt('/countries/XYZ');
    expect(await h1('Countries')).toBeTruthy();
    expect(screen.getByText("No country with the code 'XYZ'; showing all countries.")).toBeTruthy();
  });

  it('memo of a core member: the verbatim label, the downloads, the sources and the trace switch', async () => {
    renderAt('/countries/USA/memo');
    expect(await h1(/^United States: energy security, economic opportunity and partnership priorities$/)).toBeTruthy();
    expect(document.title).toMatch(/United States memo \| Countries \| /);
    // the label under the title and again at the end of the memo
    expect(screen.getAllByText('AI-generated from the data and checked against it; NEIS baseline as of 12 October 2026.').length).toBe(2);
    const pdf = screen.getByText('DOWNLOAD PDF');
    const docx = screen.getByText('DOWNLOAD WORD (.DOCX)');
    const entry = readManifest().countries.find((c) => c.iso3 === 'USA')!;
    if (entry.memo?.pdf) {
      expect(pdf.closest('a')?.getAttribute('href')).toBe(`/data/country/${entry.memo.pdf}`);
      expect(pdf.closest('a')?.getAttribute('download')).toBe('United-States-memo-2026-10-12.pdf');
    } else expect(pdf.getAttribute('aria-disabled')).toBe('true');
    if (entry.memo?.docx) expect(docx.closest('a')?.getAttribute('href')).toBe(`/data/country/${entry.memo.docx}`);
    else expect(docx.getAttribute('aria-disabled')).toBe('true');
    expect(screen.getByRole('heading', { level: 2, name: 'Summary' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: '1. Energy security risks and why' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: '5. Data notes and caveats' })).toBeTruthy();
    expect(screen.getByRole('heading', { level: 2, name: 'Sources' })).toBeTruthy();
    expect(document.getElementById('src-1')).toBeTruthy();
    expect(document.querySelectorAll('[data-path]').length).toBe(0);
    fireEvent.click(screen.getByRole('switch', { name: /Show where each number comes from/ }));
    await waitFor(() => expect(document.querySelectorAll('[data-path]').length).toBeGreaterThan(0));
    expect(screen.getByRole('link', { name: /Back to United States/ }).getAttribute('href')).toBe('/countries/USA');
    noAcronyms();
  });

  it('memo routes of a partner and of a core member without a memo go to the profile with a notice', async () => {
    const r = renderAt('/countries/BRA/memo');
    expect(await h1('Brazil')).toBeTruthy();
    expect(screen.getAllByText(/Memos are available for the 5 core coalition members for now\./).length).toBeGreaterThan(0);
    expect(r.state.location.pathname).toBe('/countries/BRA');
    cleanup();
    const r2 = renderAt('/countries/DEU/memo');
    expect(await h1('Germany')).toBeTruthy();
    expect(screen.getAllByText('The memo for Germany is being prepared.').length).toBeGreaterThan(0);
    expect(r2.state.location.pathname).toBe('/countries/DEU');
  });

  it('a stale memo is not shown', async () => {
    useSource((m) => {
      const e = m.countries.find((c) => c.iso3 === 'USA')!;
      e.memo = { ...e.memo!, stale: true };
    });
    renderAt('/countries/USA/memo');
    expect(await screen.findByText('This memo is being revised. It will be available again once it has been rewritten.', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(source.calls.some((c) => c.startsWith('memo-'))).toBe(false);
    cleanup();
    renderAt('/countries/USA');
    expect(await h1('United States')).toBeTruthy();
    const actions = await screen.findByTestId('profile-actions', {}, { timeout: 30_000 });
    expect(screen.queryByRole('link', { name: 'GENERATE MEMO' })).toBeNull();
    // GENERATE MEMO is there, disabled, and says why (the header action, the only memo control on the page)
    const buttons = screen.getAllByRole('button', { name: 'GENERATE MEMO' }) as HTMLButtonElement[];
    expect(buttons.length).toBe(1);
    expect(buttons[0].disabled).toBe(true);
    expect(within(actions).getByText('The memo for the United States is being revised. It will be available again once it has been rewritten.')).toBeTruthy();
  });

  it('a memo kept under the earlier quadrant rule is shown with its notice, on the memo page and in the profile', async () => {
    const note = "Written on 14 October 2026, when the Grow, Upgrade, Secure and Watch corners split each sector at the median economy. This page now splits at the United States' own median sector, so the memo's statements about corners may differ from the chart. Its other figures match the current data.";
    useSource((m) => {
      const e = m.countries.find((c) => c.iso3 === 'USA')!;
      e.memo = { ...e.memo!, stale: false, rule_note: 'quadrant_rule_2026_10_15', rule_note_d: note };
    });
    renderAt('/countries/USA/memo');
    expect(await screen.findByText(note, undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText('Written under an earlier corner rule')).toBeTruthy();
    cleanup();
    renderAt('/countries/USA');
    expect(await h1('United States')).toBeTruthy();
    const actions = await screen.findByTestId('profile-actions', {}, { timeout: 30_000 });
    expect(screen.getAllByRole('link', { name: 'GENERATE MEMO' }).length).toBe(1);
    expect(within(actions).getByTestId('memo-rule-note').textContent).toBe(note);
  });

  it('about: the framework first, the Legacy OSI sections after it, the glossary and the sources', async () => {
    renderAt('/countries/about');
    expect(await h1('Country profiles: data notes')).toBeTruthy();
    const heads = [
      'What the Country profiles show', 'The strategy: need, prize, position, choice, policy alignment', 'The Legacy OSI framework', 'Legacy OSI: reading the four corners',
      'How the strategy is compared with policy', 'Legacy OSI: how priorities are compared with policy', "How partners are compared with today's ties", 'The memos', 'Data and vintage', 'Glossary',
    ];
    for (const h of heads) expect(screen.getAllByRole('heading', { name: new RegExp(h.replace(/[()?]/g, '.')) }).length, h).toBeGreaterThan(0);
    const order = screen.getAllByRole('heading', { level: 2 }).map((x) => x.textContent ?? '');
    expect(order.indexOf('The strategy: need, prize, position, choice, policy alignment')).toBeLessThan(order.indexOf('Legacy OSI: reading the four corners'));
    expect(main().textContent).toMatch(/Grow ≈ Preserve, Upgrade ≈ Catch Up, Exposed ≈ Partner, Stable ≈ Watch/);
    // Watch has both routes: low need with a low prize, and low need with a high prize but neither a strong position nor a credible pathway
    expect(main().textContent).toContain('Watch where the need is low, with a low prize and also with a high prize where there is neither a strong position nor a credible pathway (a positive conclusion');
    expect(main().textContent).not.toMatch(/Watch where need and prize are both low/);
    expect(main().textContent).toMatch(/mix capability, vulnerability, production, trade, investment, demand, reserves, concentration and deployment/);
    expect(screen.getAllByRole('link', { name: /glossary/i }).some((a) => a.getAttribute('href')?.startsWith('/glossary'))).toBe(true);
    noAcronyms();
  });

  it('URL state: a bad cell is reported; picking a sector and a partner writes cell and partner', async () => {
    const r = renderAt('/countries/JPN?cell=fusion-extraction&partner=ZZZ');
    expect(await h1('Japan')).toBeTruthy();
    expect(await screen.findByText('Ignored in the link', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/cell=fusion-extraction \(not a sector\)/)).toBeTruthy();
    // a sector chip of the four-corners card (Legacy OSI) selects the sector
    await openLegacy('Japan');
    const corners = screen.getByRole('region', { name: 'The four corners' });
    // the app's corner names (2026-10-28)
    const names = [...within(corners).getByRole('table').querySelectorAll('tbody tr')].map((tr) => (tr.querySelector('td')?.textContent ?? '').trim().split(/\s/)[0]);
    expect(names).toEqual(['Grow', 'Upgrade', 'Exposed', 'Stable']);
    const chip = within(corners).getAllByRole('button')[0];
    fireEvent.click(chip);
    await waitFor(() => expect(r.state.location.search).toMatch(/^\?cell=[a-z-]+/));
    // a partner row of the slope chart (behind "Every partner", in the partnership analysis under Allied Strategy) selects the partner
    await openExplore();
    fireEvent.click(screen.getByText(/^Every partner of Japan/));
    const slope = await screen.findByRole('region', { name: "Priority partners and today's partners" });
    const row = within(slope).getAllByRole('button').find((b) => /United States|Germany|South Korea|France/.test(b.textContent ?? ''));
    expect(row).toBeTruthy();
    fireEvent.click(row!);
    await waitFor(() => expect(r.state.location.search).toMatch(/partner=[A-Z]{3}/));
  });

  it('URL state: an old three-stage cell (design U1) is mapped (Downstream) or dropped (Upstream, Midstream) with a notice', async () => {
    renderAt('/countries/JPN?cell=wind-downstream');
    expect(await h1('Japan')).toBeTruthy();
    expect(await screen.findByText('Ignored in the link', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/cell=wind-downstream \(Wind — Downstream is now Wind — Deployment; showing it\)/)).toBeTruthy();
    // the mapped sector is the selected one (its "why" link under the Legacy OSI scatter, when it is placed)
    const jpn = readManifest().countries.find((c) => c.iso3 === 'JPN')!;
    const sheet = JSON.parse(JSON.stringify(await source.json(jpn.file!))) as { cells: { key: string; placed: boolean }[] };
    await openLegacy('Japan');
    if (sheet.cells.find((c) => c.key === 'wind-deployment')?.placed) expect(screen.getByRole('button', { name: /^Why Wind — Deployment\?/ })).toBeTruthy();
    cleanup();
    renderAt('/countries/JPN?cell=batteries-midstream');
    expect(await h1('Japan')).toBeTruthy();
    expect(await screen.findByText(/cell=batteries-midstream \(Batteries — Midstream is no longer a stage: .*Components and Assembly.*; the sector was dropped\)/, undefined, { timeout: 30_000 })).toBeTruthy();
  });

  it('an economy without a posture keeps every section, each with its notice; the head says there is no strategy map', async () => {
    renderAt('/countries/DEU');
    expect(await h1('Germany')).toBeTruthy();
    const strategy = (await screen.findByRole('heading', { level: 2, name: 'Strategy' }, { timeout: 30_000 })).closest('section')!;
    expect(strategy.id).toBe('strategy');
    expect(await within(strategy).findByText('No posture', undefined, { timeout: 30_000 })).toBeTruthy();
    await waitFor(() => expect(screen.getByTestId('strategy-sentence').textContent).toBe('Germany has no strategy map: the strategic position framework classifies none of its technology and stage nodes.'));
    for (const id of ['need', 'prize-position', 'choices', 'policy-alignment', 'allied']) expect(within(document.getElementById(id)!).getByText('No strategy map'), id).toBeTruthy();
    // the facts that do not need the framework stay: the observed policy and the partnership analysis
    expect(within(document.getElementById('policy-alignment')!).getByTestId('policy-behaviour')).toBeTruthy();
    expect(within(document.getElementById('allied')!).getByText('Explore all partnership analysis')).toBeTruthy();
    expect((screen.getByRole('button', { name: 'DOWNLOAD STRATEGY (CSV)' }) as HTMLButtonElement).disabled).toBe(true);
    noAcronyms();
  });

  it('priorities table: an old #why link opens the Legacy OSI results and its disclosure; "Show all" writes all=1 and lists every in-sample sector', async () => {
    const r = renderAt('/countries/KOR#why');
    expect(await h1('South Korea')).toBeTruthy();
    // an old #why link opens its disclosures (the Legacy OSI results, then Why these sectors)
    await waitFor(() => expect(document.getElementById('fold-why')?.hasAttribute('open')).toBe(true), { timeout: 30_000 });
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(true);
    fireEvent.click(screen.getByText(/^How priorities are built/));
    const card = await screen.findByRole('region', { name: 'Which sectors the Strategic Index puts first' }, { timeout: 30_000 });
    const before = within(card).getAllByRole('row').length;
    const btn = within(card).queryByRole('button', { name: /^Show all \d+$/ });
    if (btn) {
      fireEvent.click(btn);
      await waitFor(() => expect(r.state.location.search).toMatch(/all=1/));
      expect(within(card).getAllByRole('row').length).toBeGreaterThan(before);
    }
  });

  it('old anchors land in their new place: #posture -> Strategy, #aib-role opens the partnership analysis, #sectors the Legacy OSI results', async () => {
    renderAt('/countries/USA#aib-role');
    expect(await h1('United States')).toBeTruthy();
    await waitFor(() => expect(document.getElementById('fold-partnerships-explore')?.hasAttribute('open')).toBe(true), { timeout: 30_000 });
    expect(document.getElementById('allied-aib-role')?.closest('section#allied')).toBeTruthy();
    expect(within(document.getElementById('allied-aib-role')!).getByRole('heading', { level: 3, name: 'Role in the Allied Industrial Base' })).toBeTruthy();
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(false);
    cleanup();
    renderAt('/countries/USA#sectors');
    expect(await h1('United States')).toBeTruthy();
    await waitFor(() => expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(true), { timeout: 30_000 });
    expect(await screen.findByRole('heading', { level: 3, name: 'Sector position' }, { timeout: 30_000 })).toBeTruthy();
    cleanup();
    renderAt('/countries/USA#posture');
    expect(await h1('United States')).toBeTruthy();
    expect(document.getElementById('posture')?.closest('section')?.id).toBe('strategy');
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(false);
  });

  it('a link with Perspective dials opens the Legacy OSI results, where the dials apply, and says so; the strategy does not follow them', async () => {
    renderAt('/countries/USA?h=80');
    expect(await h1('United States')).toBeTruthy();
    expect(await screen.findByText('A custom perspective in the link', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(document.getElementById('fold-legacy')?.hasAttribute('open')).toBe(true);
    expect(screen.getByTestId('legacy-custom-link')).toBeTruthy();
    // the strategy sentence is the same as without the dials
    await waitFor(() => expect(screen.getByTestId('strategy-sentence').textContent).toMatch(/^The United States should preserve its strongest positions/), { timeout: 30_000 });
    // the model loads for the Legacy OSI results only (it never becomes ready here): the strategy sections render meanwhile
    expect(within(document.getElementById('strategy')!).getByRole('region', { name: 'Country Strategy Map' })).toBeTruthy();
  });
});
