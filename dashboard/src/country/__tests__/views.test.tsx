// Country Insights views on the R-written synthetic fixture (spec_country.md A4, A5, A12): every route
// renders (landing, a core profile, an EU core member without a memo, a coalition partner, an economy
// without a Strategic Index, a territory, an economy without index data, the memo, About, an unknown
// ISO3); the memo button and memo route follow the tier and memo state (core with a memo, core without,
// stale, kept under an earlier rule with its notice, non-core); every chart card has its table twin; the URL state (cell, partner, all) round-trips
// through clicks; no bare acronym in headings, legends, column headers, buttons or labels (C3.4).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import type { CountryEntry, CountryManifest } from '../contract.ts';
import { CountryStore, setCountryStoreForTests, type CountrySource } from '../data/store.ts';
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

beforeEach(() => useSource());

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
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

describe.skipIf(!haveFixture())('Country Insights routes on the synthetic fixture', () => {
  it('landing: the lists, memo tags and the reading guide; no fact sheet is fetched', async () => {
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
    expect(source.calls.every((c) => c === 'manifest.json')).toBe(true);
    noAcronyms();
  });

  it('core profile with a memo (United States): every section, actions, disclosures, table twins, no bare acronym', async () => {
    renderAt('/countries/USA');
    expect(await h1('United States')).toBeTruthy();
    // the title effect can trail the h1 when the suite runs under load
    await waitFor(() => expect(document.title).toMatch(/United States \| Countries \| /));
    // the paper's three-stage scatter is 'Security and opportunity' since 2026-10-28 ('Strategic position' names the posture)
    await screen.findByRole('heading', { level: 2, name: 'Security and opportunity' }, { timeout: 30_000 });
    expect(screen.queryByRole('heading', { level: 2, name: 'Strategic position' })).toBeNull();
    for (const h of ['Opportunities & risks', 'Sector position', 'Policy & climate', 'Partnerships', 'Role in the Allied Industrial Base', 'Policy memo', 'Data notes']) {
      expect(screen.getAllByRole('heading', { level: 2, name: h }).length).toBeGreaterThan(0);
    }
    for (const h of ['Make bets', 'Avoid dangerous dependencies', 'Capabilities to keep']) expect(screen.getByRole('heading', { level: 3, name: h })).toBeTruthy();
    const memo = screen.getAllByRole('link', { name: 'GENERATE MEMO' });
    expect(memo.length).toBe(2); // the header action and the memo section
    for (const a of memo) expect(a.getAttribute('href')).toBe('/countries/USA/memo');
    expect(screen.getByText(/AI-generated from the data and checked against it; paper baseline as of 12 October 2026\./)).toBeTruthy();
    // the dials live on the page now: no bridge to Strategic Insights, a closed Perspective control
    expect(screen.queryByRole('link', { name: 'OPEN IN STRATEGIC INSIGHTS' })).toBeNull();
    expect(screen.getByRole('button', { name: /^Perspective: NEIS baseline/ }).getAttribute('aria-expanded')).toBe('false');
    expect(screen.getAllByText('Observed / modelled facts', { exact: false }).length).toBeGreaterThan(0);
    expect(screen.getAllByText('Preference-adjusted priorities', { exact: false }).length).toBeGreaterThan(0);
    const nav = screen.getByRole('navigation', { name: 'Sections of this page' });
    expect(within(nav).getAllByRole('link').map((a) => a.getAttribute('href'))).toEqual(['#posture', '#position', '#opportunities', '#sectors', '#policy', '#partnerships', '#aib-role', '#memo', '#notes']);
    expect(within(nav).getAllByRole('link').slice(0, 2).map((a) => a.textContent)).toEqual(['Strategic posture', 'Security and opportunity']);
    // the posture chart's wrapper is the #posture anchor
    expect(document.getElementById('posture')).toBeTruthy();
    // old anchors still exist
    for (const id of ['space', 'priorities', 'partners', 'aib-role']) expect(document.getElementById(id)).toBeTruthy();
    // Sector position rows link to the Sector profile
    const sectors = document.getElementById('sectors')!;
    expect(within(sectors).getAllByRole('link').some((a) => /^\/sector\/[a-z-]+\/(upstream|midstream|downstream)\?c=USA$/.test(a.getAttribute('href') ?? ''))).toBe(true);
    // the top Strategic Index sector of an all-Grow country: the fallback rule (A9.5) explains its top three (behind "Why these sectors")
    fireEvent.click(screen.getByText(/^Why these sectors/));
    expect(await screen.findByText(/None of the United States' placed sectors is in the Upgrade corner/)).toBeTruthy();
    // the strongest measures card says it is context and names the extract (behind a disclosure)
    fireEvent.click(screen.getByText(/^Policy records by technology and stage/));
    expect(await screen.findByRole('heading', { name: 'Strongest active industrial-policy measures' })).toBeTruthy();
    expect(screen.getAllByText(/not part of the Policy index/).length).toBeGreaterThan(0);
    noAcronyms();
    // every chart card has a table twin; opening them all keeps the page free of bare acronyms
    const n = openTwins();
    expect(n).toBeGreaterThanOrEqual(3);
    expect(within(main()).getAllByRole('table').length).toBeGreaterThanOrEqual(n);
    noAcronyms();
    expect(source.calls.filter((c) => c.startsWith('facts-'))).toHaveLength(1);
  });

  it('EU core member without a memo (Germany): no memo button, being prepared, EU tags and the EU rating', async () => {
    renderAt('/countries/DEU');
    expect(await h1('Germany')).toBeTruthy();
    await screen.findByRole('heading', { level: 2, name: 'Policy memo' }, { timeout: 30_000 });
    expect(screen.queryByRole('link', { name: 'GENERATE MEMO' })).toBeNull();
    expect(screen.getByText('The memo for Germany is being prepared.')).toBeTruthy();
    expect(screen.getByText('EU MEMBER')).toBeTruthy();
    expect(screen.getAllByText(/The EU's rating/).length).toBeGreaterThan(0);
    noAcronyms();
  });

  it('coalition partner (Brazil): the core-only memo text with a link to the list', async () => {
    renderAt('/countries/BRA');
    expect(await h1('Brazil')).toBeTruthy();
    await screen.findByRole('heading', { level: 2, name: 'Policy memo' }, { timeout: 30_000 });
    expect(screen.getByText(/Memos are available for the 5 core coalition members for now\./)).toBeTruthy();
    expect(screen.getByRole('link', { name: /See the list/ }).getAttribute('href')).toBe('/countries#core');
    expect(screen.getByText('PARTNER')).toBeTruthy();
    noAcronyms();
  });

  it('no Strategic Index (Nigeria): the notices replace the priorities; tiles say so in words', async () => {
    renderAt('/countries/NGA');
    expect(await h1('Nigeria')).toBeTruthy();
    await screen.findByRole('heading', { level: 2, name: 'Policy & climate' }, { timeout: 30_000 });
    expect(screen.getAllByText('No Strategic Index').length).toBeGreaterThan(0);
    expect(screen.getAllByText(/so the paper computes no Strategic Index for it/).length).toBeGreaterThan(0);
    expect(screen.queryByRole('heading', { name: 'Which sectors the Strategic Index puts first' })).toBeNull();
    expect(screen.getByText('POLICY: RECORDS ONLY, NO CLIMATE RATING')).toBeTruthy();
    expect(screen.getByText(/Nigeria has no Climate Action Tracker rating/)).toBeTruthy();
    noAcronyms();
  });

  it('territory (Puerto Rico): scores shown but not ranked; the territory tag', async () => {
    renderAt('/countries/PRI');
    expect(await h1('Puerto Rico')).toBeTruthy();
    expect(await screen.findByText('Scores shown but not ranked', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText('TERRITORY')).toBeTruthy();
    noAcronyms();
    openTwins();
    noAcronyms();
  });

  it('an economy without index data: the profile shell with a notice and the Sector Insights link', async () => {
    useSource((m) => {
      const e: CountryEntry = {
        iso3: 'ATA', name: 'Antarctica', tier: 'none', region: null, eu_member: false, status: 'no_data', file: null, bytes_gz: null,
        n_placed: null, quadrants: null, memo: null, drivers: null, iso_n3: null,
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
    expect(screen.getAllByText('AI-generated from the data and checked against it; paper baseline as of 12 October 2026.').length).toBe(2);
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
    await screen.findByRole('heading', { level: 2, name: 'Policy memo' }, { timeout: 30_000 });
    expect(screen.queryByRole('link', { name: 'GENERATE MEMO' })).toBeNull();
    expect(screen.getByText('The memo for the United States is being revised. It will be available again once it has been rewritten.')).toBeTruthy();
    // GENERATE MEMO is there, disabled, and says why (head and memo panel)
    const buttons = screen.getAllByRole('button', { name: 'GENERATE MEMO' }) as HTMLButtonElement[];
    expect(buttons.length).toBe(2);
    for (const b of buttons) expect(b.disabled).toBe(true);
    expect(screen.getByText('The memo for the United States is being revised')).toBeTruthy();
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
    await screen.findByRole('heading', { level: 2, name: 'Policy memo' }, { timeout: 30_000 });
    expect(screen.getAllByRole('link', { name: 'GENERATE MEMO' }).length).toBe(2);
    expect(screen.getByTestId('memo-rule-note').textContent).toBe(note);
  });

  it('about: the sections, the glossary and the sources', async () => {
    renderAt('/countries/about');
    expect(await h1('Country profiles: data notes')).toBeTruthy();
    for (const h of ['What the Country profiles show', 'Reading the four corners', 'How priorities are compared with policy', 'How partners are compared with today\'s ties', 'The memos', 'Data and vintage', 'Glossary']) {
      expect(screen.getAllByRole('heading', { name: new RegExp(h.replace(/[()?]/g, '.')) }).length).toBeGreaterThan(0);
    }
    expect(screen.getAllByRole('link', { name: /glossary/i }).some((a) => a.getAttribute('href')?.startsWith('/glossary'))).toBe(true);
    noAcronyms();
  });

  it('URL state: a bad cell is reported; picking a sector and a partner writes cell and partner', async () => {
    const r = renderAt('/countries/JPN?cell=fusion-upstream&partner=ZZZ');
    expect(await h1('Japan')).toBeTruthy();
    expect(await screen.findByText('Ignored in the link', undefined, { timeout: 30_000 })).toBeTruthy();
    expect(screen.getByText(/cell=fusion-upstream \(not a sector\)/)).toBeTruthy();
    // a sector chip of the four-corners card selects the sector
    const corners = screen.getByRole('region', { name: 'The four corners' });
    // the app's corner names (2026-10-28), not the older words the synthetic sheets were exported with
    const names = [...within(corners).getByRole('table').querySelectorAll('tbody tr')].map((tr) => (tr.querySelector('td')?.textContent ?? '').trim().split(/\s/)[0]);
    expect(names).toEqual(['Grow', 'Upgrade', 'Exposed', 'Stable']);
    const chip = within(corners).getAllByRole('button')[0];
    fireEvent.click(chip);
    await waitFor(() => expect(r.state.location.search).toMatch(/^\?cell=[a-z-]+/));
    // a partner row of the slope chart (behind "Every partner") selects the partner
    fireEvent.click(screen.getByText(/^Every partner of Japan/));
    const slope = await screen.findByRole('region', { name: "Priority partners and today's partners" });
    const row = within(slope).getAllByRole('button').find((b) => /United States|Germany|South Korea|France/.test(b.textContent ?? ''));
    expect(row).toBeTruthy();
    fireEvent.click(row!);
    await waitFor(() => expect(r.state.location.search).toMatch(/partner=[A-Z]{3}/));
  });

  it('priorities table: "Show all" writes all=1 and lists every in-sample sector', async () => {
    const r = renderAt('/countries/KOR#why');
    expect(await h1('South Korea')).toBeTruthy();
    // an old #why link opens its disclosure
    await waitFor(() => expect(document.getElementById('fold-why')?.hasAttribute('open')).toBe(true), { timeout: 30_000 });
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
});
