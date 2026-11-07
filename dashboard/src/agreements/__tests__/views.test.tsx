// The Agreements page (/aib/agreements) on the SYNTHETIC agreements bundle: the three views (all economies,
// one economy, a pair), the scope toggle, the sort-key switch, the filters in the URL, existence vs delivery,
// established vs mentioned in text, provenance on every agreement, the absence caveat, no bare acronyms, and
// the route and nav entry in the Allied Industrial Base hub.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { CountryLoadError, CountryStore, setCountryStoreForTests, type CountrySource } from '../../country/data/store.ts';
import { fixtureSource as countrySource, haveFixture as haveCountry } from '../../country/__tests__/fixture.ts';
import { AgreementsStore, setAgreementsStoreForTests } from '../data.ts';
import type { AgreementsGlobal } from '../contract.ts';
import {
  ABSENCE_CAVEAT, depthRuleWords, executionGapText, filterPairs, zeroDepthWords, gapText, globalPairs, NO_FILTER, partnersAtDepth, rankPartners, type AgrFilter, type SortKey,
} from '../derive.ts';
import AgreementsView from '../views/AgreementsView.tsx';
import { emptySource, fixtureSource, readCountry, readGlobal, readManifest } from './fixture.ts';

const T = { timeout: 30_000 };
const manifest = readManifest();
const v = manifest.vocab;
const names = manifest.names;
const nameOf = (x: string) => names[x] ?? x;

const noCountryData: CountrySource = {
  manifest: () => Promise.reject(new CountryLoadError('NO_DATA', 'No country data.')),
  json: () => Promise.reject(new CountryLoadError('NETWORK', 'No country data.')),
  url: (f) => f,
};

const restore: (() => void)[] = [];
beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
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
  // react-router's Request with an AbortSignal from jsdom (the same shim as the other view tests)
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
  setAgreementsStoreForTests(new AgreementsStore(fixtureSource()));
  setCountryStoreForTests(new CountryStore(noCountryData));
});

afterEach(() => {
  cleanup();
  setAgreementsStoreForTests(null);
  setCountryStoreForTests(null);
});

function mount(url: string) {
  const router = createMemoryRouter([{ path: '/aib/agreements', element: <AgreementsView /> }], { initialEntries: [url] });
  render(<RouterProvider router={router} />);
  return router;
}

const search = (router: ReturnType<typeof mount>) => new URLSearchParams(router.state.location.search);
const main = () => document.body;
const filter = (over: Partial<AgrFilter>): AgrFilter => ({ ...NO_FILTER, ...over });
const plural = (n: number) => `${n} ${n === 1 ? 'pair' : 'pairs'}`;
/** The partner order of the ranking table, from its links. */
const rankingOrder = (): string[] =>
  within(screen.getByTestId('partner-ranking'))
    .getAllByRole('link')
    .map((a) => new URLSearchParams(a.getAttribute('href')?.split('?')[1] ?? '').get('partner') ?? '');

describe('without the bundle', () => {
  it('says the data is not in this build (CI) and nothing else breaks', async () => {
    setAgreementsStoreForTests(new AgreementsStore(emptySource()));
    mount('/aib/agreements');
    expect(await screen.findByText('No agreements data in this build', {}, T)).toBeTruthy();
    expect(screen.getByRole('heading', { level: 1, name: 'Agreements' })).toBeTruthy();
    expect(bareAcronyms(main())).toEqual([]);
  });
});

describe('all economies', () => {
  const g = readGlobal();
  const pairs = globalPairs(g);

  it('the map card, the pair table and the network; electro-industrial by default, all partnerships on the toggle', async () => {
    const router = mount('/aib/agreements');
    const view = await screen.findByTestId('agreements-global', {}, T);
    const ei = filterPairs(pairs, filter({}), v).rows.length;
    const all = filterPairs(pairs, filter({ scope: 'all' }), v).rows.length;
    expect(ei).toBeLessThan(all);
    expect(within(view).getByText(new RegExp(`^${ei} pairs with an electro-industrial agreement, deepest first`))).toBeTruthy();
    expect(within(view).getByRole('heading', { name: 'Where partnerships run deep' })).toBeTruthy();
    // no Country data: the map says so, and the network falls back to the economies with the most agreements
    expect(within(view).getByText(/The map needs the Countries data/)).toBeTruthy();
    expect(within(view).getByTestId('network-matrix')).toBeTruthy();
    expect((within(view).getByRole('combobox', { name: 'Group' }) as HTMLSelectElement).value).toBe('top');
    expect(within(view).getAllByTestId('absence-caveat').length).toBeGreaterThan(0);
    expect(bareAcronyms(main())).toEqual([]);

    // the scope toggle writes scope=all (the default is not written) and every count follows
    fireEvent.click(screen.getByRole('radio', { name: 'All partnerships' }));
    await waitFor(() => expect(search(router).get('scope')).toBe('all'));
    expect(await within(screen.getByTestId('agreements-global')).findByText(new RegExp(`^${all} pairs with an agreement, deepest first`))).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Electro-industrial partnerships' }));
    await waitFor(() => expect(search(router).has('scope')).toBe(false));
  });

  it('a filter the pair table cannot apply is named (a single instrument); the type filter is applied, each filter on its own', async () => {
    const router = mount('/aib/agreements?inst=export_credit&type=fta&scope=all');
    const view = await screen.findByTestId('agreements-global', {}, T);
    expect(within(view).getByText('Some filters apply only with an economy')).toBeTruthy();
    const n = filterPairs(pairs, filter({ scope: 'all', type: 'fta', inst: 'export_credit' }), v).rows.length;
    expect(within(view).getByText(new RegExp(`^${n} pairs? with an agreement under the filters`))).toBeTruthy();
    expect(within(view).getByTestId('global-filter-note').textContent).toMatch(/each filter is checked on its own against the pair.s agreements/);
    expect(screen.getByText(/2 filters on/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Clear filters' }));
    await waitFor(() => expect(search(router).toString()).toBe('scope=all'));
    expect(within(screen.getByTestId('agreements-global')).queryByTestId('global-filter-note')).toBeNull();
  });

  it('with the pair columns, the years (any agreement), binding status, implementation status, stage and instrument group apply to all economies', async () => {
    mount('/aib/agreements?scope=all&from=2010&to=2015&legal=binding&status=in_force&segment=Refining&igroup=trade');
    const view = await screen.findByTestId('agreements-global', {}, T);
    expect(within(view).queryByText('Some filters apply only with an economy')).toBeNull();
    expect(within(view).queryByTestId('years-by-newest')).toBeNull();
    const r = filterPairs(pairs, filter({ scope: 'all', from: 2010, to: 2015, legal: 'binding', status: 'in_force', segment: 'Refining', igroup: 'trade' }), v);
    expect(r.rows.length).toBeGreaterThan(0);
    expect(within(view).getByText(new RegExp(`^${plural(r.rows.length)} with an agreement under the filters`))).toBeTruthy();
    expect(within(view).getByTestId('global-filter-note').textContent).toMatch(/An exact filter on each agreement needs an economy chosen/);
  });

  it('an export without the years of every agreement: the years filter reads the newest, and the page says so whenever a year is set', async () => {
    const strip = (x: unknown) => {
      const g = structuredClone(x) as AgreementsGlobal;
      for (const k of ['years', 'years_ei', 'igroups', 'legal', 'status', 'stages'] as const) delete g.pairs[k];
      return g;
    };
    setAgreementsStoreForTests(new AgreementsStore(fixtureSource(undefined, (file, x) => (file.startsWith('agreements-global') ? strip(x) : x))));
    mount('/aib/agreements?scope=all&from=2020');
    const view = await screen.findByTestId('agreements-global', {}, T);
    expect(within(view).getByTestId('years-by-newest').textContent).toMatch(/reads each pair.s most recent agreement/);
    // a year alone needs no economy: no unsupported-filters notice
    expect(within(view).queryByText('Some filters apply only with an economy')).toBeNull();
    cleanup();
    mount('/aib/agreements?scope=all&legal=binding');
    const v2 = await screen.findByTestId('agreements-global', {}, T);
    expect(within(v2).getByText('Some filters apply only with an economy')).toBeTruthy();
    expect(within(v2).queryByTestId('years-by-newest')).toBeNull();
  });

  it('the network: a pair whose agreements do not count is a 0, not a blank; the map has a table twin', async () => {
    mount('/aib/agreements?scope=all');
    const view = await screen.findByTestId('agreements-global', {}, T);
    const mx = within(view).getByTestId('network-matrix');
    // Japan and New Zealand: one expired agreement
    const zero = [...mx.querySelectorAll('[data-cell="zero"]')].find((c) => /Japan and New Zealand/.test(c.getAttribute('title') ?? ''))!;
    expect(zero).toBeTruthy();
    expect(zero.textContent).toBe('0class 0: agreements, none counts toward depth');
    const none = mx.querySelector('[data-cell="none"]')!;
    expect(none.textContent).toBe('no agreement found');
    expect(within(view).getByText(/A 0 on grey: the pair has agreements, but none counts toward depth/)).toBeTruthy();
    // the map's numbers as a table: partners at class 3 or more (the map's colour), partners, agreements
    fireEvent.click(within(view).getByRole('button', { name: 'Table' }));
    const t = within(view).getByRole('table', { name: 'Partners at depth class 3 or more, partners and agreements of each economy' });
    const jpnRow = within(t).getByText('Japan').closest('tr')!;
    const g = readGlobal();
    const i = g.countries.iso3.indexOf('JPN');
    const cells = [...jpnRow.querySelectorAll('td')].map((c) => c.textContent);
    expect(cells).toEqual(['Japan', String(partnersAtDepth(pairs, 'all').get('JPN')), String(g.countries.n_partners[i]), String(g.countries.n_agreements[i]), String(g.countries.n_ei_agreements[i])]);
    expect(cells[1]).toBe(String(g.countries.n_partners_d3[i]));
  });

  it.skipIf(!haveCountry())('with the Country roster: the network defaults to the core coalition members', async () => {
    setCountryStoreForTests(new CountryStore(countrySource()));
    mount('/aib/agreements');
    const view = await screen.findByTestId('agreements-global', {}, T);
    await waitFor(() => expect((within(view).getByRole('combobox', { name: 'Group' }) as HTMLSelectElement).value).toBe('core'), T);
    expect(within(view).queryByText(/The map needs the Countries data/)).toBeNull();
  });

  it('choosing an economy opens its view; one without a file says no agreement was found, with the caveat', async () => {
    const router = mount('/aib/agreements');
    await screen.findByTestId('agreements-global', {}, T);
    fireEvent.change(screen.getByRole('combobox', { name: 'Economy' }), { target: { value: 'JPN' } });
    await waitFor(() => expect(search(router).get('country')).toBe('JPN'));
    expect(await screen.findByTestId('agreements-country', {}, T)).toBeTruthy();
    cleanup();
    mount('/aib/agreements?country=BRA');
    const none = await screen.findByTestId('agreements-no-file', {}, T);
    expect(within(none).getByTestId('absence-caveat').textContent).toBe(ABSENCE_CAVEAT);
  });
});

describe('one economy', () => {
  const file = readCountry('JPN');

  it('ranks partners by one key the reader picks; the URL keeps it', async () => {
    const router = mount('/aib/agreements?country=JPN');
    await screen.findByTestId('agreements-country', {}, T);
    const order = (sort: SortKey, f: AgrFilter = filter({ country: 'JPN' })) => rankPartners(file, f, sort, v, names).map((r) => r.partner);
    expect(rankingOrder()).toEqual(order('depth'));
    // the rule that set each depth class names agreements by title, never by id
    expect(screen.getByTestId('partner-ranking').textContent).toMatch(/“/);
    expect(screen.getByTestId('partner-ranking').textContent).not.toMatch(/agr-[0-9a-f]{10}/);
    expect(screen.getByRole('radiogroup', { name: 'Rank partners by' })).toBeTruthy();
    fireEvent.click(screen.getByRole('radio', { name: 'Recency' }));
    await waitFor(() => expect(search(router).get('sort')).toBe('recency'));
    expect(rankingOrder()).toEqual(order('recency'));
    expect(order('recency')).not.toEqual(order('depth'));
    fireEvent.click(screen.getByRole('radio', { name: 'Implementation' }));
    await waitFor(() => expect(rankingOrder()).toEqual(order('implementation')));
    fireEvent.click(screen.getByRole('radio', { name: 'Depth' }));
    await waitFor(() => expect(search(router).has('sort')).toBe(false));
    // all partnerships: the all-agreements depth and more partners
    fireEvent.click(screen.getByRole('radio', { name: 'All partnerships' }));
    await waitFor(() => expect(rankingOrder()).toEqual(order('depth', filter({ country: 'JPN', scope: 'all' }))));
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('a filter narrows the matching agreements (delivery evidence: Australia only); clearing it restores the list', async () => {
    const router = mount('/aib/agreements?country=JPN');
    await screen.findByTestId('agreements-country', {}, T);
    fireEvent.change(screen.getByRole('combobox', { name: 'Implementation status' }), { target: { value: 'delivery' } });
    await waitFor(() => expect(search(router).get('status')).toBe('delivery'));
    await waitFor(() => expect(rankingOrder()).toEqual(['AUS']));
    fireEvent.click(screen.getByRole('button', { name: 'Clear filters' }));
    await waitFor(() => expect(rankingOrder().length).toBeGreaterThan(1));
  });

  it('coverage marks established and mentioned apart (glyph and words); gaps by group with their numbers; execution gaps', async () => {
    mount('/aib/agreements?country=JPN');
    await screen.findByTestId('agreements-country', {}, T);
    const sectors = screen.getByTestId('sector-coverage');
    expect(sectors.querySelector('[data-grade="specific"]')?.textContent).toContain('established');
    expect(sectors.querySelector('[data-grade="text"]')?.textContent).toContain('mentioned in text');
    expect(screen.getAllByTestId('grade-legend').length).toBeGreaterThan(0);
    const gaps = screen.getByTestId('strategic-gaps');
    const ctx = { focal: 'JPN', name: nameOf, depthEi: (p: string) => file.partners.find((x) => x.partner === p)?.depth_ei ?? null, compacts: v.compacts };
    const kor = file.gaps.find((x) => x.partner === 'KOR' && x.quadrant === 'opportunity')!;
    const text = gapText(kor, ctx);
    expect(text).toMatch(/^High complementarity, weak architecture: Korea could supply Batteries to Japan \(\d+ recommended cells?, capability \d+(st|nd|rd|th) percentile/);
    // compacts by name, and the absence in these sources
    expect(text).toMatch(/shared strategic compacts: Battery Materials & Cells.* but no agreement in these sources covers Batteries\.$/);
    const opp = within(gaps).getByTestId('gaps-opportunity');
    expect(within(opp).getByText(text)).toBeTruthy();
    expect(within(opp).getByTestId('absence-caveat').textContent).toBe(ABSENCE_CAVEAT);
    expect(gaps.textContent).not.toMatch(/SC-BATTERIES/);
    expect(within(gaps).getByTestId('gaps-established')).toBeTruthy();
    // the fixture's legacy row is Oil: outside the electro-industrial scope (the default), shown under all partnerships
    expect(within(gaps).queryByTestId('gaps-legacy')).toBeNull();
    // the low-low row is not a gap group
    expect(within(gaps).queryByTestId('gaps-none')).toBeNull();
    const exec = within(gaps).getByTestId('execution-gaps');
    for (const e of file.execution_gaps) expect(within(exec).getByText(executionGapText(e, ctx))).toBeTruthy();
    expect(within(exec).getByTestId('absence-caveat')).toBeTruthy();
    // the coverage tables read whole pair profiles: they say the narrowing filters do not apply
    expect(screen.getAllByText(/filters other than the scope are not applied/).length).toBe(2);
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('a partner at class 0 with agreements says they do not count, never "No agreement found" (Japan and New Zealand, all partnerships)', async () => {
    mount('/aib/agreements?country=JPN&scope=all');
    await screen.findByTestId('agreements-country', {}, T);
    const ranking = screen.getByTestId('partner-ranking');
    const row = within(ranking).getByRole('link', { name: 'New Zealand' }).closest('tr')!;
    expect(row.textContent).toContain(zeroDepthWords(1, 'all'));
    expect(ranking.textContent).not.toMatch(/No agreement found/);
  });

  it('all partnerships brings back the gaps of oil, gas and coal', async () => {
    mount('/aib/agreements?country=JPN&scope=all');
    await screen.findByTestId('agreements-country', {}, T);
    expect(within(screen.getByTestId('strategic-gaps')).getByTestId('gaps-legacy')).toBeTruthy();
  });
});

describe('a pair', () => {
  it('Japan and Australia: depth with its rule in both scopes, architecture, existence and delivery apart, provenance on every row', async () => {
    mount('/aib/agreements?country=JPN&partner=AUS');
    const view = await screen.findByTestId('agreements-pair', {}, T);
    const jpn = readCountry('JPN');
    const aus = jpn.partners.find((p) => p.partner === 'AUS')!;
    const all = within(view).getByTestId('depth-all');
    expect(all.textContent).toContain(`Class ${aus.depth_all}`);
    // the rule in words: agreement titles, not ids
    const rule = depthRuleWords(aus.depth_all_rule, v, new Map(jpn.agreements.map((a) => [a.id, a])))!;
    expect(rule).toMatch(/“/);
    expect(all.textContent).toContain(rule);
    expect(all.textContent).not.toMatch(/agr-[0-9a-f]{10}/);
    expect(within(view).getByTestId('depth-ei').textContent).toContain(`Class ${aus.depth_ei}`);
    expect(within(view).getByTestId('architecture').textContent).toMatch(/Trade agreement\s*Yes/);
    expect(within(view).getByTestId('timeline')).toBeTruthy();

    // existence and delivery in separate columns; delivery evidence with its links, none muted
    const delivered = within(view).getAllByTestId('delivery').filter((d) => d.dataset.delivery === 'yes');
    const none = within(view).getAllByTestId('delivery').filter((d) => d.dataset.delivery === 'no');
    expect(delivered.some((d) => /^Delivery: implementation underway \(2 items\)/.test(d.textContent ?? ''))).toBe(true);
    // two evidence items, one with a link
    const two = delivered.find((d) => /underway \(2 items\)/.test(d.textContent ?? ''))!;
    expect(two.querySelectorAll('li').length).toBe(2);
    expect(two.querySelectorAll('a').length).toBe(1);
    expect(none.length).toBeGreaterThan(0);
    for (const d of none) expect(d.textContent).toBe('Delivery: no evidence recorded');
    expect(within(view).getAllByTestId('exists').map((e) => e.textContent)).toContain('Exists: signed 22 October 2022 · non-binding');

    // established vs mentioned in text: a body-text code is never shown as established
    const table = within(view).getByRole('table', { name: 'Agreements of Japan and Australia' });
    const mentioned = [...table.querySelectorAll('[data-grade="text"]')].map((e) => e.textContent ?? '');
    expect(mentioned.some((t) => /Hydrogen/.test(t) && /mentioned in text/.test(t))).toBe(true);
    for (const t of mentioned) expect(t).not.toMatch(/coded by the source|recorded by hand/);

    // provenance: every row names its sources, their number and the confidence
    const prov = within(view).getAllByTestId('provenance');
    expect(prov.length).toBe(within(table).getAllByTestId('exists').length);
    expect(prov.some((p) => /2 sources · confidence high/.test(p.textContent ?? ''))).toBe(true);
    expect(prov.some((p) => /1 source · confidence low/.test(p.textContent ?? ''))).toBe(true);
    // the primary source on every provenance line: the official text, an official page (no primary_url, an official source record), or not identified
    const primary = within(table).getAllByTestId('primary-source');
    expect(primary.length).toBe(prov.length);
    const kinds = new Map(primary.map((x) => [x.dataset.primary, x]));
    expect(kinds.get('text')?.textContent).toBe('Primary source: official text');
    expect(kinds.get('text')?.querySelector('a')).toBeTruthy();
    const page = kinds.get('page')!;
    expect(page.textContent).toBe('Primary source: official page');
    expect(page.querySelector('a')?.getAttribute('href')).toBe('https://www.energy.gov.au/government-priorities/energy-and-climate-change-ministerial-council');
    expect(kinds.get('none')?.textContent).toBe('Primary source: not identified');
    expect(table.textContent).not.toMatch(/No link to the original document/);
    // an instrument named only in the title is mentioned, not established
    const lts = [...table.querySelectorAll('[data-grade]')].find((e) => /Long-term supply arrangement/.test(e.textContent ?? ''))!;
    expect(lts.getAttribute('data-grade')).toBe('text');
    // the pair's gaps carry the absence caveat
    expect(within(within(view).getByTestId('pair-gaps')).getByTestId('absence-caveat')).toBeTruthy();
    expect(bareAcronyms(main())).toEqual([]);
  });

  it('an agreement reached through the EU says so (Germany and the United States)', async () => {
    mount('/aib/agreements?country=DEU&partner=USA');
    const view = await screen.findByTestId('agreements-pair', {}, T);
    expect(within(view).getAllByText('through the EU').length).toBeGreaterThan(0);
  });

  it('depth class 0 and an empty result show the absence caveat (Japan and New Zealand)', async () => {
    mount('/aib/agreements?country=JPN&partner=NZL');
    const view = await screen.findByTestId('agreements-pair', {}, T);
    expect(within(view).getByTestId('depth-all').textContent).toContain('Class 0');
    // why class 0: one expired agreement in all; none electro-industrial
    expect(within(view).getByTestId('depth-all').textContent).toContain(zeroDepthWords(1, 'all'));
    expect(within(view).getByTestId('depth-ei').textContent).toContain('No electro-industrial agreement of the pair is found in these sources.');
    // the absence caveat only where nothing was found: the expired agreement was found
    expect(within(within(view).getByTestId('depth-all')).queryByTestId('absence-caveat')).toBeNull();
    expect(within(within(view).getByTestId('depth-ei')).getByTestId('absence-caveat').textContent).toBe(ABSENCE_CAVEAT);
    // electro-industrial scope: no agreement matches
    expect(within(view).getByText('No agreement matches these filters.')).toBeTruthy();
    expect(within(view).getAllByTestId('absence-caveat').length).toBeGreaterThan(2);
  });

  it('a partner with no agreement at all', async () => {
    mount('/aib/agreements?country=JPN&partner=BRA');
    const view = await screen.findByTestId('agreements-pair', {}, T);
    expect(within(view).getByText('No agreement of Japan and BRA was found.')).toBeTruthy();
    expect(within(view).getByTestId('absence-caveat')).toBeTruthy();
  });
});
