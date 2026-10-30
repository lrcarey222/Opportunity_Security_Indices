// The strategic roles in the app (2026-10-20; task spec s.17-18), on the synthetic AIB fixture:
//   /aib/roles  "Strategic roles" above the operational part: the economy selector (allied economies, then
//               the major non-allied suppliers; the choice in the URL, `economy`; a #role-ISO3 link opens on
//               its economy), the summary card (primary strategic roles, what it contributes, what it
//               needs with the allied economies that could supply it, its most important strategic
//               compacts linking to their pages with the toggle carried, its highest-value partners with the
//               pair's relationship logic, where new capacity matters) and "How the model derives this"
//               (both gates of every role, the rule, then the economy's operational roles under the
//               scenario); who holds each strategic role, each holder a button that shows its summary and
//               moves focus to it; the operational part follows under "Operational network roles"; a
//               bundle without the layer shows the operational part only.
//   Country profile  "Role in the Allied Industrial Base" (since 2026-10-29 under Allied Strategy, in "Explore all
//               partnership analysis", #allied-aib-role; the old #aib-role link opens it): strategic AIB roles, the
//               strategic compacts with its role in each (each linking to its page), its key bilateral
//               relationships (what each side contributes); an economy the layer does not assess says so; the
//               operational card follows.
//   AIB_DATA_DIR (the real export)  the summary of the five validation economies (Japan, Korea, the United
//               States, Australia, India) and China renders its export's words, without a bare acronym.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { bareAcronyms } from '../../components/testing/acronyms.ts';
import { resetLastLocations } from '../../shell/lastLocation.ts';
import { setEngineClient } from '../../state/EngineProvider.tsx';
import { EngineClient } from '../../worker/client.ts';
import { CountryStore, setCountryStoreForTests } from '../../country/data/store.ts';
import { fixtureSource as countrySource, haveFixture as haveCountry } from '../../country/__tests__/fixture.ts';
import type { AibDoc, AibScenarioDoc } from '../contract.ts';
import { AibStore, setAibStoreForTests } from '../data/store.ts';
import { resetLastCompact } from '../nav.ts';
import { FIXTURE_DIR, fixtureSource, haveFixture, readDoc, stripStrategic, SYNTHETIC_DIR, type FixtureSourceOpts } from './fixture.ts';
import { strategicSummaryOf } from '../views/strategicRoles/derive.ts';

const restore: (() => void)[] = [];

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
function useSource(edit?: (d: AibDoc) => void, opts?: FixtureSourceOpts): void {
  setAibStoreForTests(new AibStore(fixtureSource(SYNTHETIC_DIR, edit, opts)));
}

beforeEach(() => useSource());

afterEach(() => {
  cleanup();
  setAibStoreForTests(null);
  setCountryStoreForTests(null);
  setEngineClient(null);
  resetLastLocations();
  resetLastCompact();
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}

const T = { timeout: 30_000 };
const h1 = async (name: RegExp | string) => screen.findByRole('heading', { level: 1, name }, T);
const main = () => document.querySelector('main') as HTMLElement;
const summary = () => screen.getByTestId('strategic-summary');
const slot = (id: string) => within(summary()).getByTestId(`slot-${id}`);
const hrefs = (el: HTMLElement) => within(el).getAllByRole('link').map((a) => [a.textContent, a.getAttribute('href')]);
const search = (r: ReturnType<typeof renderAt>) => new URLSearchParams(r.state.location.search);

describe.skipIf(!haveFixture(SYNTHETIC_DIR))('/aib/roles: the strategic summary above the operational table', () => {
  it('opens on the first allied economy by name with a primary role: its roles, contributions, needs, compacts, partners and capacity; the operational part follows', async () => {
    renderAt('/aib/roles');
    expect(await h1('Country roles')).toBeTruthy();
    const sec = await screen.findByTestId('strategic-roles', {}, T);
    expect(within(sec).getByRole('heading', { level: 2, name: 'Strategic roles' })).toBeTruthy();
    expect((within(sec).getByRole('combobox', { name: 'Strategic summary of' }) as HTMLSelectElement).value).toBe('AAA');
    expect(within(summary()).getByRole('heading', { level: 3 }).textContent).toBe("Avalon's role in the Allied Industrial Base");
    expect(within(summary()).getByTestId('strategic-headline').textContent).toBe('Combines demand & market and capital & investment roles; secondary coordination / bridge role.');
    // primary strategic roles as chips; the secondary one in words
    expect(within(within(slot('roles')).getByRole('list', { name: 'Primary strategic roles' })).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      'Demand & Market Anchor',
      'Capital & Investment Anchor',
    ]);
    expect(slot('roles').textContent).toContain('Secondary: Coordination / Bridge Hub.');
    // what it contributes: the node, its share of world supply, the compact it sits in (a link, Managed Interdependence: no scn)
    expect(slot('contributes').textContent).toMatch(/^What it contributesMagnets, magnet parts & assemblies · 18% of world supplyin Permanent Magnets & Drive Systems/);
    expect(hrefs(slot('contributes'))).toEqual([['Permanent Magnets & Drive Systems', '/compacts/s/SC-MAGNETS']]);
    // what it needs: the allied economies with a supply position in each needed node
    const needs = within(slot('needs')).getAllByRole('listitem');
    expect(needs.map((li) => li.textContent)).toEqual([
      'Rare-earth oxides & compounds · 41% of allied demandAllied suppliers with a position: Carpania (11% of world supply), Dorvalia (6% of world supply) · in Permanent Magnets & Drive Systems',
      'Synthetic graphite (anode-grade, spherical) · 38% of allied demandAllied suppliers with a position: Carpania (14% of world supply), Borduria (8% of world supply) · in Battery Materials & Cells',
    ]);
    expect(within(needs[0]).getByRole('link', { name: 'Carpania' }).getAttribute('href')).toBe('/countries/CCC#aib-role');
    // the three most relevant compacts, each linking to its page
    expect(hrefs(slot('compacts')).map(([, h]) => h)).toEqual(['/compacts/s/SC-WIND', '/compacts/s/SC-MAGNETS', '/compacts/s/SC-BATTERIES']);
    expect(slot('compacts').textContent).toContain('Demand anchor in magnets (36% of world imports)');
    expect(slot('compacts').textContent).toContain('A role in 2 more');
    // the partners: their profiles, the pair's logic and priority compacts
    const partners = within(slot('partners')).getByRole('list', { name: 'Highest-value partners' });
    expect(within(partners).getAllByRole('listitem').map((li) => li.getAttribute('data-partner'))).toEqual(['CCC', 'BBB', 'EEE']);
    expect(within(partners).getAllByRole('listitem')[0].textContent).toBe(
      'CarpaniaA trade, technology and resilience / second sourcing relationship · priority compacts: Permanent Magnets & Drive Systems, Battery Materials & CellsThe relationship in Partnerships →',
    );
    // each partner opens the pair on the Partnerships page (its relationship thesis)
    expect(within(partners).getByRole('link', { name: 'The relationship between Avalon and Carpania in Partnerships' }).getAttribute('href')).toBe('/aib/partnerships?country=AAA&partner=CCC');
    expect(slot('partners').textContent).toContain('in the Managed Interdependence network');
    expect(slot('capacity').textContent).toContain('It is not a top host with capability at scale for any node.');
    // "How the model derives this": every role with both gates (held first, then the export's order), the rule, then the operational roles once the scenario file is in
    const ev = within(summary()).getByTestId('strategic-role-evidence');
    expect([...ev.querySelectorAll('[data-role]')].map((li) => `${li.getAttribute('data-role')}:${li.getAttribute('data-status')}`)).toEqual([
      'DMA:primary', 'CIA:primary', 'CBH:secondary', 'STM:none', 'RPA:none', 'SMI:none', 'DSP:none',
    ]);
    expect(ev.textContent).toMatch(/at or above the 90th percentile of the economies assessed for it/);
    const op = within(summary()).getByTestId('operational-evidence');
    expect(op.textContent).toMatch(/^Operational network roles under Managed Interdependence/);
    expect(await within(op).findByRole('list', { name: 'Operational network roles of Avalon' }, T)).toBeTruthy();
    // the page's order: the strategic roles, then the operational part
    const h2s = within(main()).getAllByRole('heading', { level: 2 }).map((h) => h.textContent);
    expect(h2s.indexOf('Strategic roles')).toBeLessThan(h2s.indexOf('Operational network roles'));
    expect(h2s.indexOf('Operational network roles')).toBeLessThan(h2s.indexOf('How roles are read'));
    expect(await screen.findByRole('region', { name: 'Roles by economy' }, T)).toBeTruthy();
    expect(bareAcronyms(document.body)).toEqual([]);
  }, 60_000);

  it('the selector and the role holders choose the economy (written to the URL, the toggle kept); a holder moves focus to its summary', async () => {
    const r = renderAt('/aib/roles?scn=decoupling');
    expect(await h1('Country roles')).toBeTruthy();
    await screen.findByTestId('strategic-summary', {}, T);
    fireEvent.change(screen.getByRole('combobox', { name: 'Strategic summary of' }), { target: { value: 'CCC' } });
    await waitFor(() => expect(search(r).get('economy')).toBe('CCC'));
    expect(search(r).get('scn')).toBe('decoupling');
    expect(within(summary()).getByRole('heading', { level: 3 }).textContent).toBe("Carpania's role in the Allied Industrial Base");
    // where new capacity matters: the nodes, the total, its host compacts, every link carrying Decoupling
    expect(slot('capacity').textContent).toContain('Together US$1.6bn of unmet allied demand that it could host.');
    expect(hrefs(slot('capacity')).map(([, h]) => h)).toEqual([
      '/compacts/s/SC-GRID?scn=decoupling', '/compacts/s/SC-MAGNETS?scn=decoupling', '/compacts/s/SC-MAGNETS?scn=decoupling', '/compacts/s/SC-GRID?scn=decoupling',
    ]);
    expect(slot('roles').textContent).toContain('Also meets the bar, beyond the cap: Development & Scale-Up Platform.');
    expect(within(summary()).getByTestId('operational-evidence').textContent).toMatch(/^Operational network roles under Decoupling/);
    // who holds each role: one card per role; Dorvalia holds Development & Scale-Up Platform as primary
    const holders = screen.getByTestId('role-holders');
    expect(within(holders).getAllByRole('heading', { level: 4 }).map((h) => h.textContent)).toEqual([
      'Resource & Processing Anchor', 'Specialized Technology & Manufacturing Anchor', 'Scale Manufacturing & Integration Platform', 'Demand & Market Anchor',
      'Capital & Investment Anchor', 'Development & Scale-Up Platform', 'Coordination / Bridge Hub',
    ]);
    const rpa = holders.querySelector('[data-role="RPA"]') as HTMLElement;
    expect(rpa.textContent).toContain('Zembla (non-allied)');
    expect(within(rpa).getByRole('button', { name: 'Carpania' }).getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(within(holders.querySelector('[data-role="DSP"]') as HTMLElement).getByRole('button', { name: 'Dorvalia' }));
    await waitFor(() => expect(search(r).get('economy')).toBe('DDD'));
    const h3 = within(summary()).getByRole('heading', { level: 3 });
    expect(h3.textContent).toBe("Dorvalia's role in the Allied Industrial Base");
    await waitFor(() => expect(document.activeElement).toBe(h3));
    expect(search(r).get('scn')).toBe('decoupling');
    expect(bareAcronyms(document.body)).toEqual([]);
  }, 60_000);

  it('a #role-ISO3 link opens on its economy (the matrix row still marked); a non-allied supplier is read on the supply roles only', async () => {
    renderAt('/aib/roles?scn=decoupling#role-EEE');
    expect(await h1('Country roles')).toBeTruthy();
    await waitFor(() => expect(summary().getAttribute('data-iso3')).toBe('EEE'), T);
    await waitFor(() => expect((document.getElementById('role-EEE') as HTMLElement).className).toContain('bg-signal/10'), T);
    cleanup();
    renderAt('/aib/roles?economy=ZZZ');
    expect(await h1('Country roles')).toBeTruthy();
    await waitFor(() => expect(summary().getAttribute('data-iso3')).toBe('ZZZ'), T);
    expect(summary().textContent).toContain('A major non-allied supplier: assessed on the supply roles only');
    expect(slot('needs').textContent).toContain('Not read for a non-allied supplier.');
    const ev = within(summary()).getByTestId('strategic-role-evidence');
    expect([...ev.querySelectorAll('[data-role]')].map((li) => li.getAttribute('data-role'))).toEqual(['RPA', 'SMI', 'STM']);
    expect(ev.textContent).toContain('A non-allied supplier is assessed on the three supply roles only');
    // an economy the layer does not assess: the default instead
    cleanup();
    renderAt('/aib/roles?economy=QQQ');
    expect(await h1('Country roles')).toBeTruthy();
    await waitFor(() => expect(summary().getAttribute('data-iso3')).toBe('AAA'), T);
  }, 60_000);

  it('a bundle without the strategic layer: the operational part only, as before', async () => {
    useSource((d) => void stripStrategic(d));
    renderAt('/aib/roles?scn=decoupling');
    expect(await h1('Country roles')).toBeTruthy();
    expect(await screen.findByRole('heading', { level: 2, name: 'How roles are read' }, T)).toBeTruthy();
    expect(screen.queryByTestId('strategic-roles')).toBeNull();
    expect(screen.queryByRole('heading', { level: 2, name: 'Operational network roles' })).toBeNull();
    expect(screen.queryByRole('combobox', { name: 'Strategic summary of' })).toBeNull();
  });
});

/** The AIB economy AAA renamed USA (a fact-sheet economy), in place. */
function renamed<D extends AibDoc | AibScenarioDoc>(d: D): void {
  Object.assign(d, JSON.parse(JSON.stringify(d).replaceAll('"AAA"', '"USA"')) as D);
}

describe.skipIf(!haveFixture(SYNTHETIC_DIR) || !haveCountry())('the Country profile: Role in the Allied Industrial Base', () => {
  beforeEach(() => {
    setCountryStoreForTests(new CountryStore(countrySource()));
    setAibStoreForTests(new AibStore(fixtureSource(SYNTHETIC_DIR, renamed, { editScenario: (d) => renamed(d) })));
    setEngineClient(new EngineClient({ onmessage: null, postMessage: () => undefined }, '/never/'));
  });

  it('strategic AIB roles, the strategic compacts with its role in each (linking to their pages), its key bilateral relationships; the operational card follows', async () => {
    // the old #aib-role link opens the Allied Strategy section's "Explore all partnership analysis" on the role (design 5b)
    renderAt('/countries/USA?scn=decoupling#aib-role');
    await screen.findByRole('heading', { level: 3, name: 'Role in the Allied Industrial Base' }, T);
    const role = document.getElementById('allied-aib-role')!;
    expect(document.getElementById('allied')?.contains(role)).toBe(true);
    const strat = await within(role).findByTestId('aib-strategic', {}, T);
    // 1. strategic AIB roles
    const roles = within(strat).getByTestId('aib-strategic-roles');
    expect(within(roles).getByRole('heading', { level: 3, name: 'Strategic AIB roles' })).toBeTruthy();
    expect(within(roles).getByTestId('aib-strategic-headline').textContent).toBe('Combines demand & market and capital & investment roles; secondary coordination / bridge role.');
    expect(within(within(roles).getByRole('list', { name: 'Primary strategic roles' })).getAllByRole('listitem').map((li) => li.textContent)).toEqual([
      'Demand & Market Anchor',
      'Capital & Investment Anchor',
    ]);
    expect(within(roles).getByTestId('aib-slot-needs').textContent).toContain('Allied suppliers with a position: Carpania (11% of world supply)');
    expect(within(roles).getByTestId('strategic-role-evidence')).toBeTruthy();
    // 2-3. compact participation and its role in each: every compact, most relevant first, each linking to its page with the toggle
    const compacts = within(strat).getByTestId('aib-strategic-compacts');
    expect(within(compacts).getByText(/has a role in 5 strategic compacts of 5, as a core country in 5\./)).toBeTruthy();
    const items = within(within(compacts).getByRole('list', { name: 'Roles in the strategic compacts' })).getAllByRole('listitem');
    expect(items.map((li) => li.getAttribute('data-compact'))).toEqual(['SC-WIND', 'SC-MAGNETS', 'SC-BATTERIES', 'SC-GRID', 'SC-SOLAR']);
    expect(within(items[1]).getByRole('link', { name: 'Permanent Magnets & Drive Systems' }).getAttribute('href')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling');
    expect(items[1].textContent).toContain('Demand anchor in magnets (36% of world imports)');
    expect(items[1].textContent).toContain('Key nodes: Magnets, magnet parts & assemblies; Rare-earth oxides & compounds');
    expect(within(compacts).getByRole('link', { name: /Every Strategic Sectoral Compact/ }).getAttribute('href')).toBe('/compacts?scn=decoupling');
    // 4. key bilateral relationships: the partners, the logic, what each side contributes
    const bi = within(strat).getByTestId('aib-bilateral');
    const partners = within(bi).getByRole('list', { name: 'Highest-value partners' });
    expect([...partners.querySelectorAll(':scope > li')].map((li) => li.getAttribute('data-partner'))).toEqual(['CCC', 'BBB', 'EEE']);
    // the economy by the profile's own name, its partners by the AIB's
    expect(within(partners).getByRole('list', { name: 'What each side contributes: the United States and Carpania' }).textContent).toBe(
      'Permanent Magnets & Drive Systems: the United States as demand anchor, Carpania as processing anchorBattery Materials & Cells: the United States as demand anchor, Carpania as processing anchor',
    );
    expect(within(partners).getByRole('link', { name: 'The relationship between the United States and Carpania in Partnerships' }).getAttribute('href')).toBe(
      '/aib/partnerships?country=USA&partner=CCC&scn=decoupling',
    );
    // the operational card follows, under the scenario
    expect(within(role).getByRole('heading', { level: 3, name: 'Operational roles and workstreams' })).toBeTruthy();
    expect(await within(role).findByText('Demand anchor / Buyer', {}, T)).toBeTruthy();
    const order = [...role.querySelectorAll('[data-testid="aib-strategic"], [data-testid="aib-role-scenario"], [data-testid="aib-roles"]')].map((e) => e.getAttribute('data-testid'));
    expect(order).toEqual(['aib-strategic', 'aib-role-scenario', 'aib-roles']);
    expect(bareAcronyms(role)).toEqual([]);
  }, 90_000);

  it('Managed Interdependence (no scn): the compact links carry no scenario; an economy the layer does not assess says so', async () => {
    renderAt('/countries/USA#aib-role');
    await screen.findByRole('heading', { level: 3, name: 'Role in the Allied Industrial Base' }, T);
    const role = document.getElementById('allied-aib-role')!;
    const compacts = await within(role).findByTestId('aib-strategic-compacts', {}, T);
    expect(within(compacts).getAllByRole('link').map((a) => a.getAttribute('href'))).toContain('/compacts/s/SC-WIND');
    cleanup();
    renderAt('/countries/DEU#aib-role');
    await screen.findByRole('heading', { level: 3, name: 'Role in the Allied Industrial Base' }, T);
    expect((await within(document.getElementById('allied-aib-role')!).findByTestId('aib-strategic-none', {}, T)).textContent).toMatch(/^Germany is not among the economies the strategic layer assesses/);
  }, 90_000);
});

// the directory of the view tests (AIB_DATA_DIR=public/data/aib: the real export)
describe.skipIf(!haveFixture(FIXTURE_DIR) || !readDoc(FIXTURE_DIR).strategic)("the strategic summaries of the view tests' bundle (AIB_DATA_DIR)", () => {
  it('the validation economies (or every economy of the synthetic fixture): heading, headline, primary roles and compact links from the export, no bare acronym', async () => {
    setAibStoreForTests(new AibStore(fixtureSource(FIXTURE_DIR)));
    const fx = readDoc(FIXTURE_DIR);
    const st = fx.strategic!;
    const want = ['JPN', 'KOR', 'USA', 'AUS', 'IND', 'CHN'].filter((x) => st.countries.some((c) => c.iso3 === x));
    const isos = want.length ? want : st.countries.map((c) => c.iso3);
    for (const iso of isos) {
      const sum = strategicSummaryOf(st, fx.nodes, fx.countries, iso)!;
      renderAt(`/aib/roles?economy=${iso}&scn=decoupling`);
      expect(await h1('Country roles')).toBeTruthy();
      await waitFor(() => expect(summary().getAttribute('data-iso3')).toBe(iso), T);
      expect(within(summary()).getByTestId('strategic-headline').textContent).toBe(`${sum.headline}${sum.secondaryLine ? `; ${sum.secondaryLine}` : ''}.`);
      const chips = within(slot('roles')).queryByRole('list', { name: 'Primary strategic roles' });
      expect(chips ? within(chips).getAllByRole('listitem').length : 0).toBe(sum.cs.primary.length);
      for (const [, h] of hrefs(slot('compacts'))) expect(h).toMatch(/^\/compacts\/s\/SC-[A-Z0-9-]+\?scn=decoupling$/);
      expect(within(slot('compacts')).getAllByRole('listitem').map((li) => li.getAttribute('data-compact'))).toEqual(sum.compacts.slice(0, 3).map((r) => r.compact));
      expect(bareAcronyms(document.body)).toEqual([]);
      cleanup();
    }
  }, 120_000);
});
