// Climate sources (sector-climate-src; user request 2026-10-17): a small chunk on the TS fixture passes the
// contract (its cross-table rules catch a wrong cell match and a GCP row with a match), derives the
// Country profile's fossil CO2 headline (the GCP's transfer sign), the transition indicators against the
// median economy, the WEO region and its sentence, the 2021 roadmap milestone, the investment headline and
// the fuel context, and renders the Country profile's three cards and the Sector profile's investment and
// fuel cards, labelled as observed estimates or IEA scenario projections.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { createMemoryRouter, MemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { buildSectorFixture, fixtureGeo, fixtureItem, fixtureSource } from '../__fixtures__/build.ts';
import { climateSrcChunk, withClimateSrc } from '../__fixtures__/climateSrc.ts';
import { checkChunk, refSizes, validateChunk, type SectorClimateSrcChunk } from '../contract.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { modelOf } from '../data/model.ts';
import {
  climateSrcData, eiRows, fuelRows, gcpCountry, gcpHeadline, nze21For, weiContext, weiHeadline, weoRegionLine, weoRegionOf, weoRegionRows,
} from '../derive/sources.ts';
import { CountryTransitionCards } from './climate/CountryTransition.tsx';

describe('sector-climate-src: contract and derive', () => {
  const fx = buildSectorFixture();
  const meta = fx.chunks['sector-meta'] as Parameters<typeof modelOf>[0];
  const ref = refSizes(meta);
  const d = () => climateSrcData(validateChunk('sector-climate-src', climateSrcChunk(), ref) as SectorClimateSrcChunk);

  it('passes the contract; the cross-table rules catch a wrong cell match and a GCP row with a match', () => {
    expect(checkChunk('sector-climate-src', climateSrcChunk(), ref).errors).toEqual([]);
    const bad = climateSrcChunk();
    (bad['csrc.cells'] as { cols: { match: (string | null)[] } }).cols.match[0] = 'proxy';
    expect(checkChunk('sector-climate-src', bad, ref).errors.join('; ')).toMatch(/best match of the cell's WEI rows/);
    const bad2 = climateSrcChunk();
    const m = bad2['csrc.map'] as { cols: { src: (string | null)[]; match: (string | null)[] } };
    m.cols.match[m.cols.src.indexOf('gcp')] = 'direct';
    expect(checkChunk('sector-climate-src', bad2, ref).errors.join('; ')).toMatch(/gcp rows a fuel and role context without a match/);
  });

  it('derives the fossil CO2 headline with the GCP transfer sign (negative: a net importer of embodied CO2)', () => {
    const c = gcpCountry(d(), fixtureGeo('JPN'), fixtureGeo('WLD'))!;
    expect(c.last).toEqual({ year: 2024, value: 962 });
    expect(gcpHeadline(c, 'Japan')).toBe(
      'Japan’s fossil CO2 emissions were 962 Mt in 2024 (−16% since 1990, −21% since 2015), 2.5% of the world’s. Counting the CO2 in what it buys from abroad instead (consumption-based), its emissions were 1,140 Mt in 2023, more than it emits at home: the CO2 in its imports exceeds that in its exports by 155 Mt.',
    );
    expect(gcpCountry(d(), fixtureGeo('BRA'), null)).toBeNull();
  });

  it('puts each indicator against the median economy; the low-carbon share sums the low-carbon sources', () => {
    const rows = eiRows(d(), fixtureGeo('JPN'));
    const by = new Map(rows.map((r) => [r.ind.id, r]));
    expect(by.get('ghg')).toMatchObject({ value: 1031, median: 583, n: 3, vsMedian: null, prev: 1228, prevYear: 2017 }); // a national total: not ranked
    expect(by.get('ghg')!.change).toBeCloseTo(1031 / 1228 - 1, 12);
    expect(by.get('renewables_share')).toMatchObject({ vsMedian: 'worse', change: null });
    expect(by.get('energy_per_capita')!.vsMedian).toBeNull();
    // power_wind is the only low-carbon source of the fixture; the median is over every economy with all of them (JPN, DEU)
    expect(by.get('power_low_carbon')).toMatchObject({ value: 0.01, n: 2 });
    expect(by.get('power_low_carbon')!.median).toBeCloseTo(0.155, 12);
    expect(by.has('power_wind')).toBe(false);
  });

  it('maps an economy to its WEO region and says how; the regional sentence and the renewables share', () => {
    const x = d();
    expect(weoRegionOf(x, fixtureGeo('JPN'), 'Japan')).toMatchObject({ region: 'Japan', match: 'own', mapping: 'The IEA models Japan as its own region.' });
    expect(weoRegionOf(x, fixtureGeo('DEU'), 'Germany')!.mapping).toBe('Germany is a member of the European Union, so this is the European Union’s pathway, not Germany’s.');
    expect(weoRegionOf(x, fixtureGeo('BRA'), 'Brazil')).toBeNull();
    expect(weoRegionLine(x, 'Japan')).toBe('Japan’s energy-sector CO2 was 958 Mt in 2024 (IEA estimate). The IEA’s scenarios project 629 Mt in 2035 under Stated Policies (−34%) and 705 Mt under Current Policies; in 2050, 452 Mt and 581 Mt.');
    expect(weoRegionLine(x, 'European Union')).toMatch(/^The European Union’s energy-sector CO2 was 2,380 Mt in 2024/);
    const share = weoRegionRows(x, 'Japan').find((r) => r.id === 'renewables_share')!;
    expect([share.base, share.stated35]).toEqual([0.25, 0.5]);
  });

  it('pairs the 2021 roadmap milestone; the investment headline and context; the fuel context', () => {
    const x = d();
    expect(nze21For(x, 'cap_solar')).toMatchObject({ value: 4956, base: 737, baseYear: 2020 });
    expect(nze21For(x, 'cap_wind')).toBeNull();
    expect(weiHeadline(x, fixtureItem('cell', 'solar-deployment'))!.text).toBe(
      'Solar power drew $407 bn of investment worldwide in 2025 (+142% since 2015); Asia Pacific 61% of it. The IEA estimates $366 bn for 2026.',
    );
    expect(weiHeadline(x, fixtureItem('cell', 'coal-extraction'))).toBeNull();
    expect(weiContext(x)).toBe('World energy investment in 2025: $2,110 bn in clean energy of $3,260 bn in all, against $985 bn in fossil fuel supply (2.1 to 1).');
    const f = fuelRows(x, fixtureItem('cell', 'coal-extraction'));
    expect(f.map((r) => r.fuel)).toEqual(['coal']);
    expect(f[0].share).toBeCloseTo(15.8 / 38.6, 12);
    expect(f[0].since2015).toBeCloseTo(15.8 / 14.6 - 1, 12);
  });
});

// ---- UI ------------------------------------------------------------------------------------------

const restore: (() => void)[] = [];
beforeAll(() => {
  const cw = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'clientWidth');
  Object.defineProperty(HTMLElement.prototype, 'clientWidth', { configurable: true, get: () => 900 });
  restore.push(() => cw && Object.defineProperty(HTMLElement.prototype, 'clientWidth', cw));
  window.matchMedia = ((q: string) => ({
    matches: /min-width/.test(q), media: q, onchange: null,
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
beforeEach(() => setSectorStoreForTests(new SectorStore(fixtureSource(withClimateSrc(buildSectorFixture())))));
afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}
const openGroups = async () => {
  const sec = await screen.findByText((_, el) => el?.id === 'climate', {}, { timeout: 10_000 });
  const show = await within(sec).findByRole('button', { name: /^Show/ }, { timeout: 10_000 });
  if (show.getAttribute('aria-expanded') === 'false') fireEvent.click(show);
};

describe('Country profile: fossil CO2, transition indicators and the regional pathway', () => {
  it('Japan: the GCP headline, the EI line against the median, and its own WEO region labelled as projections', async () => {
    render(
      <MemoryRouter>
        <CountryTransitionCards iso3="JPN" name="Japan" />
      </MemoryRouter>,
    );
    const head = await screen.findByTestId('policy-co2-head', {}, { timeout: 10_000 });
    expect(head.textContent).toMatch(/^Japan’s fossil CO2 emissions were 962 Mt in 2024/);
    expect(screen.getByRole('heading', { level: 3, name: 'Japan’s fossil CO2 emissions, 1990–2024' })).toBeTruthy();
    expect(screen.getByTestId('policy-ei-head').textContent).toMatch(/^Renewables were 7\.8% of Japan’s energy consumption in 2024 \(median economy 15%\)/);
    const reg = screen.getByTestId('policy-weo-region');
    expect(within(reg).getByText('IEA scenario projections · Japan')).toBeTruthy();
    expect(within(reg).getByTestId('weo-region-mapping').textContent).toMatch(/^The IEA models Japan as its own region\./);
    expect(within(reg).getByTestId('weo-region-line').textContent).toMatch(/629 Mt in 2035 under Stated Policies/);
  });

  it('Germany: the European Union pathway, stated as the EU’s, not Germany’s', async () => {
    render(
      <MemoryRouter>
        <CountryTransitionCards iso3="DEU" name="Germany" />
      </MemoryRouter>,
    );
    const reg = await screen.findByTestId('policy-weo-region', {}, { timeout: 10_000 });
    expect(within(reg).getByRole('heading', { level: 3, name: 'Where the IEA’s scenarios lead for the European Union' })).toBeTruthy();
    expect(within(reg).getByTestId('weo-region-mapping').textContent).toMatch(/not Germany’s/);
  });

  it('Brazil: says what the sources do not cover instead of showing empty figures', async () => {
    render(
      <MemoryRouter>
        <CountryTransitionCards iso3="BRA" name="Brazil" />
      </MemoryRouter>,
    );
    const co2 = await screen.findByTestId('policy-co2', {}, { timeout: 10_000 });
    expect(co2.textContent).toMatch(/reports no fossil CO2 emissions for Brazil/);
    expect(screen.getByTestId('policy-weo-region').textContent).toMatch(/has no region for Brazil/);
    expect(screen.getByTestId('policy-ei-head').textContent).toMatch(/Renewables were 50% of Brazil’s energy consumption/);
  });
});

describe('Sector profile: investment and fuel context in the Climate section', () => {
  it('Solar — Deployment: the investment headline, match quality and the World clean-versus-fossil context', async () => {
    renderAt('/sector/solar/deployment');
    await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await openGroups();
    const block = await screen.findByTestId('wei-block', {}, { timeout: 10_000 });
    expect(within(block).getByTestId('wei-head').textContent).toMatch(/^Solar power drew \$407 bn of investment worldwide in 2025/);
    expect(within(block).getByTestId('wei-match').textContent).toBe('Direct match');
    expect(within(block).getByTestId('wei-context').textContent).toMatch(/\$985 bn in fossil fuel supply/);
    expect(within(block).getByRole('heading', { level: 3, name: 'Investment in solar power, 2015–2026' })).toBeTruthy();
    expect(screen.queryByTestId('gcp-fuel')).toBeNull();
  });

  it('Coal — Extraction: World coal CO2 as context, never the sector’s own emissions', async () => {
    renderAt('/sector/coal/extraction');
    await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await openGroups();
    const fuel = await screen.findByTestId('gcp-fuel', {}, { timeout: 10_000 });
    expect(fuel.textContent).toMatch(/World fossil CO2 from coal was 15\.8 Gt in 2024, 41% of all fossil CO2/);
    expect(fuel.textContent).toMatch(/not this sector’s own emissions/);
    expect(screen.getByTestId('wei-match').textContent).toBe('No match');
  });

  it('a manufacturing sector (Refining, Components, Assembly) says why manufacturing emissions per value added are not shown', async () => {
    renderAt('/sector/solar/assembly');
    await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await openGroups();
    await screen.findByTestId('climate-sources', {}, { timeout: 10_000 });
    expect(screen.queryByTestId('value-added-note')).toBeTruthy();
  });
});
