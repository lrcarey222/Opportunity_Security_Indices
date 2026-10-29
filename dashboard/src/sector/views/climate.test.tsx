// sector-climate (climate data change 2026-10-17): a small Climate TRACE chunk on the TS fixture passes
// the contract (and its cross-table rules catch a wrong match), derives the emitters, trend, intensity and
// country rows, and renders in the Sector profile's Climate lens and section with the match quality,
// the attribution and the IPCC lifecycle line (the only climate figure of a cell with no match).

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, MemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { buildSectorFixture, fixtureGeo, fixtureItem, fixtureSource, table, type SectorFixture } from '../__fixtures__/build.ts';
import { checkChunk, CLIMATE_SPEC, refSizes, validateChunk, type SectorChunkName, type SectorClimateChunk } from '../contract.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { modelOf } from '../data/model.ts';
import { climateData, countryClimateRows, emitters, fmtLifecycle, fmtTonnes, intensityKeys, worldTrend } from '../derive/climate.ts';
import { CountryClimateCard } from './climate/CountryClimate.tsx';

type Spec = Parameters<typeof table>[0];
const t = (path: keyof typeof CLIMATE_SPEC, rows: Record<string, string | number | boolean | null>[]) => table(CLIMATE_SPEC[path] as unknown as Spec, rows);

/** The climate chunk on the fixture's geos and cells. */
function climateChunk(): Record<string, unknown> {
  const cell = (s: string) => fixtureItem('cell', s);
  const em: { src: number; geo: number; year: number; value: number }[] = [];
  const add = (src: number, year: number, vals: [string, number][]) => {
    for (const [g, v] of vals) em.push({ src, geo: fixtureGeo(g), year, value: v });
  };
  add(0, 2021, [['CHN', 80], ['USA', 20], ['WLD', 120]]);
  add(0, 2025, [['CHN', 100], ['USA', 15], ['JPN', 0], ['WLD', 140]]);
  add(1, 2021, [['CHN', 50], ['DEU', 10], ['WLD', 70]]);
  add(1, 2025, [['CHN', 60], ['DEU', 5], ['WLD', 75]]);
  add(2, 2025, [['CHN', 9e8], ['WLD', 1.9e9]]);
  em.sort((a, z) => a.src - z.src || a.year - z.year || a.geo - z.geo);
  const cells = [
    { item: cell('coal-extraction'), match: 'direct', note: 'Coal mining.' },
    { item: cell('coal-deployment'), match: 'partial', note: 'Coal-fired power plants only.' },
    { item: cell('wind-components'), match: 'none', note: 'No turbine-manufacturing subsector.' },
  ].sort((a, z) => a.item - z.item);
  const map = [
    { item: cell('coal-extraction'), src: 0, role: 'own', match: 'direct', order: 1 },
    { item: cell('coal-deployment'), src: 1, role: 'own', match: 'partial', order: 1 },
    { item: cell('wind-components'), src: 2, role: 'context', match: null, order: 1 },
  ].sort((a, z) => a.item - z.item || a.order - z.order);
  return {
    'climate.src': t('climate.src', [
      { id: 'coal-mining', subsector: 'coal-mining', sector: 'fossil-fuel-operations', label: 'Coal mining', asset_group: null, basis: 'country', repeated: false },
      { id: 'electricity-generation:coal', subsector: 'electricity-generation', sector: 'power', label: 'Electricity generation, coal-fired plants', asset_group: 'coal', basis: 'assets', repeated: false },
      { id: 'iron-and-steel', subsector: 'iron-and-steel', sector: 'manufacturing', label: 'Iron and steel', asset_group: null, basis: 'country', repeated: false },
    ]),
    'climate.map': t('climate.map', map),
    'climate.cells': t('climate.cells', cells),
    'climate.lifecycle': t('climate.lifecycle', [
      { tech: 'Coal', basis: 'ipcc', g_min: 820, g_max: 820, rows: 'Coal (pulverised) 820', proxy_of: null },
      { tech: 'Wind', basis: 'ipcc', g_min: 11, g_max: 12, rows: 'Wind - onshore 11; Wind - offshore 12', proxy_of: null },
      { tech: 'Batteries', basis: 'proxy', g_min: null, g_max: null, rows: null, proxy_of: 'Solar' },
    ]),
    'climate.em': t('climate.em', em),
    'climate.intensity': t('climate.intensity', [
      { subsector: 'electricity-generation', asset_group: 'coal', geo: fixtureGeo('CHN'), year: 2025, n: 4, emissions: 60, activity: 60, value: 1, unit: 't CO2e per MWh', activity_unit: 'MWh' },
      { subsector: 'electricity-generation', asset_group: 'coal', geo: fixtureGeo('WLD'), year: 2025, n: 9, emissions: 75, activity: 100, value: 0.75, unit: 't CO2e per MWh', activity_unit: 'MWh' },
    ]),
  };
}

/** The fixture with a sector-climate chunk listed in the manifest. */
function withClimate(fx: SectorFixture): SectorFixture {
  const chunks = { ...fx.chunks, 'sector-climate': climateChunk() };
  const files = { ...fx.files, 'sector-climate': 'sector-climate.0c11a7e5.osb' };
  const manifest = JSON.parse(JSON.stringify(fx.manifest)) as SectorFixture['manifest'];
  manifest.flags.climate = true;
  manifest.chunks.push({ name: 'sector-climate', file: files['sector-climate'], bytes: 9000, bytes_gz: 3000, md5: 'c'.repeat(32), tech: null });
  return { manifest, chunks, files };
}

describe('sector-climate: contract and derive', () => {
  const fx = buildSectorFixture();
  const meta = validateChunk('sector-meta', fx.chunks['sector-meta']);
  const ref = refSizes(meta);
  const model = modelOf(meta);

  it('passes the contract; the cross-table rules catch a wrong cell match and a wrong intensity', () => {
    expect(checkChunk('sector-climate' as SectorChunkName, climateChunk(), ref).errors).toEqual([]);
    const bad = climateChunk();
    (bad['climate.cells'] as { cols: { match: (string | null)[] } }).cols.match[0] = 'proxy';
    expect(checkChunk('sector-climate', bad, ref).errors.join('; ')).toMatch(/best match/);
    const bad2 = climateChunk();
    (bad2['climate.intensity'] as { cols: { value: Float64Array } }).cols.value[0] = 2;
    expect(checkChunk('sector-climate', bad2, ref).errors.join('; ')).toMatch(/emissions \/ activity/);
  });

  it('ranks emitters above zero, with shares of the world, the trend, intensity groups and country rows', () => {
    const d = climateData(validateChunk('sector-climate', climateChunk(), ref) as SectorClimateChunk);
    expect([d.first, d.latest]).toEqual([2021, 2025]);
    const e = emitters(d, model, 0, 2025);
    expect(e.rows.map((r) => r.g.code)).toEqual(['CHN', 'USA']);
    expect(e.rows[0].share).toBeCloseTo(100 / 140, 12);
    expect(e.rows[0].change).toBeCloseTo(0.25, 12);
    expect(worldTrend(d, model, 0)).toEqual([{ year: 2021, value: 120 }, { year: 2025, value: 140 }]);
    expect(intensityKeys(d, d.srcs[1]).map((k) => k.group)).toEqual(['coal']);
    expect(intensityKeys(d, d.srcs[0])).toEqual([]);
    const jpn = countryClimateRows(d, model, model.geoByCode.get('JPN')!);
    expect(jpn.find((r) => r.src.id === 'coal-mining')).toMatchObject({ value: 0, rank: null, share: 0 });
    const chn = countryClimateRows(d, model, model.geoByCode.get('CHN')!).find((r) => r.src.id === 'coal-mining')!;
    expect([chn.rank, chn.n]).toEqual([1, 2]);
    expect(chn.cells.map((c) => c.match)).toEqual(['direct']);
    expect(fmtTonnes(1.62e9)).toBe('1.62 Gt');
    expect(fmtTonnes(87.1e6)).toBe('87.1 Mt');
    expect(fmtLifecycle(d.lifecycle.get('Wind'))).toBe('11–12 gCO2e/kWh');
    expect(fmtLifecycle(d.lifecycle.get('Batteries'))).toBeNull();
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
beforeEach(() => setSectorStoreForTests(new SectorStore(fixtureSource(withClimate(buildSectorFixture())))));
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
  const shows = await screen.findAllByRole('button', { name: /^Show/ }, { timeout: 10_000 });
  for (const b of shows) if (b.getAttribute('aria-expanded') === 'false') fireEvent.click(b);
};

describe('Country profile: emissions in the mapped sectors', () => {
  it('lists the economy\'s subsectors with value, share, rank, change and the sectors with their match', async () => {
    render(
      <MemoryRouter>
        <CountryClimateCard iso3="CHN" name="China" />
      </MemoryRouter>,
    );
    const card = screen.getByTestId('policy-emissions');
    const table = await within(card).findByRole('table', {}, { timeout: 10_000 });
    const coal = within(table).getByText('Coal mining').closest('tr') as HTMLElement;
    expect(within(coal).getByText('100 t')).toBeTruthy();
    expect(within(coal).getByText('71.4%')).toBeTruthy();
    expect(within(coal).getByText('1 of 2')).toBeTruthy();
    expect(within(coal).getByRole('link', { name: 'Coal — Extraction' }).getAttribute('href')).toBe('/sector/coal/extraction#climate');
    expect(within(coal).getByText(/direct match/)).toBeTruthy();
    expect(within(table).getByText('Iron and steel').closest('tr')?.textContent).toMatch(/context/);
    expect(within(card).getByText(/Climate TRACE \(climatetrace\.org\), CC BY 4\.0/)).toBeTruthy();
  });
});

describe('Sector profile: Climate lens and section with Climate TRACE', () => {
  it('a matched cell: emissions headline, match quality, emitters table and lifecycle', async () => {
    renderAt('/sector/coal/extraction');
    const lens = await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await waitFor(() => expect(within(lens).getByText(/Coal mining: 140 t CO2e worldwide in 2025, \+16\.7% since 2021\. Largest emitter: China/)).toBeTruthy(), { timeout: 10_000 });
    expect(within(lens).getByText('Direct match')).toBeTruthy();
    expect(within(lens).getByText('820 gCO2e/kWh')).toBeTruthy();
    expect(within(lens).getByText(/Climate TRACE \(climatetrace\.org\), CC BY 4\.0/)).toBeTruthy();
    await openGroups();
    const block = await screen.findByTestId('climate-emissions', {}, { timeout: 10_000 });
    expect(within(block).getByRole('heading', { level: 3, name: /Coal mining: emissions by economy, 2025/ })).toBeTruthy();
    expect(within(block).getAllByText('China').length).toBeGreaterThan(0);
    expect(within(block).getByTestId('climate-lifecycle').textContent).toMatch(/820 gCO2e\/kWh/);
  });

  it('a cell with no match says so and shows the IPCC lifecycle intensity and the context material', async () => {
    renderAt('/sector/wind/components');
    const lens = await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await waitFor(() => expect(within(lens).getByText(/Climate TRACE has no emissions subsector for Wind — Components\. Wind over a plant’s life: 11–12 gCO2e\/kWh/)).toBeTruthy(), { timeout: 10_000 });
    expect(within(lens).getByText('No match')).toBeTruthy();
    await openGroups();
    const block = await screen.findByTestId('climate-emissions', {}, { timeout: 10_000 });
    expect(within(block).getByRole('heading', { level: 3, name: 'Materials, shown as context' })).toBeTruthy();
    expect(within(block).getByTestId('climate-lifecycle').textContent).toMatch(/Shown because Climate TRACE has no subsector/);
  });
});
