// IEA pathways (climate.nze_* tables of sector-climate; user request 2026-10-17): a small chunk on the TS
// fixture passes the contract (its cross-table rules catch a wrong match and a missing table), derives the
// lens headline, the pathway lines, the mineral demand by scenario and the world line, and renders in the
// Sector profile's Climate lens and section and the Country profile's emissions card, always labelled as
// IEA scenario projections for the World.

import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { createMemoryRouter, MemoryRouter, RouterProvider } from 'react-router';
import { routes } from '../../routes.tsx';
import { buildSectorFixture, fixtureGeo, fixtureItem, fixtureSource, table, type SectorFixture } from '../__fixtures__/build.ts';
import { checkChunk, CLIMATE_SPEC, refSizes, validateChunk, type SectorClimateChunk } from '../contract.ts';
import { SectorStore, setSectorStoreForTests } from '../data/store.ts';
import { modelOf } from '../data/model.ts';
import { climateData } from '../derive/climate.ts';
import { mineralPaths, nzeCompare, nzeData, nzeHeadline, nzeWorldLine, pathwayLines } from '../derive/nze.ts';
import { withClimateSrc } from '../__fixtures__/climateSrc.ts';
import { CountryTransitionCards } from './climate/CountryTransition.tsx';

type Spec = Parameters<typeof table>[0];
type Row = Record<string, string | number | boolean | null>;
const t = (path: keyof typeof CLIMATE_SPEC, rows: Row[]) => table(CLIMATE_SPEC[path] as unknown as Spec, rows);

const SERIES = [
  { id: 'cap_solar', grp: 'deployment', label: 'Solar PV capacity', unit: 'GW', category: 'Capacity: installed', product: 'Solar PV', flow: 'Electrical capacity' },
  { id: 'tes_coal', grp: 'fuel', label: 'Coal supply', unit: 'EJ', category: 'Energy', product: 'Coal', flow: 'Total energy supply' },
  { id: 'co2_energy', grp: 'emissions', label: 'Energy-sector CO2', unit: 'Mt CO2', category: 'CO2 total', product: 'Total', flow: 'Total energy supply' },
];

/** values: [hist 2023, hist 2024, stated 2035, current 2035, nze 2035, stated 2050, current 2050, nze 2050] */
function values(): Row[] {
  const v: Record<string, number[]> = { cap_solar: [1400, 1900, 4000, 3600, 6400, 8000, 7000, 12000], tes_coal: [170, 172, 150, 165, 80, 120, 160, 20], co2_energy: [37000, 37600, 34000, 38000, 12000, 30000, 39000, 0] };
  const out: Row[] = [];
  SERIES.forEach((s, i) => {
    const x = v[s.id];
    out.push({ series: i, scenario: 'historical', year: 2023, value: x[0] }, { series: i, scenario: 'historical', year: 2024, value: x[1] });
    ['stated', 'current', 'nze'].forEach((sc, k) => out.push({ series: i, scenario: sc, year: 2035, value: x[2 + k] }, { series: i, scenario: sc, year: 2050, value: x[5 + k] }));
  });
  return out;
}

function nzeTables(): Record<string, unknown> {
  const cell = (s: string) => fixtureItem('cell', s);
  const map = [
    { item: cell('solar-downstream'), series: 0, role: 'own', match: 'direct', order: 1 },
    { item: cell('solar-midstream'), series: 0, role: 'demand', match: 'partial', order: 1 },
    { item: cell('solar-upstream'), series: 0, role: 'demand', match: 'proxy', order: 1 },
    { item: cell('coal-upstream'), series: 1, role: 'own', match: 'direct', order: 1 },
    { item: cell('coal-upstream'), series: 2, role: 'context', match: null, order: 2 },
  ].sort((a, z) => a.item - z.item || a.order - z.order);
  const cells = [
    { item: cell('solar-downstream'), match: 'direct', note: 'Installed solar PV capacity.', mineral_techs: null },
    { item: cell('solar-midstream'), match: 'partial', note: 'The stock factories add to.', mineral_techs: null },
    { item: cell('solar-upstream'), match: 'proxy', note: 'Solar minerals follow solar build-out.', mineral_techs: 'Solar PV' },
    { item: cell('coal-upstream'), match: 'direct', note: 'World coal supply.', mineral_techs: null },
    { item: cell('semiconductors-upstream'), match: 'none', note: 'Not energy technologies.', mineral_techs: null },
  ].sort((a, z) => a.item - z.item);
  const minerals: Row[] = [];
  const add = (mineral: string, tech: string, k: number[], slug: string | null) =>
    (['current', 'stated', 'high_demand'] as const).forEach((sc, j) =>
      [2025, 2035, 2050].forEach((y, yi) => minerals.push({ sheet: '3.1', mineral, tech, scenario: sc, year: y, kt: yi === 0 ? k[0] : k[0] * (1 + yi * (j + 1) * 0.1), slug })),
    );
  add('Copper', 'Solar PV', [2000], 'copper');
  add('Silver', 'Solar PV', [10], 'silver');
  add('Copper', 'Wind', [500], 'copper');
  return {
    'climate.nze_series': t('climate.nze_series', SERIES),
    'climate.nze_values': t('climate.nze_values', values()),
    'climate.nze_map': t('climate.nze_map', map),
    'climate.nze_cells': t('climate.nze_cells', cells),
    'climate.nze_minerals': t('climate.nze_minerals', minerals),
    'climate.nze_meta': t('climate.nze_meta', [
      { key: 'weo', value: 'World Energy Outlook 2025' },
      { key: 'minerals_base_year', value: '2025' },
    ]),
  };
}

/** A minimal Climate TRACE part plus the nze tables. */
function climateChunk(withNze = true): Record<string, unknown> {
  return {
    'climate.src': t('climate.src', [{ id: 'coal-mining', subsector: 'coal-mining', sector: 'fossil-fuel-operations', label: 'Coal mining', asset_group: null, basis: 'country', repeated: false }]),
    'climate.map': t('climate.map', [{ item: fixtureItem('cell', 'coal-upstream'), src: 0, role: 'own', match: 'direct', order: 1 }]),
    'climate.cells': t('climate.cells', [{ item: fixtureItem('cell', 'coal-upstream'), match: 'direct', note: 'Coal mining.' }]),
    'climate.lifecycle': t('climate.lifecycle', [{ tech: 'Solar', basis: 'ipcc', g_min: 41, g_max: 48, rows: 'Solar 41', proxy_of: null }]),
    'climate.em': t('climate.em', [
      { src: 0, geo: fixtureGeo('CHN'), year: 2025, value: 100 },
      { src: 0, geo: fixtureGeo('WLD'), year: 2025, value: 140 },
    ].sort((a, z) => a.geo - z.geo)),
    'climate.intensity': t('climate.intensity', []),
    ...(withNze ? nzeTables() : {}),
  };
}

function withClimate(fx: SectorFixture): SectorFixture {
  const chunks = { ...fx.chunks, 'sector-climate': climateChunk() };
  const files = { ...fx.files, 'sector-climate': 'sector-climate.1d22b8f6.osb' };
  const manifest = JSON.parse(JSON.stringify(fx.manifest)) as SectorFixture['manifest'];
  manifest.flags.climate = true;
  manifest.chunks.push({ name: 'sector-climate', file: files['sector-climate'], bytes: 9000, bytes_gz: 3000, md5: 'd'.repeat(32), tech: null });
  return { manifest, chunks, files };
}

describe('climate.nze_*: contract and derive', () => {
  const fx = buildSectorFixture();
  const meta = fx.chunks['sector-meta'] as Parameters<typeof modelOf>[0];
  const model = modelOf(meta);
  const ref = refSizes(meta);
  const decoded = () => validateChunk('sector-climate', climateChunk(), ref) as SectorClimateChunk;

  it('passes the contract with or without the nze tables; the cross-table rules catch errors', () => {
    expect(checkChunk('sector-climate', climateChunk(), ref).errors).toEqual([]);
    expect(checkChunk('sector-climate', climateChunk(false), ref).errors).toEqual([]);
    const bad = climateChunk();
    (bad['climate.nze_cells'] as { cols: { match: (string | null)[] } }).cols.match[0] = 'none';
    expect(checkChunk('sector-climate', bad, ref).errors.join('; ')).toMatch(/best match of the cell's own and demand series/);
    const bad2 = climateChunk();
    delete bad2['climate.nze_meta'];
    expect(checkChunk('sector-climate', bad2, ref).errors.join('; ')).toMatch(/every nze table or none/);
    expect(climateData(validateChunk('sector-climate', climateChunk(false), ref) as SectorClimateChunk).nze).toBeNull();
  });

  it('derives the lens headline, the pathway lines, mineral demand by scenario and the world line', () => {
    const d = nzeData(decoded())!;
    expect([d.baseYear, d.projYears]).toEqual([2024, [2035, 2050]]);
    const solar = nzeHeadline(d, fixtureItem('cell', 'solar-downstream'))!;
    expect(solar.text).toBe('The Net Zero pathway needs 6,400 GW of solar PV capacity by 2035, 1.6 times Stated Policies (4,000 GW); 1,900 GW in 2024 (IEA estimate).');
    expect(nzeHeadline(d, fixtureItem('cell', 'solar-midstream'))!.text).toMatch(/^Demand this sector serves: the Net Zero pathway needs 6,400 GW/);
    expect(nzeHeadline(d, fixtureItem('cell', 'solar-upstream'))!.text).toMatch(/solar PV capacity \(a proxy\)/);
    const coal = nzeHeadline(d, fixtureItem('cell', 'coal-upstream'))!;
    expect(coal.text).toBe('On the Net Zero pathway, coal supply falls to 80 EJ by 2035 (−53% from 2024), against 150 EJ under Stated Policies.');
    expect(nzeHeadline(d, fixtureItem('cell', 'semiconductors-upstream'))).toBeNull();
    const lines = pathwayLines(d, d.byId.get('cap_solar')!);
    expect(lines.map((l) => l.scenario)).toEqual(['historical', 'stated', 'current', 'nze']);
    // every scenario starts at the last historical estimate, and only its own points are projections
    expect(lines[3].points).toEqual([{ x: 2024, y: 1900, projection: false }, { x: 2035, y: 6400, projection: true }, { x: 2050, y: 12000, projection: true }]);
    expect(nzeCompare(d, d.byId.get('cap_solar')!).ratio).toBeCloseTo(1.6, 12);
    // copper: Solar PV only (Wind is not the cell's technology); largest 2035 Stated Policies demand first
    const m = mineralPaths(d, ['Solar PV']);
    expect(m.map((x) => x.mineral)).toEqual(['Copper', 'Silver']);
    expect(m[0].by.get('stated')?.get(2035)).toBeCloseTo(2000 * 1.2, 9);
    expect(m[0].base).toBe(2000);
    expect(mineralPaths(d, ['Solar PV', 'Wind'])[0].by.get('stated')?.get(2025)).toBe(2500);
    expect(nzeWorldLine(d)).toBe('World context, not this economy’s: energy-sector CO2 was 37.6 Gt in 2024 (IEA estimate). The IEA’s scenarios project 34 Gt in 2035 under Stated Policies, 38 Gt under Current Policies and 12 Gt on the Net Zero pathway, reaching net zero in 2050.');
    expect(model.cellLabel(model.items[fixtureItem('cell', 'solar-downstream')])).toMatch(/Solar/);
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
beforeEach(() => setSectorStoreForTests(new SectorStore(fixtureSource(withClimateSrc(withClimate(buildSectorFixture()))))));
afterEach(() => {
  cleanup();
  setSectorStoreForTests(null);
});

function renderAt(path: string) {
  const router = createMemoryRouter(routes, { initialEntries: [path] });
  render(<RouterProvider router={router} />);
  return router;
}
/** Open the Climate group of the profile (collapsed by default). */
const openGroups = async () => {
  const sec = await screen.findByText((_, el) => el?.id === 'climate', {}, { timeout: 10_000 });
  const show = await within(sec).findByRole('button', { name: /^Show/ }, { timeout: 10_000 });
  if (show.getAttribute('aria-expanded') === 'false') fireEvent.click(show);
};

describe('Sector profile: IEA pathways in the Climate lens and section', () => {
  it('Solar — Downstream: the Net Zero line in the lens, the pathway chart and the series table in the section', async () => {
    renderAt('/sector/solar/downstream');
    const lens = await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    const line = await within(lens).findByTestId('nze-lens', {}, { timeout: 10_000 });
    expect(within(line).getByText('IEA scenario projections · World')).toBeTruthy();
    expect(line.textContent).toMatch(/The Net Zero pathway needs 6,400 GW of solar PV capacity by 2035, 1\.6 times Stated Policies \(4,000 GW\); 1,900 GW in 2024 \(IEA estimate\)\./);
    expect(line.textContent).toMatch(/World Energy Outlook 2025 free dataset \(World\), CC BY-NC-SA 4\.0/);
    await openGroups();
    const block = await screen.findByTestId('nze-pathways', {}, { timeout: 10_000 });
    expect(within(block).getByRole('heading', { level: 3, name: /Where the IEA’s pathways lead for Solar/ })).toBeTruthy();
    expect(within(block).getByTestId('nze-match').textContent).toBe('Direct match');
    expect(within(block).getByRole('heading', { level: 3, name: 'Solar PV capacity, World, to 2050' })).toBeTruthy();
    expect(within(block).getByTestId('nze-compare').textContent).toMatch(/Net Zero, 2035\s*6,400 GW\s*1\.6 times Stated Policies/);
    // the 2021 roadmap's 2030 milestone beside it, labelled as the older edition (sector-climate-src)
    const m21 = await within(block).findByTestId('nze21-milestone', {}, { timeout: 10_000 });
    expect(m21.textContent).toMatch(/Net Zero, 2030 \(2021 roadmap, older edition\)\s*4,960 GW/);
    expect(within(block).getByRole('columnheader', { name: /Net Zero 2030 \(2021 roadmap\)/ })).toBeTruthy();
    expect(within(block).getByRole('table', { name: /IEA World pathways mapped to/ })).toBeTruthy();
    expect(within(block).queryByText(/Mineral demand from/)).toBeNull();
  });

  it('Solar — Upstream: demand context and the mineral demand by scenario, with no Net Zero mineral scenario', async () => {
    renderAt('/sector/solar/upstream');
    const lens = await screen.findByTestId('lens-climate', {}, { timeout: 30_000 });
    await waitFor(() => expect(within(lens).getByTestId('nze-lens').textContent).toMatch(/Demand this sector serves: the Net Zero pathway needs/), { timeout: 10_000 });
    await openGroups();
    const block = await screen.findByTestId('nze-pathways', {}, { timeout: 10_000 });
    expect(within(block).getByRole('heading', { level: 3, name: 'Mineral demand from solar PV, by IEA scenario' })).toBeTruthy();
    expect(within(block).getAllByTestId('small-multiples').length).toBe(1);
    expect(block.textContent).toMatch(/no Net Zero scenario/);
  });

  it('a cell with no IEA series says so', async () => {
    renderAt('/sector/wind/downstream');
    await openGroups();
    const block = await screen.findByTestId('nze-pathways', {}, { timeout: 10_000 });
    expect(within(block).getByTestId('nze-match').textContent).toBe('No match');
    expect(within(block).queryByRole('heading', { name: /to 2050/ })).toBeNull();
  });
});

describe('Country profile: world pathway context', () => {
  it('adds the world line, labelled World, to the regional pathway card', async () => {
    render(
      <MemoryRouter>
        <CountryTransitionCards iso3="CHN" name="China" />
      </MemoryRouter>,
    );
    const card = await screen.findByTestId('policy-weo-region', {}, { timeout: 10_000 });
    const world = await within(card).findByTestId('nze-world', {}, { timeout: 10_000 });
    expect(world.textContent).toMatch(/World context, not this economy’s: energy-sector CO2 was 37\.6 Gt in 2024/);
    expect(world.textContent).toMatch(/Net Zero pathway for the World only, so neither China nor its region has one here/);
    expect(within(world).getByText('IEA scenario projections · World')).toBeTruthy();
  });
});
