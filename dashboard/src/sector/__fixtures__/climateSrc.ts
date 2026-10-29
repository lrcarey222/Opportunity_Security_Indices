// A small sector-climate-src chunk on the TS fixture (climate sources, user request 2026-10-17): GCP fossil
// CO2 for Japan, Germany and the World and World CO2 by fuel; EI tracker indicators for Japan, Germany and
// Brazil with their medians; the IEA WEO regions Japan (own) and European Union (Germany); one NZE 2021
// series paired with cap_solar; World Energy Investment rows; the cell map of Solar Deployment (WEI solar)
// and Coal Extraction (GCP coal). Shared by sources.test.tsx and nze.test.tsx.

import { CLIMATE_SRC_SPEC } from '../contract.ts';
import { fixtureGeo, fixtureItem, table, type SectorFixture } from './build.ts';

type Spec = Parameters<typeof table>[0];
type Row = Record<string, string | number | boolean | null>;
const t = (path: keyof typeof CLIMATE_SRC_SPEC, rows: Row[]) => table(CLIMATE_SRC_SPEC[path] as unknown as Spec, rows);

export function climateSrcChunk(): Record<string, unknown> {
  const G = (c: string) => fixtureGeo(c);
  const gcp: Row[] = [];
  const add = (geo: string, measure: string, pts: [number, number][]) => pts.forEach(([year, value]) => gcp.push({ geo: G(geo), measure, year, value }));
  add('WLD', 'territorial', [[1990, 22700], [2015, 35500], [2024, 38600]]);
  add('JPN', 'territorial', [[1990, 1150], [2015, 1220], [2023, 987], [2024, 962]]);
  add('JPN', 'consumption', [[1990, 1360], [2023, 1142]]);
  add('JPN', 'transfers', [[1990, -210], [2023, -155]]);
  add('DEU', 'territorial', [[1990, 1055], [2015, 800], [2024, 572]]);
  gcp.sort((a, z) => (a.geo as number) - (z.geo as number));
  const ei: Row[] = [];
  const e = (ind: string, year: number, v: Record<string, number>) => Object.entries(v).forEach(([geo, value]) => ei.push({ geo: G(geo), ind, year, value }));
  e('ghg', 2017, { JPN: 1228, DEU: 792, BRA: 400 });
  e('ghg', 2024, { JPN: 1031, DEU: 583, BRA: 450 });
  e('renewables_share', 2024, { JPN: 0.078, DEU: 0.15, BRA: 0.5 });
  e('carbon_intensity', 2024, { JPN: 62.7, DEU: 57.6, BRA: 40 });
  e('energy_per_capita', 2024, { JPN: 133 });
  e('power_wind', 2024, { JPN: 0.01, DEU: 0.3 });
  e('power_coal', 2024, { JPN: 0.3, DEU: 0.2 });
  const wv: Row[] = [];
  const w = (region: string, series: number, pts: [string, number, number][]) => pts.forEach(([scenario, year, value]) => wv.push({ region, series, scenario, year, value }));
  w('Japan', 0, [['historical', 2024, 958], ['stated', 2035, 629], ['current', 2035, 705], ['stated', 2050, 452], ['current', 2050, 581]]);
  w('Japan', 1, [['historical', 2024, 1000], ['stated', 2035, 1000]]);
  w('Japan', 2, [['historical', 2024, 250], ['stated', 2035, 500]]);
  w('European Union', 0, [['historical', 2024, 2380], ['stated', 2035, 1480], ['current', 2035, 1640], ['stated', 2050, 710], ['current', 2050, 995]]);
  const iv: Row[] = [];
  const i = (row: number, region: string, pts: [number, number][]) => pts.forEach(([year, value]) => iv.push({ row, region, year, value, estimate: year === 2026 }));
  i(0, 'World', [[2025, 3261]]);
  i(1, 'World', [[2025, 2105]]);
  i(2, 'World', [[2025, 985]]);
  i(3, 'World', [[2015, 168], [2025, 407], [2026, 366]]);
  i(3, 'Asia Pacific', [[2015, 100], [2025, 250], [2026, 220]]);
  i(3, 'Europe', [[2015, 30], [2025, 62], [2026, 60]]);
  const cell = (s: string) => fixtureItem('cell', s);
  const map = [
    { item: cell('solar-deployment'), src: 'wei', key: 'solar', role: 'own', match: 'direct', order: 1 },
    { item: cell('coal-extraction'), src: 'gcp', key: 'coal', role: 'context', match: null, order: 1 },
  ].sort((a, z) => a.item - z.item);
  const cells = [
    { item: cell('solar-deployment'), match: 'direct', note: 'Investment in solar power.' },
    { item: cell('coal-extraction'), match: 'none', note: 'Coal.' },
  ].sort((a, z) => a.item - z.item);
  return {
    'csrc.gcp': t('csrc.gcp', gcp),
    'csrc.gcp_fuel': t('csrc.gcp_fuel', [
      { fuel: 'total', year: 2015, value: 35.5 }, { fuel: 'total', year: 2024, value: 38.6 },
      { fuel: 'coal', year: 2015, value: 14.6 }, { fuel: 'coal', year: 2024, value: 15.8 },
    ]),
    'csrc.ei': t('csrc.ei', ei),
    'csrc.ei_ind': t('csrc.ei_ind', [
      { id: 'ghg', label: 'Energy-related greenhouse gas emissions', unit: 'Mt CO2e', better: 'lower', sheet: 'Greenhouse Gas Emissions', low_carbon: null },
      { id: 'renewables_share', label: 'Renewables’ share of energy consumption', unit: 'share', better: 'higher', sheet: 'Renewable Energy Consumption', low_carbon: null },
      { id: 'carbon_intensity', label: 'Carbon intensity of energy', unit: 't CO2e per TJ', better: 'lower', sheet: 'Carbon Intensity', low_carbon: null },
      { id: 'energy_per_capita', label: 'Energy consumption per person', unit: 'GJ per person', better: 'none', sheet: 'Energy Consumption per Capita', low_carbon: null },
      { id: 'power_wind', label: 'Wind’s share of power generation', unit: 'share', better: 'none', sheet: 'Power Sector Decarbonisation', low_carbon: true },
      { id: 'power_coal', label: 'Coal’s share of power generation', unit: 'share', better: 'none', sheet: 'Power Sector Decarbonisation', low_carbon: false },
    ]),
    'csrc.ei_median': t('csrc.ei_median', [
      { ind: 'ghg', year: 2017, value: 792, n: 3 }, { ind: 'ghg', year: 2024, value: 583, n: 3 }, { ind: 'renewables_share', year: 2024, value: 0.15, n: 3 },
      { ind: 'carbon_intensity', year: 2024, value: 57.6, n: 3 }, { ind: 'energy_per_capita', year: 2024, value: 133, n: 1 },
      { ind: 'power_wind', year: 2024, value: 0.155, n: 2 }, { ind: 'power_coal', year: 2024, value: 0.25, n: 2 },
    ]),
    'csrc.weo_series': t('csrc.weo_series', [
      { id: 'co2_energy', label: 'Energy-sector CO2', unit: 'Mt CO2', category: 'CO2 total', product: 'Total', flow: 'Total energy supply' },
      { id: 'gen_total', label: 'Electricity generation', unit: 'TWh', category: 'Energy', product: 'Total', flow: 'Electricity generation' },
      { id: 'gen_renewables', label: 'Electricity from renewables', unit: 'TWh', category: 'Energy', product: 'Renewables', flow: 'Electricity generation' },
    ]),
    'csrc.weo_values': t('csrc.weo_values', wv),
    'csrc.weo_regions': t('csrc.weo_regions', [{ region: 'Japan', kind: 'country' }, { region: 'European Union', kind: 'union' }]),
    'csrc.weo_geo': t('csrc.weo_geo', [{ geo: G('DEU'), region: 'European Union', match: 'member' }, { geo: G('JPN'), region: 'Japan', match: 'own' }].sort((a, z) => a.geo - z.geo)),
    'csrc.nze21_series': t('csrc.nze21_series', [
      { id: 'cap_solar', label: 'Solar PV capacity', unit: 'GW', category: 'Capacity', product: 'Solar PV', flow: 'Installed total power capacity', weo: 'cap_solar', match: 'direct', note: null },
    ]),
    'csrc.nze21_values': t('csrc.nze21_values', [{ series: 0, year: 2020, value: 737 }, { series: 0, year: 2030, value: 4956 }]),
    'csrc.wei_rows': t('csrc.wei_rows', [
      { id: 'total', label: 'Total Billion USD (2025, MER)', name: 'Total energy investment', parent: null, unit: 'Billion USD (2025, MER)' },
      { id: 'clean', label: 'of which: Clean energy', name: 'Clean energy', parent: 'total', unit: 'Billion USD (2025, MER)' },
      { id: 'fossil_fuels', label: 'Fossil fuels', name: 'Fossil fuel supply', parent: null, unit: 'Billion USD (2025, MER)' },
      { id: 'solar', label: 'o/w solar', name: 'Solar power', parent: null, unit: 'Billion USD (2025, MER)' },
    ]),
    'csrc.wei_values': t('csrc.wei_values', iv),
    'csrc.wei_regions': t('csrc.wei_regions', [
      { region: 'World', label: 'World', kind: 'world' }, { region: 'Asia Pacific', label: 'Asia Pacific', kind: 'region' }, { region: 'Europe', label: 'Europe', kind: 'region' },
    ]),
    'csrc.map': t('csrc.map', map),
    'csrc.cells': t('csrc.cells', cells),
    'csrc.meta': t('csrc.meta', [{ key: 'gcp', value: 'Global Carbon Budget 2025 v1.0' }]),
  };
}

/** The fixture with a sector-climate-src chunk listed in the manifest. */
export function withClimateSrc(fx: SectorFixture): SectorFixture {
  const chunks = { ...fx.chunks, 'sector-climate-src': climateSrcChunk() };
  const files = { ...fx.files, 'sector-climate-src': 'sector-climate-src.5e1c0a2b.osb' };
  const manifest = JSON.parse(JSON.stringify(fx.manifest)) as SectorFixture['manifest'];
  manifest.flags.climate_src = true;
  manifest.chunks.push({ name: 'sector-climate-src', file: files['sector-climate-src'], bytes: 9000, bytes_gz: 3000, md5: 'e'.repeat(32), tech: null });
  return { manifest, chunks, files };
}
