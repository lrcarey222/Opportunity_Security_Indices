// The variable -> Sector chart table (2026-10-30, the user's request: "For the variables drop-downs under each
// fence, I want to be able to choose one of the specific variables that make up that analysis and it will then
// show the relevant chart ... from the Sectors section"). Each variable of a fence table (its column id and
// header) maps to the Sector Insights measures that chart it, in order of preference, or says why there is none.
//
//   theme, m    the Sector explore page (/sector/explore/<theme>?m=<m>): a measure of config/sector_dictionary.yml
//               in its own theme (src/posture/__tests__/variableCharts.test.ts reads the dictionary; the
//               sector-real test checks the bundle's model)
//   item        how the measure's item is found for the node: its own cell (tech + stage); the technology's
//               Deployment & Use cell; the item linked to the node's cell on the measure's axis, heaviest with
//               data (Batteries Extraction -> graphite; a stage's ILO activity); or a fixed item per technology
//               (the fuel of a power technology), or none (one value per economy). A technology the rule does
//               not list has no chart there.
//   chart       the chart shown first (sector/derive/explore.ts ExplorePart); the reader can pick another one the
//               layout has
//   stages      the stages where the measure speaks for the variable (every stage when absent); techs likewise
//   as          'share': open on "Share of world" (a world supply share)
//   basis       'source': the Sector measure comes from the source the variable reads; 'related': a Sector
//               measure of the same thing, not the variable's own input. The note says which, in a sentence.
//
// A variable with no Sector measure (a composite score, a node attribute of the Yards & Fences evidence, data
// the Sector bundle does not carry) has `none`: the picker says "No Sector chart for this variable" and why. The
// table never invents a measure. Pure: no React, no IO.

import type { SupplyStage } from '../lib/stages.ts';
import type { ExplorePart } from '../sector/derive/explore.ts';
import { writeSectorSearch, type SectorUrlState } from '../sector/url.ts';

/** The analyses whose variables the fence tables show. */
export type VariableAnalysis = 'need' | 'prize' | 'position' | 'deployment';

/** How the Sector item of a measure is found for a node. */
export type ItemRule =
  /** the node's own cell (tech + stage) */
  | { kind: 'cell' }
  /** the technology's Deployment & Use cell */
  | { kind: 'deployment' }
  /** the item linked to the node's cell on the measure's axis (Sector dims.item_links), heaviest with data */
  | { kind: 'linked' }
  /** a fixed item per technology (Solar -> the fuel `solar`) */
  | { kind: 'tech'; items: Readonly<Record<string, string>> }
  /** a measure without items: one value per economy (the lending rate) */
  | { kind: 'none' };

export interface SectorChart {
  theme: string;
  m: string;
  item: ItemRule;
  chart: ExplorePart;
  stages?: readonly SupplyStage[];
  techs?: readonly string[];
  as?: 'share';
  basis: 'source' | 'related';
  /** one sentence: what the chart shows against the variable */
  note: string;
}

export interface FenceVariable {
  /** the table column id */
  id: string;
  /** the column header */
  label: string;
  /** a column of the table, or a variable behind the analysis the table does not show */
  group: 'table' | 'behind';
  charts: readonly SectorChart[];
  /** why there is no Sector chart (charts empty) */
  none?: string;
}

// ---- the measures, each once ------------------------------------------------------------------------------

const CELL: ItemRule = { kind: 'cell' };
const DEPLOYMENT: ItemRule = { kind: 'deployment' };
const LINKED: ItemRule = { kind: 'linked' };
const PRODUCING: readonly SupplyStage[] = ['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery'];
/** The technologies whose Extraction and Refining cells link to minerals in the Sector data (dims.item_links). */
const MINERAL_TECHS = ['Batteries', 'Electric Grid', 'Electric Vehicles', 'Green Hydrogen', 'Solar', 'Wind'] as const;
const FOSSIL_FUEL: ItemRule = { kind: 'tech', items: { Oil: 'oil', Gas: 'gas', Coal: 'coal' } };
const POWER_FUEL: ItemRule = { kind: 'tech', items: { Solar: 'solar', Wind: 'wind', Nuclear: 'nuclear', Coal: 'coal', Gas: 'gas' } };

const EXPORTS_SHARE: SectorChart = {
  theme: 'trade', m: 'trade.exports', item: CELL, chart: 'map', as: 'share', stages: PRODUCING, basis: 'source',
  note: 'Share of world exports of the stage’s HS6 basket (UN Comtrade, reconciled): the export share the world supply share and the export position read.',
};
const MINE_PRODUCTION: SectorChart = {
  theme: 'production', m: 'production.mining', item: LINKED, techs: MINERAL_TECHS, chart: 'map', as: 'share', stages: ['Extraction'], basis: 'source',
  note: 'Mined supply of the node’s minerals (IEA Critical Minerals), the measure a mineral Extraction node’s world supply share reads.',
};
const REFINED_PRODUCTION: SectorChart = {
  theme: 'production', m: 'production.refining', item: LINKED, techs: MINERAL_TECHS, chart: 'map', as: 'share', stages: ['Refining'], basis: 'source',
  note: 'Refined supply of the node’s minerals (IEA Critical Minerals), the measure a mineral Refining node’s world supply share reads.',
};
const MFG_SHARE: SectorChart = {
  theme: 'manufacturing', m: 'manufacturing.share_now', item: CELL, chart: 'all', stages: ['Components', 'Assembly'], basis: 'source',
  note: 'Shares of global manufacturing capacity (IEA), the measure a manufacturing node’s world supply share reads where IEA reports it.',
};
const ANNUAL_INVESTMENT: SectorChart = {
  theme: 'investment', m: 'investment.annual', item: CELL, chart: 'map', basis: 'source',
  note: 'Annual clean-technology investment (Clean Investment Monitor), the source of investment momentum and current investment; the momentum is the change in world capex, which Over time shows.',
};
const WEO_OUTLOOK: SectorChart = {
  theme: 'consumption', m: 'consumption.weo', item: { kind: 'tech', items: { Oil: 'oil', Gas: 'gas', Coal: 'coal', Nuclear: 'nuclear', Solar: 'solar', Wind: 'wind', 'Green Hydrogen': 'hydrogen' } },
  chart: 'all', stages: PRODUCING, basis: 'source',
  note: 'World energy supply of the technology’s fuel, 2024 and the 2035 Stated Policies projection (IEA World Energy Outlook 2025): the world demand growth the node prize reads.',
};
const MINERAL_DEMAND: SectorChart = {
  theme: 'consumption', m: 'consumption.mineral_demand', item: LINKED, techs: MINERAL_TECHS, chart: 'all', stages: ['Extraction', 'Refining'], basis: 'source',
  note: 'Clean-energy demand for the node’s minerals by technology, observed and Stated Policies projections (IEA): the key-mineral demand growth a mineral stage’s node prize reads.',
};
const EV_SALES: SectorChart = {
  theme: 'consumption', m: 'consumption.ev_sales', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source',
  note: 'Electric-car sales, 2025 and the 2035 Stated Policies projection (IEA Global EV Outlook 2026), the source the electric-vehicle demand and home-market measures read.',
};
const EV_STOCK: SectorChart = {
  theme: 'consumption', m: 'consumption.ev_stock', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source',
  note: 'Electric cars on the road, 2025 and the 2035 Stated Policies projection (IEA Global EV Outlook 2026): the stock growth the home market reads for electric vehicles.',
};
/** Electricity generation by fuel (Energy Institute), a related measure wherever it appears. */
const GENERATION_MEASURE = { theme: 'production', m: 'production.generation', basis: 'related' } as const;
const GENERATION: SectorChart = {
  ...GENERATION_MEASURE, item: POWER_FUEL, chart: 'over-time',
  note: 'Electricity generation from the technology’s fuel (Energy Institute): the country’s deployment and its trend; the variable itself reads expected additions or generation growth (IEA WEO, Ember), national data only.',
};
const POWER_ADDITIONS: SectorChart = {
  theme: 'investment', m: 'investment.power_additions', item: DEPLOYMENT, techs: ['Geothermal', 'Nuclear', 'Solar', 'Wind'], chart: 'map', basis: 'related',
  note: 'Power capacity added in the year (Clean Investment Monitor): the recent build rate; the variable reads Ember capacity and additions.',
};
const DEPLOYMENT_INVESTMENT: SectorChart = {
  theme: 'investment', m: 'investment.annual', item: DEPLOYMENT, techs: ['Batteries', 'Electric Vehicles', 'Geothermal', 'Nuclear', 'Solar', 'Wind'], chart: 'map', basis: 'related',
  note: 'Deployment investment (Clean Investment Monitor): what is spent deploying the technology at home; the variable reads expected or observed additions, national data only.',
};
/** The variable renewables that storage and grids integrate: solar first, wind in the Fuel select. */
const VRE_GENERATION: SectorChart = {
  ...GENERATION_MEASURE,
  item: { kind: 'tech', items: { Batteries: 'solar', 'Electric Grid': 'solar' } },
  chart: 'over-time',
  note: 'Solar generation (Energy Institute; wind in the Fuel select): the variable renewables whose share of generation is the integration and flexibility need storage and grids answer.',
};
const OIL_USE_CHANGE: SectorChart = {
  theme: 'consumption', m: 'consumption.pc_change', item: { kind: 'tech', items: { Oil: 'oil' } }, chart: 'map', basis: 'related',
  note: 'Change in oil consumption per person, 2020-2025 (Energy Institute); the variable reads the observed change in total oil consumption.',
};
const TRADE_FLOW_PARTNERS: SectorChart = {
  theme: 'trade', m: 'trade.flow', item: CELL, chart: 'partners', stages: PRODUCING, basis: 'related',
  note: 'Where the economy’s imports of the stage’s HS6 basket come from (bilateral flows, UN Comtrade reconciled), China and the coalition in their group colours.',
};
const FEASIBILITY: SectorChart = {
  theme: 'trade', m: 'trade.feasibility', item: CELL, chart: 'map', stages: PRODUCING, basis: 'source',
  note: 'Harvard Atlas product-space feasibility of the stage’s basket: the feasibility signal of the credible-pathway test (a supporting signal, never an anchor).',
};
const MFG_CAPACITY: SectorChart = {
  theme: 'manufacturing', m: 'manufacturing.capacity', item: CELL, chart: 'all', stages: ['Refining', 'Components', 'Assembly'], basis: 'source',
  note: 'Manufacturing capacity by product and status (Clean Investment Monitor): operating, under construction and announced, the capacity the pathway test and Competitive Position read.',
};
const MFG_PIPELINE_SHARE: SectorChart = {
  theme: 'manufacturing', m: 'manufacturing.share_2035', item: CELL, chart: 'all', stages: ['Components', 'Assembly'], basis: 'related',
  note: 'Shares of announced global manufacturing capacity, the 2035 pipeline (IEA); the pathway test reads the Clean Investment Monitor’s pipeline.',
};
const WAGES: SectorChart = {
  theme: 'prices', m: 'prices.wages', item: LINKED, chart: 'map', stages: PRODUCING, basis: 'related',
  note: 'Average monthly earnings in the stage’s activity (ILO), one input cost; the pathway test reads the Input Cost Index.',
};
const LENDING_RATE: SectorChart = {
  theme: 'prices', m: 'prices.lending_rate', item: { kind: 'none' }, chart: 'map', stages: PRODUCING, basis: 'related',
  note: 'The lending rate (World Bank), the cost of capital; the pathway test reads the Input Cost Index.',
};
const MINERAL_RESERVES: SectorChart = {
  theme: 'resources', m: 'resources.mineral_reserves', item: LINKED, techs: MINERAL_TECHS, chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Recorded reserves of the node’s minerals: the resource base an Extraction node needs.',
};
const FUEL_RESERVES: SectorChart = {
  theme: 'resources', m: 'resources.fuel_reserves', item: FOSSIL_FUEL, chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Proved reserves of the fuel: the resource base an Extraction node needs.',
};
const URANIUM_RESOURCES: SectorChart = {
  theme: 'resources', m: 'resources.uranium_resources', item: CELL, techs: ['Nuclear'], chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Identified recoverable uranium resources: the resource base of Nuclear Extraction.',
};

// ---- the variables of each analysis -----------------------------------------------------------------------

const composite = (parts: string) => `A composite score: ${parts}. It has no Sector chart of its own; pick one of its variables.`;
const NODE_ATTRIBUTE = 'A node attribute of the Yards & Fences evidence, assessed node by node; the Sector data have no measure of it.';
const NODE_PRIZE_CHARTS = [MINERAL_DEMAND, WEO_OUTLOOK, EV_SALES] as const;

const NEED: readonly FenceVariable[] = [
  { id: 'need', label: 'Supply-chain vulnerability', group: 'table', charts: [], none: composite('the within-step percentile of import dependence, supplier concentration, risky supplier exposure and lack of trusted alternatives') },
  {
    id: 'import', label: 'Import dependence', group: 'table',
    charts: [
      { theme: 'trade', m: 'trade.net', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Net trade (exports minus imports) of the stage’s HS6 basket: below zero, a net importer; the variable reads net imports over GDP.' },
      { theme: 'trade', m: 'trade.imports', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Imports of the stage’s HS6 basket, before netting exports.' },
      { theme: 'consumption', m: 'consumption.self_sufficiency', item: FOSSIL_FUEL, chart: 'map', stages: ['Extraction'], basis: 'related', note: 'Production of the fuel as a percent of its consumption (Energy Institute): the fossil-fuel self-sufficiency the variable reads beside the trade balance.' },
    ],
  },
  { id: 'concentration', label: 'Supplier concentration', group: 'table', charts: [{ ...TRADE_FLOW_PARTNERS, note: `${TRADE_FLOW_PARTNERS.note} The variable is the concentration (HHI) of these import partners.` }] },
  { id: 'risky', label: 'Risky supplier exposure', group: 'table', charts: [{ ...TRADE_FLOW_PARTNERS, note: `${TRADE_FLOW_PARTNERS.note} The variable reads the share from China (with Hong Kong and Macao) and the opposite bloc.` }] },
  {
    id: 'trusted', label: 'Lack of trusted alternatives', group: 'table',
    charts: [
      { ...EXPORTS_SHARE, chart: 'key', note: 'World exports of the stage’s HS6 basket split by group (key figures): the coalition’s share is the trusted suppliers’ share of world supply the variable reads.' },
      MFG_SHARE,
      REFINED_PRODUCTION,
      MINE_PRODUCTION,
    ],
  },
];

const PRIZE: readonly FenceVariable[] = [
  { id: 'opportunity', label: 'Strategic Industrial Value', group: 'table', charts: [], none: composite('the market prize, industrial commons and future option value') },
  { id: 'market', label: 'Market prize', group: 'table', charts: [], none: composite('world demand growth and investment momentum, lifted by home-market growth') },
  { id: 'commons', label: 'Industrial commons', group: 'table', charts: [], none: NODE_ATTRIBUTE },
  { id: 'option', label: 'Future option', group: 'table', charts: [], none: NODE_ATTRIBUTE },
  { id: 'node-prize', label: 'World demand growth', group: 'table', charts: NODE_PRIZE_CHARTS },
  { id: 'home', label: 'Home-market growth', group: 'table', charts: [GENERATION, POWER_ADDITIONS, EV_SALES, EV_STOCK, OIL_USE_CHANGE, DEPLOYMENT_INVESTMENT] },
  { id: 'investment', label: 'Investment momentum', group: 'table', charts: [ANNUAL_INVESTMENT] },
  { id: 'outlook', label: 'Market outlook', group: 'table', charts: NODE_PRIZE_CHARTS },
  { id: 'need', label: 'Supply-chain vulnerability', group: 'table', charts: [], none: composite('the within-step percentile of the four need families (High fence)') },
];

const POSITION: readonly FenceVariable[] = [
  { id: 'cp', label: 'Competitive Position', group: 'table', charts: [], none: composite('the step’s demonstrated capability (export position, production, operating capacity, investment, resources, deployment scale where they apply) placed among its main producers') },
  { id: 'share', label: 'World supply share', group: 'table', charts: [MINE_PRODUCTION, REFINED_PRODUCTION, MFG_SHARE, { theme: 'production', m: 'production.fuels', item: FOSSIL_FUEL, chart: 'map', as: 'share', stages: ['Extraction'], basis: 'related', note: 'Fuel production (Energy Institute), share of world.' }, { theme: 'production', m: 'production.uranium', item: CELL, techs: ['Nuclear'], chart: 'map', as: 'share', stages: ['Extraction'], basis: 'related', note: 'Uranium mine production, share of world.' }, EXPORTS_SHARE] },
  {
    id: 'export', label: 'Export position', group: 'table',
    charts: [
      EXPORTS_SHARE,
      { theme: 'trade', m: 'trade.atlas_share', item: CELL, chart: 'map', stages: PRODUCING, basis: 'source', note: 'Share of world exports in the Harvard Atlas, which the export position uses for economies that do not report their trade.' },
      { theme: 'trade', m: 'trade.rca', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Revealed comparative advantage of the stage’s basket (exporter-reported): specialisation, which the export position read before the structural benchmark.' },
    ],
  },
  {
    id: 'structural', label: 'Structural trade share', group: 'table',
    charts: [
      { ...EXPORTS_SHARE, chart: 'over-time', basis: 'related', note: 'Share of world exports in 2020 and 2024 (UN Comtrade, reconciled); the structural benchmark averages the 2022-2024 annual exports to the world.' },
      { theme: 'trade', m: 'trade.atlas_share', item: CELL, chart: 'map', stages: PRODUCING, basis: 'source', note: 'Share of world exports in the Harvard Atlas, which the benchmark uses for economies that do not report their trade.' },
    ],
  },
  { id: 'invest', label: 'Current investment', group: 'table', charts: [ANNUAL_INVESTMENT] },
  { id: 'momentum', label: 'Trade momentum', group: 'table', charts: [], none: 'It reads monthly trade (UN Comtrade, the latest 12 months against the 12 before), which the Sector data do not carry; the monthly trade card of the Competitive Position section shows it.' },
  { id: 'feasibility', label: 'Export feasibility (pathway)', group: 'behind', charts: [FEASIBILITY] },
  { id: 'operating', label: 'Operating capacity', group: 'behind', charts: [MFG_CAPACITY, MFG_SHARE] },
  { id: 'pipeline', label: 'Committed and announced pipeline', group: 'behind', charts: [MFG_CAPACITY, MFG_PIPELINE_SHARE] },
  { id: 'input-cost', label: 'Input cost', group: 'behind', charts: [WAGES, LENDING_RATE] },
  { id: 'resource', label: 'Resource base', group: 'behind', charts: [MINERAL_RESERVES, FUEL_RESERVES, URANIUM_RESOURCES] },
];

const DEPLOYMENT_VARS: readonly FenceVariable[] = [
  { id: 'need', label: 'Deployment need', group: 'table', charts: [], none: composite('the mean of the observed family percentiles: demand adequacy, the target or climate gap and system need') },
  {
    id: 'demand', label: 'Demand adequacy', group: 'table',
    charts: [
      { ...GENERATION, note: 'Electricity generation from the technology’s fuel over time (Energy Institute); the family reads the growth of electricity demand and of the technology’s generation (IEA WEO 2025 Current Policies, else the Ember trend).' },
      { ...VRE_GENERATION, item: { kind: 'tech', items: { Batteries: 'solar' } }, note: 'Solar generation over time (Energy Institute; wind in the Fuel select): for Batteries the family reads the growth of the variable renewables storage has to firm.' },
      { theme: 'consumption', m: 'consumption.ev_sales_share', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source', note: 'Electric share of new-car sales (IEA Global EV Outlook 2026); the family reads its change per year.' },
      { ...OIL_USE_CHANGE, note: 'Change in oil consumption per person, 2020-2025 (Energy Institute); the family reads the observed growth of total oil consumption.' },
    ],
  },
  { id: 'gap', label: 'Target / climate gap', group: 'table', charts: [{ ...POWER_ADDITIONS, note: 'Power capacity added in the year (Clean Investment Monitor): the build rate side of the gap. The national targets and approved plans the family reads (Ember 2030 target tracker) are not in the Sector data.' }] },
  {
    id: 'system', label: 'System need', group: 'table',
    charts: [
      {
        theme: 'consumption', m: 'consumption.balance',
        item: { kind: 'tech', items: { Solar: 'fossil', Wind: 'fossil', Nuclear: 'fossil', Geothermal: 'fossil', Coal: 'fossil', Gas: 'fossil', 'Electric Vehicles': 'oil', 'Green Hydrogen': 'gas' } },
        chart: 'map', basis: 'related',
        note: 'Net balance (production minus consumption) of the fuel the technology displaces (Energy Institute; fossil sums oil, gas and coal): a net importer has imported fuel to displace, one of the family’s measures.',
      },
      VRE_GENERATION,
    ],
  },
  { id: 'alt', label: 'Climate-aligned (alternative)', group: 'table', charts: [], none: 'An alternative, world-level climate-aligned scenario shown beside the choice and never used in it; the Sector data have no country measure of it.' },
  { id: 'trend', label: 'Deployment trend', group: 'table', charts: [GENERATION, { ...EV_SALES, chart: 'over-time' }, { ...POWER_ADDITIONS, chart: 'over-time' }, { ...DEPLOYMENT_INVESTMENT, chart: 'over-time' }] },
];

/** Every analysis' variables, in the table's column order then the ones behind it. */
export const VARIABLE_CHARTS: Readonly<Record<VariableAnalysis, readonly FenceVariable[]>> = {
  need: NEED,
  prize: PRIZE,
  position: POSITION,
  deployment: DEPLOYMENT_VARS,
};

/**
 * The variables a fence table's picker offers: the analysis' variables, the prize's labelled by the core's
 * prize measure; without Strategic Industrial Value (a core whose prize is the market measure) the prize is
 * Industrial Opportunity and the three families are not offered.
 */
export function fenceVariables(analysis: VariableAnalysis, opts: { siv?: boolean; prizeLabel?: string } = {}): FenceVariable[] {
  const vs = VARIABLE_CHARTS[analysis];
  if (analysis !== 'prize') return [...vs];
  const siv = opts.siv ?? true;
  return vs
    .filter((v) => siv || !['market', 'commons', 'option'].includes(v.id))
    .map((v) =>
      v.id === 'opportunity'
        ? { ...v, label: opts.prizeLabel ?? v.label, none: siv ? v.none : composite('world demand growth and investment momentum, lifted by home-market growth') }
        : v,
    );
}

/** The node a chart is for: its technology and stage (the data vocabulary: "Electric Grid", "Assembly"). */
export interface ChartNode {
  tech: string;
  stage: string;
}

/** The charts of a variable that speak for it at this node (its stage and technology), in order of preference. */
export function chartsAt(v: FenceVariable, node: ChartNode): SectorChart[] {
  return v.charts.filter(
    (c) =>
      (!c.stages || (c.stages as readonly string[]).includes(node.stage)) &&
      (!c.techs || c.techs.includes(node.tech)) &&
      (c.item.kind !== 'tech' || Object.hasOwn(c.item.items, node.tech)),
  );
}

/**
 * The Sector state a chart opens on for a node and economy, as far as it is known without the Sector model:
 * the measure, the cell (tech + stage) or the technology's fixed item, the highlight and Share of world. A
 * linked item is resolved by the embedded explorer once the model is in.
 */
export function chartState(c: SectorChart, node: ChartNode, iso3: string | null): Partial<SectorUrlState> {
  const s: Partial<SectorUrlState> = { m: c.m, c: iso3 };
  if (c.item.kind === 'cell') Object.assign(s, { tech: node.tech, stage: node.stage });
  else if (c.item.kind === 'deployment') Object.assign(s, { tech: node.tech, stage: 'Deployment' });
  else if (c.item.kind === 'tech') s.item = c.item.items[node.tech] ?? null;
  if (c.as === 'share') s.as = 'share';
  return s;
}

/** The Sector explore page of a theme with a state: /sector/explore/<theme>?c=..&m=..&tech=..&stage=.. */
export function exploreHref(theme: string, state: Partial<SectorUrlState>): string {
  const q = writeSectorSearch(state);
  return `/sector/explore/${theme}${q ? `?${q}` : ''}`;
}
