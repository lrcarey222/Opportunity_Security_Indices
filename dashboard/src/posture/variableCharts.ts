// The variable -> Sector chart table (2026-10-30, the user's request: "For the variables drop-downs under each
// fence, I want to be able to choose one of the specific variables that make up that analysis and it will then
// show the relevant chart ... from the Sectors section"). Reworked 2026-10-31 (the user: "The 'Show a Variable'
// function isn't working because it's only showing the composites. It should show the variables that make up
// the composites"): the picker offers indicators, never a composite or a family score.
//
// Each analysis (a fence table) is its composite's families, in the composite's own structure: the need's four
// families; the prize's market prize (its three parts) then industrial commons and future option; Competitive
// Position's families, then the two credible-pathway tests behind Catch Up; deployment need's three families, the
// climate-aligned alternative and the deployment trend. Each family lists its indicators: one Sector chart each
// (a SectorChart, with a short plain `label`), in order of preference, or says why it has none.
//
//   label       the indicator in a few plain words: the picker's option ("Imports of the step’s goods")
//   theme, m    the Sector explore page (/sector/explore/<theme>?m=<m>): a measure of config/sector_dictionary.yml
//               in its own theme (src/posture/__tests__/variableCharts.test.ts reads the dictionary; the
//               sector-real test checks the bundle's model)
//   item        how the measure's item is found for the step: its own cell (tech + stage); the technology's
//               Deployment & Use cell; the item linked to the step's cell on the measure's axis, heaviest with
//               data (Batteries Extraction -> graphite; a stage's ILO activity); or a fixed item per technology
//               (the fuel of a power technology), or none (one value per economy). A technology the rule does
//               not list has no chart there.
//   chart       the chart shown first (sector/derive/explore.ts ExplorePart); the reader can pick another one the
//               layout has
//   stages      the stages where the measure speaks for the family (every stage when absent); techs likewise
//   as          'share': open on "Share of world" (a world supply share)
//   basis       'source': the Sector measure comes from the source the family reads; 'related': a Sector
//               measure of the same thing, not the family's own input. The note says which, in a sentence.
//
// A family with no Sector measure (a step attribute of the Yards & Fences evidence, data the Sector bundle does
// not carry) has `none` and is named under the picker with where to see it instead (the row's details; the
// monthly trade card for trade momentum). The table never invents a measure. Pure: no React, no IO.

import type { SupplyStage } from '../lib/stages.ts';
import type { ExplorePart } from '../sector/derive/explore.ts';
import { writeSectorSearch, type SectorUrlState } from '../sector/url.ts';
import { MARKET_PRIZE_LABEL } from './grammar.ts';

/** The analyses whose families the fence tables show. */
export type VariableAnalysis = 'need' | 'prize' | 'position' | 'deployment';

/** How the Sector item of a measure is found for a step. */
export type ItemRule =
  /** the step's own cell (tech + stage) */
  | { kind: 'cell' }
  /** the technology's Deployment & Use cell */
  | { kind: 'deployment' }
  /** the item linked to the step's cell on the measure's axis (Sector dims.item_links), heaviest with data */
  | { kind: 'linked' }
  /** a fixed item per technology (Solar -> the fuel `solar`) */
  | { kind: 'tech'; items: Readonly<Record<string, string>> }
  /** a measure without items: one value per economy (the lending rate) */
  | { kind: 'none' };

/** One indicator: a Sector measure that charts a family. */
export interface SectorChart {
  /** the indicator in a few plain words, the picker's option: what is measured */
  label: string;
  theme: string;
  m: string;
  item: ItemRule;
  chart: ExplorePart;
  stages?: readonly SupplyStage[];
  techs?: readonly string[];
  as?: 'share';
  basis: 'source' | 'related';
  /** one sentence: what the chart shows against the family */
  note: string;
}

/** A family of a composite score: its indicators, or why it has none. */
export interface FenceFamily {
  /** the family's id (the table's column id where the table shows it) */
  id: string;
  /** the family's name (the column header where the table shows it) */
  label: string;
  /** the part of the composite the family feeds, where that is not the composite itself ("market prize") */
  part?: string;
  /** its indicators, in order of preference */
  charts: readonly SectorChart[];
  /** why there is no Sector chart (charts empty) */
  none?: string;
  /** a chartless family whose trade the Position section's monthly trade card shows (trade momentum) */
  monthly?: boolean;
}

// ---- the indicators, each once ----------------------------------------------------------------------------

const CELL: ItemRule = { kind: 'cell' };
const DEPLOYMENT: ItemRule = { kind: 'deployment' };
const LINKED: ItemRule = { kind: 'linked' };
const PRODUCING: readonly SupplyStage[] = ['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery'];
/** The technologies whose Extraction and Refining cells link to minerals in the Sector data (dims.item_links). */
const MINERAL_TECHS = ['Batteries', 'Electric Grid', 'Electric Vehicles', 'Green Hydrogen', 'Solar', 'Wind'] as const;
const FOSSIL_FUEL: ItemRule = { kind: 'tech', items: { Oil: 'oil', Gas: 'gas', Coal: 'coal' } };
const POWER_FUEL: ItemRule = { kind: 'tech', items: { Solar: 'solar', Wind: 'wind', Nuclear: 'nuclear', Coal: 'coal', Gas: 'gas' } };

const EXPORTS_SHARE: SectorChart = {
  label: 'Exports, share of world',
  theme: 'trade', m: 'trade.exports', item: CELL, chart: 'map', as: 'share', stages: PRODUCING, basis: 'source',
  note: 'Share of world exports of the stage’s HS6 basket (UN Comtrade, reconciled): the export share the world supply share and the export position read.',
};
const ATLAS_SHARE = { label: 'Exports, share of world (Harvard Atlas)', theme: 'trade', m: 'trade.atlas_share', item: CELL, chart: 'map', stages: PRODUCING, basis: 'source' } as const;
const MINE_PRODUCTION: SectorChart = {
  label: 'Mine production, share of world',
  theme: 'production', m: 'production.mining', item: LINKED, techs: MINERAL_TECHS, chart: 'map', as: 'share', stages: ['Extraction'], basis: 'source',
  note: 'Mined supply of the step’s minerals (IEA Critical Minerals), the measure a mineral Extraction step’s world supply share reads.',
};
const REFINED_PRODUCTION: SectorChart = {
  label: 'Refined production, share of world',
  theme: 'production', m: 'production.refining', item: LINKED, techs: MINERAL_TECHS, chart: 'map', as: 'share', stages: ['Refining'], basis: 'source',
  note: 'Refined supply of the step’s minerals (IEA Critical Minerals), the measure a mineral Refining step’s world supply share reads.',
};
const MFG_SHARE: SectorChart = {
  label: 'Share of world manufacturing capacity',
  theme: 'manufacturing', m: 'manufacturing.share_now', item: CELL, chart: 'all', stages: ['Components', 'Assembly'], basis: 'source',
  note: 'Shares of global manufacturing capacity (IEA), the measure a manufacturing step’s world supply share reads where IEA reports it.',
};
const ANNUAL_INVESTMENT: SectorChart = {
  label: 'Annual clean-technology investment',
  theme: 'investment', m: 'investment.annual', item: CELL, chart: 'map', basis: 'source',
  note: 'Annual clean-technology investment (Clean Investment Monitor), the source of investment momentum and current investment; the momentum is the change in world capex, which Over time shows.',
};
const WEO_OUTLOOK: SectorChart = {
  label: 'World energy supply outlook',
  theme: 'consumption', m: 'consumption.weo', item: { kind: 'tech', items: { Oil: 'oil', Gas: 'gas', Coal: 'coal', Nuclear: 'nuclear', Solar: 'solar', Wind: 'wind', 'Green Hydrogen': 'hydrogen' } },
  chart: 'all', stages: PRODUCING, basis: 'source',
  note: 'World energy supply of the technology’s fuel, 2024 and the 2035 Stated Policies projection (IEA World Energy Outlook 2025): the world demand growth the step’s prize reads.',
};
const MINERAL_DEMAND: SectorChart = {
  label: 'Clean-energy demand for the step’s minerals',
  theme: 'consumption', m: 'consumption.mineral_demand', item: LINKED, techs: MINERAL_TECHS, chart: 'all', stages: ['Extraction', 'Refining'], basis: 'source',
  note: 'Clean-energy demand for the step’s minerals by technology, observed and Stated Policies projections (IEA): the key-mineral demand growth a mineral stage’s prize reads.',
};
const EV_SALES: SectorChart = {
  label: 'Electric-car sales',
  theme: 'consumption', m: 'consumption.ev_sales', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source',
  note: 'Electric-car sales, 2025 and the 2035 Stated Policies projection (IEA Global EV Outlook 2026), the source the electric-vehicle demand and home-market measures read.',
};
const EV_STOCK: SectorChart = {
  label: 'Electric cars on the road',
  theme: 'consumption', m: 'consumption.ev_stock', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source',
  note: 'Electric cars on the road, 2025 and the 2035 Stated Policies projection (IEA Global EV Outlook 2026): the stock growth the home market reads for electric vehicles.',
};
/** Electricity generation by fuel (Energy Institute), a related measure wherever it appears. */
const GENERATION_MEASURE = { theme: 'production', m: 'production.generation', basis: 'related' } as const;
const GENERATION: SectorChart = {
  ...GENERATION_MEASURE, label: 'Electricity generation from the technology', item: POWER_FUEL, chart: 'over-time',
  note: 'Electricity generation from the technology’s fuel (Energy Institute): the country’s deployment and its trend; the score itself reads expected additions or generation growth (IEA WEO, Ember), national data only.',
};
const POWER_ADDITIONS: SectorChart = {
  label: 'Power capacity added',
  theme: 'investment', m: 'investment.power_additions', item: DEPLOYMENT, techs: ['Geothermal', 'Nuclear', 'Solar', 'Wind'], chart: 'map', basis: 'related',
  note: 'Power capacity added in the year (Clean Investment Monitor): the recent build rate; the score reads Ember capacity and additions.',
};
const DEPLOYMENT_INVESTMENT: SectorChart = {
  label: 'Deployment investment',
  theme: 'investment', m: 'investment.annual', item: DEPLOYMENT, techs: ['Batteries', 'Electric Vehicles', 'Geothermal', 'Nuclear', 'Solar', 'Wind'], chart: 'map', basis: 'related',
  note: 'Deployment investment (Clean Investment Monitor): what is spent deploying the technology at home; the score reads expected or observed additions, national data only.',
};
/** The variable renewables that storage and grids integrate: solar first, wind in the Fuel select. */
const VRE_GENERATION: SectorChart = {
  ...GENERATION_MEASURE,
  label: 'Solar and wind generation',
  item: { kind: 'tech', items: { Batteries: 'solar', 'Electric Grid': 'solar' } },
  chart: 'over-time',
  note: 'Solar generation (Energy Institute; wind in the Fuel select): the variable renewables whose share of generation is the integration and flexibility need storage and grids answer.',
};
const OIL_USE_CHANGE: SectorChart = {
  label: 'Change in oil use per person',
  theme: 'consumption', m: 'consumption.pc_change', item: { kind: 'tech', items: { Oil: 'oil' } }, chart: 'map', basis: 'related',
  note: 'Change in oil consumption per person, 2020-2025 (Energy Institute); the score reads the observed change in total oil consumption.',
};
const TRADE_FLOW_PARTNERS = {
  theme: 'trade', m: 'trade.flow', item: CELL, chart: 'partners', stages: PRODUCING, basis: 'related',
  note: 'Where the economy’s imports of the stage’s HS6 basket come from (bilateral flows, UN Comtrade reconciled), China and the coalition in their group colours.',
} as const;
const FEASIBILITY: SectorChart = {
  label: 'Export feasibility (Harvard Atlas)',
  theme: 'trade', m: 'trade.feasibility', item: CELL, chart: 'map', stages: PRODUCING, basis: 'source',
  note: 'Harvard Atlas product-space feasibility of the stage’s basket: the feasibility signal of the credible-pathway test (a supporting signal, never an anchor).',
};
const MFG_CAPACITY: SectorChart = {
  label: 'Manufacturing capacity by status',
  theme: 'manufacturing', m: 'manufacturing.capacity', item: CELL, chart: 'all', stages: ['Refining', 'Components', 'Assembly'], basis: 'source',
  note: 'Manufacturing capacity by product and status (Clean Investment Monitor): operating, under construction and announced, the capacity the pathway test and Competitive Position read.',
};
const MFG_PIPELINE_SHARE: SectorChart = {
  label: 'Announced capacity, share of world 2035',
  theme: 'manufacturing', m: 'manufacturing.share_2035', item: CELL, chart: 'all', stages: ['Components', 'Assembly'], basis: 'related',
  note: 'Shares of announced global manufacturing capacity, the 2035 pipeline (IEA); the pathway test reads the Clean Investment Monitor’s pipeline.',
};
const WAGES: SectorChart = {
  label: 'Wages in the step’s activity',
  theme: 'prices', m: 'prices.wages', item: LINKED, chart: 'map', stages: PRODUCING, basis: 'related',
  note: 'Average monthly earnings in the stage’s activity (ILO), one input cost; the pathway test reads the Input Cost Index.',
};
const LENDING_RATE: SectorChart = {
  label: 'Lending rate',
  theme: 'prices', m: 'prices.lending_rate', item: { kind: 'none' }, chart: 'map', stages: PRODUCING, basis: 'related',
  note: 'The lending rate (World Bank), the cost of capital; the pathway test reads the Input Cost Index.',
};
const MINERAL_RESERVES: SectorChart = {
  label: 'Mineral reserves',
  theme: 'resources', m: 'resources.mineral_reserves', item: LINKED, techs: MINERAL_TECHS, chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Recorded reserves of the step’s minerals: the resource base an Extraction step needs.',
};
const FUEL_RESERVES: SectorChart = {
  label: 'Fuel reserves',
  theme: 'resources', m: 'resources.fuel_reserves', item: FOSSIL_FUEL, chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Proved reserves of the fuel: the resource base an Extraction step needs.',
};
const URANIUM_RESOURCES: SectorChart = {
  label: 'Uranium resources',
  theme: 'resources', m: 'resources.uranium_resources', item: CELL, techs: ['Nuclear'], chart: 'map', stages: ['Extraction'], basis: 'source',
  note: 'Identified recoverable uranium resources: the resource base of Nuclear Extraction.',
};

// ---- the families of each composite -----------------------------------------------------------------------

const STEP_ATTRIBUTE = 'An attribute of each supply-chain step in the Yards & Fences evidence, assessed step by step; the Sector data have no measure of it.';
/** The prize's market part (Industrial Opportunity): world demand growth and investment momentum, lifted by home-market growth. */
const MARKET_PART = 'market prize';
/** The two tests behind a Catch Up choice that are not families of Competitive Position. */
const PATHWAY_PART = 'credible pathway';

/** Supply-chain vulnerability: its four families. */
const NEED: readonly FenceFamily[] = [
  {
    id: 'import', label: 'Import dependence',
    charts: [
      { label: 'Net trade in the step’s goods', theme: 'trade', m: 'trade.net', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Net trade (exports minus imports) of the stage’s HS6 basket: below zero, a net importer; the score reads net imports over GDP.' },
      { label: 'Imports of the step’s goods', theme: 'trade', m: 'trade.imports', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Imports of the stage’s HS6 basket, before netting exports.' },
      { label: 'Fuel production as a share of use', theme: 'consumption', m: 'consumption.self_sufficiency', item: FOSSIL_FUEL, chart: 'map', stages: ['Extraction'], basis: 'related', note: 'Production of the fuel as a percent of its consumption (Energy Institute): the fossil-fuel self-sufficiency the score reads beside the trade balance.' },
    ],
  },
  { id: 'concentration', label: 'Supplier concentration', charts: [{ ...TRADE_FLOW_PARTNERS, label: 'Who supplies the imports', note: `${TRADE_FLOW_PARTNERS.note} The score is the concentration (HHI) of these import partners.` }] },
  { id: 'risky', label: 'Risky supplier exposure', charts: [{ ...TRADE_FLOW_PARTNERS, label: 'Imports from China and the opposite bloc', note: `${TRADE_FLOW_PARTNERS.note} The score reads the share from China (with Hong Kong and Macao) and the opposite bloc.` }] },
  {
    id: 'trusted', label: 'Lack of trusted alternatives',
    charts: [
      { ...EXPORTS_SHARE, label: 'World exports by supplier group', chart: 'key', note: 'World exports of the stage’s HS6 basket split by group (key figures): the coalition’s share is the trusted suppliers’ share of world supply the score reads.' },
      MFG_SHARE,
      REFINED_PRODUCTION,
      MINE_PRODUCTION,
    ],
  },
];

/** Strategic Industrial Value: the market prize's three parts, industrial commons and future option value. */
const PRIZE: readonly FenceFamily[] = [
  { id: 'node-prize', label: 'World demand growth', part: MARKET_PART, charts: [MINERAL_DEMAND, WEO_OUTLOOK, EV_SALES] },
  { id: 'home', label: 'Home-market growth', part: MARKET_PART, charts: [GENERATION, POWER_ADDITIONS, EV_SALES, EV_STOCK, OIL_USE_CHANGE, DEPLOYMENT_INVESTMENT] },
  { id: 'investment', label: 'Investment momentum', part: MARKET_PART, charts: [ANNUAL_INVESTMENT] },
  { id: 'commons', label: 'Industrial commons', charts: [], none: STEP_ATTRIBUTE },
  { id: 'option', label: 'Future option', charts: [], none: STEP_ATTRIBUTE },
];

/** Competitive Position: its families (the table's columns first), then the credible-pathway tests behind Catch Up. */
const POSITION: readonly FenceFamily[] = [
  {
    id: 'share', label: 'World supply share',
    charts: [
      MINE_PRODUCTION,
      REFINED_PRODUCTION,
      MFG_SHARE,
      { label: 'Fuel production, share of world', theme: 'production', m: 'production.fuels', item: FOSSIL_FUEL, chart: 'map', as: 'share', stages: ['Extraction'], basis: 'related', note: 'Fuel production (Energy Institute), share of world.' },
      { label: 'Uranium mine production, share of world', theme: 'production', m: 'production.uranium', item: CELL, techs: ['Nuclear'], chart: 'map', as: 'share', stages: ['Extraction'], basis: 'related', note: 'Uranium mine production, share of world.' },
      EXPORTS_SHARE,
    ],
  },
  {
    id: 'export', label: 'Export position',
    charts: [
      EXPORTS_SHARE,
      { ...ATLAS_SHARE, note: 'Share of world exports in the Harvard Atlas, which the export position uses for economies that do not report their trade.' },
      { label: 'Export specialization (RCA)', theme: 'trade', m: 'trade.rca', item: CELL, chart: 'map', stages: PRODUCING, basis: 'related', note: 'Revealed comparative advantage of the stage’s basket (exporter-reported): specialisation, which the export position read before the structural benchmark.' },
    ],
  },
  {
    id: 'structural', label: 'Structural trade share',
    charts: [
      { ...EXPORTS_SHARE, label: 'Exports, share of world over time', chart: 'over-time', basis: 'related', note: 'Share of world exports in 2020 and 2024 (UN Comtrade, reconciled); the structural benchmark averages the 2022-2024 annual exports to the world.' },
      { ...ATLAS_SHARE, note: 'Share of world exports in the Harvard Atlas, which the benchmark uses for economies that do not report their trade.' },
    ],
  },
  { id: 'invest', label: 'Current investment', charts: [ANNUAL_INVESTMENT] },
  { id: 'operating', label: 'Operating capacity', charts: [MFG_CAPACITY, MFG_SHARE] },
  { id: 'input-cost', label: 'Input cost', charts: [WAGES, LENDING_RATE] },
  { id: 'resource', label: 'Resource base', charts: [MINERAL_RESERVES, FUEL_RESERVES, URANIUM_RESOURCES] },
  {
    id: 'momentum', label: 'Trade momentum', charts: [], monthly: true,
    none: 'It reads monthly trade (UN Comtrade, the latest 12 months against the 12 before), which the Sector data do not carry; the monthly trade card of the Competitive Position section shows it.',
  },
  { id: 'feasibility', label: 'Export feasibility', part: PATHWAY_PART, charts: [FEASIBILITY] },
  { id: 'pipeline', label: 'Committed and announced pipeline', part: PATHWAY_PART, charts: [MFG_CAPACITY, MFG_PIPELINE_SHARE] },
];

/** Deployment need: its three families, the climate-aligned alternative beside them, and the deployment trend. */
const DEPLOYMENT_FAMILIES: readonly FenceFamily[] = [
  {
    id: 'demand', label: 'Demand adequacy',
    charts: [
      { ...GENERATION, note: 'Electricity generation from the technology’s fuel over time (Energy Institute); the family reads the growth of electricity demand and of the technology’s generation (IEA WEO 2025 Current Policies, else the Ember trend).' },
      { ...VRE_GENERATION, item: { kind: 'tech', items: { Batteries: 'solar' } }, note: 'Solar generation over time (Energy Institute; wind in the Fuel select): for Batteries the family reads the growth of the variable renewables storage has to firm.' },
      { label: 'Electric share of new-car sales', theme: 'consumption', m: 'consumption.ev_sales_share', item: DEPLOYMENT, techs: ['Electric Vehicles'], chart: 'map', basis: 'source', note: 'Electric share of new-car sales (IEA Global EV Outlook 2026); the family reads its change per year.' },
      { ...OIL_USE_CHANGE, note: 'Change in oil consumption per person, 2020-2025 (Energy Institute); the family reads the observed growth of total oil consumption.' },
    ],
  },
  { id: 'gap', label: 'Target / climate gap', charts: [{ ...POWER_ADDITIONS, note: 'Power capacity added in the year (Clean Investment Monitor): the build rate side of the gap. The national targets and approved plans the family reads (Ember 2030 target tracker) are not in the Sector data.' }] },
  {
    id: 'system', label: 'System need',
    charts: [
      {
        label: 'Balance of the fuel it displaces',
        theme: 'consumption', m: 'consumption.balance',
        item: { kind: 'tech', items: { Solar: 'fossil', Wind: 'fossil', Nuclear: 'fossil', Geothermal: 'fossil', Coal: 'fossil', Gas: 'fossil', 'Electric Vehicles': 'oil', 'Green Hydrogen': 'gas' } },
        chart: 'map', basis: 'related',
        note: 'Net balance (production minus consumption) of the fuel the technology displaces (Energy Institute; fossil sums oil, gas and coal): a net importer has imported fuel to displace, one of the family’s measures.',
      },
      VRE_GENERATION,
    ],
  },
  { id: 'alt', label: 'Climate-aligned (alternative)', charts: [], none: 'An alternative, world-level climate-aligned scenario shown beside the choice and never used in it; the Sector data have no country measure of it.' },
  { id: 'trend', label: 'Deployment trend', charts: [GENERATION, { ...EV_SALES, chart: 'over-time' }, { ...POWER_ADDITIONS, chart: 'over-time' }, { ...DEPLOYMENT_INVESTMENT, chart: 'over-time' }] },
];

/** Every analysis' families, in its composite's own structure. */
export const VARIABLE_CHARTS: Readonly<Record<VariableAnalysis, readonly FenceFamily[]>> = {
  need: NEED,
  prize: PRIZE,
  position: POSITION,
  deployment: DEPLOYMENT_FAMILIES,
};

/**
 * The families a fence table's picker draws on. A core whose prize is the market measure (no Strategic Industrial
 * Value): the prize is the market prize itself, so its three parts are named after it (`prizeLabel`, Industrial
 * Opportunity) and industrial commons and future option are not part of it.
 */
export function fenceFamilies(analysis: VariableAnalysis, opts: { siv?: boolean; prizeLabel?: string } = {}): FenceFamily[] {
  const fs = VARIABLE_CHARTS[analysis];
  if (analysis !== 'prize' || (opts.siv ?? true)) return [...fs];
  const part = opts.prizeLabel ?? MARKET_PRIZE_LABEL;
  return fs.filter((f) => f.part === MARKET_PART).map((f) => ({ ...f, part }));
}

/** A family's group in the picker: its name, and the part of the composite it feeds ("Investment momentum (market prize)"). */
export const familyGroup = (f: Pick<FenceFamily, 'label' | 'part'>): string => (f.part ? `${f.label} (${f.part})` : f.label);

/** The step a chart is for: its technology and stage (the data vocabulary: "Electric Grid", "Assembly"). */
export interface ChartNode {
  tech: string;
  stage: string;
}

/** Whether an indicator speaks at a step: its stage and technology. */
export function chartApplies(c: SectorChart, node: ChartNode): boolean {
  return (
    (!c.stages || (c.stages as readonly string[]).includes(node.stage)) &&
    (!c.techs || c.techs.includes(node.tech)) &&
    (c.item.kind !== 'tech' || Object.hasOwn(c.item.items, node.tech))
  );
}

/** The indicators of a family that speak for it at this step (its stage and technology), in order of preference. */
export function chartsAt(f: FenceFamily, node: ChartNode): SectorChart[] {
  return f.charts.filter((c) => chartApplies(c, node));
}

/** One option of the picker: one indicator of one family. */
export interface VariableOption {
  /** "<family id>:<index of the indicator in the family>" */
  value: string;
  /** the indicator's label */
  label: string;
  /** the family's group (familyGroup) */
  group: string;
  family: FenceFamily;
  chart: SectorChart;
}

const optionValue = (f: FenceFamily, k: number) => `${f.id}:${k}`;

/** The picker's options at a step: every indicator with a chart there, grouped by family in the composite's order. */
export function variableOptions(families: readonly FenceFamily[], node: ChartNode): VariableOption[] {
  return families.flatMap((f) =>
    f.charts.flatMap((c, k) => (chartApplies(c, node) ? [{ value: optionValue(f, k), label: c.label, group: familyGroup(f), family: f, chart: c }] : [])),
  );
}

/** The indicator a picker value names, whatever the step (null for none or an unknown value). */
export function findVariable(families: readonly FenceFamily[], value: string): { family: FenceFamily; chart: SectorChart } | null {
  for (const f of families) {
    const k = f.charts.findIndex((_, i) => optionValue(f, i) === value);
    if (k >= 0) return { family: f, chart: f.charts[k] };
  }
  return null;
}

/** A family the picker cannot offer at a step: it has no Sector measure at all, or none at this step. */
export interface Chartless {
  family: FenceFamily;
  reason: 'none' | 'step';
}

/** The families with no indicator at this step, in the composite's order. */
export function chartlessAt(families: readonly FenceFamily[], node: ChartNode): Chartless[] {
  return families.flatMap((f): Chartless[] => (!f.charts.length ? [{ family: f, reason: 'none' }] : chartsAt(f, node).length ? [] : [{ family: f, reason: 'step' }]));
}

const listWords = (xs: readonly string[]): string => (xs.length < 2 ? (xs[0] ?? '') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

/**
 * The one line under the picker naming the families it cannot offer and where to see them instead: the row's
 * details (the drawer shows every family's value), and for trade momentum the monthly trade card. Null when
 * every family has a chart at the step.
 */
export function chartlessLine(list: readonly Chartless[], opts: { selected: boolean }): string | null {
  if (!list.length) return null;
  const names = listWords(list.map((c) => (c.reason === 'step' ? `${c.family.label} (none at this step)` : c.family.label)));
  const them = list.length > 1 ? 'them' : 'it';
  const where = opts.selected ? `the selected row’s details show ${them}` : `select a row to see ${them} in its details`;
  const monthly = list.filter((c) => c.family.monthly).map((c) => c.family.label.toLowerCase());
  return `No Sector chart for ${names}: ${where}${monthly.length ? `; for ${listWords(monthly)}, see also the monthly trade card in the Position section` : ''}.`;
}

/**
 * The Sector state a chart opens on for a step and economy, as far as it is known without the Sector model:
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
