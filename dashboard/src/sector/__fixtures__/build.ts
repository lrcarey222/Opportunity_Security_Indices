// TS fixture builder (spec_sector.md 12.2 C1): a small deterministic Sector bundle built in memory, as
// decoded OSB objects (typed-array columns) that pass validateChunk(), plus a manifest that passes
// isSectorManifest(). It mirrors the synthetic bundle of 8.3 (scripts/87 --synthetic): CHN, USA, DEU
// and a non-reporter among a dozen economies; a trade cell and a thin cell; two minerals (lithium traded
// at two stages: mineral_stage items); a region and IEA entities; every status code; two trade years; the
// series chunk. Six stages since 2026-10-28 (schema 2): the 66 headline cells plus Semiconductors Refining,
// Components and Assembly; the manufacturing figures the sources give once (investment, capacity shares)
// on both Batteries Components and Assembly with the facts.notes id same_figure_pair. Used by the unit and UI tests
// until builder A's committed fixture (tests/fixtures/sector/synthetic/) is wired in, and kept for
// hand-checked cases afterwards. Values are invented; they only need to be internally consistent.

import {
  cellSlug, FACTS_SPEC, HEADLINE_TECH_SLUGS, META_SPEC, POLICY_SPEC, POLICY_TEXT_SPEC, SERIES_SPEC, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, TRADE_SPEC,
  type SectorChunkName, type SectorManifest, type StageSlug, type TechSlug, type TradeTechSlug,
} from '../contract.ts';
import { NA_BOOL, NA_INT } from '../../engine/numeric.ts';
import type { SectorSource } from '../data/store.ts';
import { SectorLoadError } from '../data/store.ts';
import { SectorModel } from '../data/model.ts';
import { FactsIndex } from '../data/facts.ts';
import { summaryFromFacts } from '../derive/summary.ts';
import type { SectorFactsChunk, SectorMetaChunk, SectorTradeChunk } from '../contract.ts';

type Spec = { kind: 'table'; cols: Record<string, { t: 'f64' | 'i32' | 'bool' | 'str' }> } | { kind: 'strv' };
type Val = string | number | boolean | null | undefined;
type RowObj = Record<string, Val>;

/** A decoded OSB table from row objects, typed by the contract's column spec. */
export function table(spec: Spec, rows: RowObj[]): { nrow: number; cols: Record<string, Float64Array | Int32Array | Uint8Array | (string | null)[]> } {
  if (spec.kind !== 'table') throw new Error('not a table spec');
  const cols: Record<string, Float64Array | Int32Array | Uint8Array | (string | null)[]> = {};
  for (const [name, cs] of Object.entries(spec.cols)) {
    const n = rows.length;
    if (cs.t === 'f64') {
      const a = new Float64Array(n);
      rows.forEach((r, i) => (a[i] = r[name] === null || r[name] === undefined ? NaN : Number(r[name])));
      cols[name] = a;
    } else if (cs.t === 'i32') {
      const a = new Int32Array(n);
      rows.forEach((r, i) => (a[i] = r[name] === null || r[name] === undefined ? NA_INT : Number(r[name])));
      cols[name] = a;
    } else if (cs.t === 'bool') {
      const a = new Uint8Array(n);
      rows.forEach((r, i) => (a[i] = r[name] === null || r[name] === undefined ? NA_BOOL : r[name] ? 1 : 0));
      cols[name] = a;
    } else {
      cols[name] = rows.map((r) => (r[name] === null || r[name] === undefined ? null : String(r[name])));
    }
  }
  return { nrow: rows.length, cols };
}

// ---- dims ----------------------------------------------------------------------------------------

interface G {
  code: string;
  name: string;
  kind: 'country' | 'eu' | 'world' | 'region' | 'entity' | 'residual';
  tier: 'core' | 'partner' | 'none';
  group: 'china' | 'core' | 'partner' | 'rest' | 'na';
  eu?: boolean;
  hub?: boolean;
  n3?: number;
  reason?: string;
}

const GEOS: G[] = [
  { code: 'WLD', name: 'World', kind: 'world', tier: 'none', group: 'na' },
  { code: 'EUU', name: 'European Union (27)', kind: 'eu', tier: 'core', group: 'core' },
  { code: 'CHN', name: 'China', kind: 'country', tier: 'none', group: 'china', n3: 156 },
  { code: 'USA', name: 'United States', kind: 'country', tier: 'core', group: 'core', n3: 840, reason: 'Core member: G7.' },
  { code: 'DEU', name: 'Germany', kind: 'country', tier: 'core', group: 'core', eu: true, n3: 276, reason: 'Core member: EU.' },
  { code: 'FRA', name: 'France', kind: 'country', tier: 'core', group: 'core', eu: true, n3: 250, reason: 'Core member: EU.' },
  { code: 'JPN', name: 'Japan', kind: 'country', tier: 'core', group: 'core', n3: 392, reason: 'Core member: G7.' },
  { code: 'KOR', name: 'Korea, Rep.', kind: 'country', tier: 'core', group: 'core', n3: 410, reason: 'Core member.' },
  { code: 'BRA', name: 'Brazil', kind: 'country', tier: 'partner', group: 'partner', n3: 76, reason: 'Partner: reserves >= 10%.' },
  { code: 'IDN', name: 'Indonesia', kind: 'country', tier: 'partner', group: 'partner', n3: 360, reason: 'Partner.' },
  { code: 'VNM', name: 'Viet Nam', kind: 'country', tier: 'none', group: 'rest', n3: 704 },
  { code: 'HKG', name: 'Hong Kong SAR, China', kind: 'country', tier: 'none', group: 'rest', hub: true, n3: 344 },
  { code: 'RUS', name: 'Russian Federation', kind: 'country', tier: 'none', group: 'rest', n3: 643 },
  { code: 'CYP', name: 'Cyprus', kind: 'country', tier: 'none', group: 'rest', eu: true, n3: 196 },
  { code: 'R:iea:Europe', name: 'Europe (IEA region)', kind: 'region', tier: 'core', group: 'core' },
  { code: 'R:iea:China', name: 'China (IEA region)', kind: 'region', tier: 'none', group: 'china' },
  { code: 'E:iea:China', name: 'China', kind: 'entity', tier: 'none', group: 'china' },
  { code: 'E:iea:EU', name: 'European Union', kind: 'entity', tier: 'core', group: 'core' },
  { code: 'E:iea:USA', name: 'United States', kind: 'entity', tier: 'core', group: 'core' },
  { code: 'E:iea:IND', name: 'India', kind: 'entity', tier: 'partner', group: 'partner' },
  { code: 'RES:iea', name: 'Rest of world (IEA)', kind: 'residual', tier: 'none', group: 'na' },
  { code: 'RES:ei', name: 'Other (EI)', kind: 'residual', tier: 'none', group: 'na' },
];
const GI = new Map(GEOS.map((g, i) => [g.code, i]));
const gi = (code: string): number => {
  const i = GI.get(code);
  if (i === undefined) throw new Error(`fixture geo ${code}`);
  return i;
};
const COUNTRIES = GEOS.filter((g) => g.kind === 'country').map((g) => g.code);

interface It {
  axis: 'cell' | 'mineral' | 'mineral_stage' | 'fuel' | 'commodity' | 'activity' | 'policy_tag' | 'none';
  slug: string;
  label: string;
  tech?: TechSlug;
  stage?: StageSlug;
  has_trade?: boolean;
}

// solar-refining and semiconductors-refining: the materials baskets of the 2026-10 review (C5.1)
const TRADE_CELLS = new Set(['batteries-assembly', 'batteries-extraction', 'oil-extraction', 'semiconductors-components', 'solar-refining', 'semiconductors-refining']);
const ITEMS: It[] = [];
for (const t of TECH_SLUGS) {
  for (const s of STAGE_SLUGS) {
    // Semiconductors (non-headline): only the stages its basket has
    if (t === 'semiconductors' && !['refining', 'components', 'assembly'].includes(s)) continue;
    const slug = cellSlug(t, s);
    ITEMS.push({ axis: 'cell', slug, label: `${TECH_LABELS[t]} ${STAGE_LABELS[s]}`, tech: t, stage: s, has_trade: TRADE_CELLS.has(slug) });
  }
}
ITEMS.push(
  { axis: 'mineral', slug: 'lithium', label: 'Lithium' },
  { axis: 'mineral', slug: 'graphite', label: 'Graphite (battery grade)' },
  // the critical-minerals trade by stage (schema 2): ores at Extraction, refined products at Refining
  { axis: 'mineral_stage', slug: 'lithium-extraction', label: 'Lithium — Extraction', stage: 'extraction' },
  { axis: 'mineral_stage', slug: 'lithium-refining', label: 'Lithium — Refining', stage: 'refining' },
  { axis: 'fuel', slug: 'oil', label: 'Oil' },
  { axis: 'fuel', slug: 'gas', label: 'Gas' },
  { axis: 'commodity', slug: 'crude-oil', label: 'Crude oil (Brent)' },
  { axis: 'commodity', slug: 'cobalt', label: 'Cobalt' },
  { axis: 'activity', slug: 'manufacturing', label: 'Manufacturing' },
  // IEA PAMS tags as items of policy.records (A's convention: one avail row per tag)
  { axis: 'policy_tag', slug: 'batteries', label: 'Batteries' },
  { axis: 'policy_tag', slug: 'solar', label: 'Solar' },
  { axis: 'policy_tag', slug: 'oil', label: 'Oil' },
  { axis: 'policy_tag', slug: 'critical-minerals', label: 'Critical Minerals' },
  { axis: 'policy_tag', slug: 'cross-cutting', label: 'Cross-cutting (economy-wide)' },
  { axis: 'none', slug: 'none', label: 'Economy-wide' },
);
const II = new Map(ITEMS.map((x, i) => [`${x.axis}:${x.slug}`, i]));
const ii = (axis: It['axis'], slug: string): number => {
  const i = II.get(`${axis}:${slug}`);
  if (i === undefined) throw new Error(`fixture item ${axis}:${slug}`);
  return i;
};
const cell = (slug: string) => ii('cell', slug);

// ---- dictionary --------------------------------------------------------------------------------

const UNITS = [
  { id: 'usd', short: 'USD', long: 'US dollars, current', unit_kind: 'usd' },
  { id: 'usd_bn_2024', short: 'USD bn (2024$)', long: 'US dollars, billions, 2024 prices', unit_kind: 'usd_bn_real' },
  { id: 'share', short: 'share', long: 'share', unit_kind: 'share' },
  { id: 'ratio', short: 'ratio', long: 'ratio (1 = world average)', unit_kind: 'ratio' },
  { id: 'kt', short: 'kt', long: 'thousand tonnes', unit_kind: 'kt' },
  { id: 'kbd', short: 'kb/d', long: 'thousand barrels a day', unit_kind: 'kbd' },
  { id: 'ej', short: 'EJ', long: 'exajoules', unit_kind: 'ej' },
  { id: 'gj_pc', short: 'GJ per person', long: 'gigajoules per person', unit_kind: 'gj_pc' },
  { id: 'pct', short: '%', long: 'percent', unit_kind: 'pct' },
  { id: 'mveh', short: 'm vehicles', long: 'million vehicles', unit_kind: 'mvehicles' },
  { id: 'veh', short: 'vehicles', long: 'vehicles', unit_kind: 'vehicles' },
  { id: 'usd_bbl', short: 'USD/bbl', long: 'US dollars a barrel', unit_kind: 'price' },
  { id: 'usd_t', short: 'USD/t', long: 'US dollars a tonne', unit_kind: 'price' },
  { id: 'vol', short: 'a year', long: 'annualised volatility', unit_kind: 'share' },
  { id: 'usd_month', short: 'USD a month', long: 'US dollars a month', unit_kind: 'usd_month' },
  { id: 'trl', short: 'TRL', long: 'technology readiness level', unit_kind: 'trl' },
  { id: 'count', short: 'records', long: 'records', unit_kind: 'count' },
  { id: 'gw', short: 'GW', long: 'gigawatts', unit_kind: 'gw' },
  { id: 'gwh', short: 'GWh', long: 'gigawatt-hours', unit_kind: 'gwh' },
  { id: 'twh', short: 'TWh', long: 'terawatt-hours', unit_kind: 'twh' },
  { id: 'index01', short: '0-1', long: 'position, 0 = China', unit_kind: 'ratio' },
  { id: 'feas01', short: '0-1', long: 'index from 0 (none) to 1 (all)', unit_kind: 'index01' },
];

const SOURCES = [
  { id: 'comtrade_matrix', short: 'UN Comtrade, reconciled matrix', citation: 'UN Comtrade (2026), bilateral HS6 trade, reconciled.', publisher: 'United Nations', edition: '2026 pull', url: 'https://comtradeplus.un.org/', licence: 'attribution', attribution: 'Source: UN Comtrade.', accessed: '2026-09-01', transform: 'Importer-declared CIF; exporter reports x1.06 where the importer did not declare.', issues: null },
  { id: 'atlas', short: 'Harvard Atlas', citation: 'Growth Lab, Atlas of Economic Complexity.', publisher: 'Harvard Growth Lab', edition: 'HS92', url: 'https://atlas.hks.harvard.edu/', licence: 'attribution', attribution: 'Source: Atlas.', accessed: '2026-09-01', transform: null, issues: null },
  { id: 'comtrade_reported', short: 'UN Comtrade, exporter-reported', citation: 'UN Comtrade (2026), exporter-reported World rows.', publisher: 'United Nations', edition: '2026', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'usgs', short: 'USGS MCS', citation: 'USGS Mineral Commodity Summaries 2026.', publisher: 'USGS', edition: '2026', url: 'https://www.usgs.gov/', licence: 'open', attribution: null, accessed: '2026-02-01', transform: 'Brazil REE erratum applied.', issues: null },
  { id: 'iea_cm', short: 'IEA Critical Minerals', citation: 'IEA Critical Minerals Dataset 2026.', publisher: 'IEA', edition: '2026', url: 'https://www.iea.org/', licence: 'attribution', attribution: 'IEA (2026), all rights reserved.', accessed: '2026-05-01', transform: null, issues: null },
  { id: 'ei', short: 'EI Statistical Review', citation: 'Energy Institute Statistical Review of World Energy 2026.', publisher: 'Energy Institute', edition: '2026', url: 'https://www.energyinst.org/', licence: 'attribution', attribution: null, accessed: '2026-06-30', transform: null, issues: null },
  { id: 'cim', short: 'Clean Investment Monitor', citation: 'Rhodium Group / MIT CEEPR, Clean Investment Monitor.', publisher: 'Rhodium', edition: 'Q4 2025', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'iea_etp', short: 'IEA ETP', citation: 'IEA Energy Technology Perspectives 2024.', publisher: 'IEA', edition: '2024', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'iea_ev', short: 'IEA Global EV Outlook', citation: 'IEA GEVO 2026.', publisher: 'IEA', edition: '2026', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'imf', short: 'IMF Primary Commodity Prices', citation: 'IMF PCPS.', publisher: 'IMF', edition: 'Dec 2025', url: null, licence: 'open', attribution: null, accessed: null, transform: 'IMF unit labels corrected: cobalt USD/t.', issues: null },
  { id: 'ilo', short: 'ILOSTAT', citation: 'ILO.', publisher: 'ILO', edition: null, url: null, licence: 'open', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'imf_rates', short: 'IMF IFS', citation: 'IMF IFS.', publisher: 'IMF', edition: null, url: null, licence: 'open', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'iea_trl', short: 'IEA ETP Guide', citation: 'IEA Clean Technology Guide.', publisher: 'IEA', edition: '2025', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'pams', short: 'IEA Policies and Measures', citation: 'IEA Policies and Measures database.', publisher: 'IEA', edition: '2026', url: 'https://www.iea.org/policies', licence: 'attribution', attribution: null, accessed: '2026-08-01', transform: null, issues: null },
  { id: 'cat', short: 'Climate Action Tracker', citation: 'CAT country ratings.', publisher: 'CAT', edition: '2025', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'unga', short: 'UN GA ideal points', citation: 'Bailey, Strezhnev and Voeten.', publisher: null, edition: '2023', url: null, licence: 'open', attribution: null, accessed: null, transform: null, issues: null },
  { id: 'redbook', short: 'Red Book', citation: 'OECD NEA / IAEA Uranium 2024.', publisher: 'NEA', edition: '2024', url: null, licence: 'attribution', attribution: null, accessed: null, transform: null, issues: null },
];

const CAVEATS = [
  { id: 'hs6_overlap', text: 'Exports are technology-attributed: some HS6 codes also count in other sectors.' },
  { id: 'nominal_usd', text: 'Nominal US dollars; 2020 was a COVID trough.' },
  { id: 'mirror_factor', text: "Where an importer did not declare, the exporter's report x1.06 is used." },
  { id: 'hkg_hub', text: 'Hong Kong is kept with the re-export hub tag.' },
  { id: 'iea_top6', text: 'IEA lists the top 6 producers; the rest is one Rest of world band, so the HHI is a lower bound.' },
  { id: 'est_q4', text: '2025 includes estimated Q4 construction starts.' },
  { id: 'exporter_reported', text: 'Exporter-reported basis (142 reporters, Hong Kong excluded); not comparable with the matrix.' },
  { id: 'unit_to_confirm', text: 'Unit to confirm.' },
  { id: 'production_assumed_zero', text: "Where EI does not report an economy's production of a fuel separately and no EIA value is used, production is assumed to be 0: the net balance is then minus consumption and self-sufficiency 0%." },
  { id: 'eia_production', text: "Where EI does not report an economy's production of a fuel separately, production comes from the EIA International Energy Statistics." },
  { id: 'feasibility_reading', text: "Export feasibility is the Atlas of Economic Complexity's relatedness (also called density). 0 means none of the related capabilities are in place, 1 means all of them." },
  { id: 'atlas_source', text: 'Harvard Atlas 2023: a different source and year from the Comtrade 2024 matrix, so it is shown in its own card.' },
  { id: 'sub_component_rule', text: "Sub-components are the groups of HS6 codes the sector's basket is built from; a sector's sub-components add up to its total." },
  { id: 'hhi_reading', text: "Concentration (HHI): take each holder's share of the world total, square it and add up the squares." },
  { id: 'hhi_refs_context', text: 'Competition authorities screen mergers with HHI thresholds; they were written for markets of competing firms, not for trade between whole economies, so they are a yardstick, not a verdict.' },
  { id: 'basket_solar_refining', text: 'Solar Refining trade is silicon metal only (HS 2804.69), the feedstock of polysilicon.' },
  { id: 'basket_semis_refining', text: 'Semiconductors Refining trade is unwrought gallium, germanium and indium (HS 8112.92).' },
  { id: 'same_figure_pair', text: "Same figure on Components and Assembly: the source gives one figure for the technology's manufacturing, not one per stage, so the value is shown on both sectors. Never add the two; totals count it once." },
];

interface VarDef {
  id: string;
  family: string;
  label: string;
  short: string;
  unit: string;
  data_year: string;
  years: number[];
  level: string;
  axis: It['axis'];
  agg: 'sum' | 'ratio' | 'none';
  basis: 'source_world' | 'sum_all' | 'one' | 'sum_listed' | 'none';
  source: string;
  roster?: string;
  caveats?: string[];
  primary_for?: string[];
  section?: string;
  year_field?: boolean;
  stale_years?: number;
  projection_from?: number;
  series_window?: string;
  scale?: 'log' | 'linear';
  prov?: string;
  feeds?: string[];
  definition?: string;
}

const VARS: VarDef[] = [
  { id: 'trade.exports', family: 'exports', label: 'Exports of the sector’s traded goods', short: 'Exports', unit: 'usd', data_year: '2024', years: [2020, 2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', roster: 'comtrade_matrix', caveats: ['hs6_overlap', 'nominal_usd', 'hkg_hub', 'mirror_factor'], primary_for: ['batteries-assembly', 'batteries-extraction', 'semiconductors-components', 'solar-refining', 'semiconductors-refining'], section: 'trade', scale: 'log', prov: 'X', feeds: ['ES:Trade', 'EO:Trade'], definition: 'Value of the economy’s exports of the basket.' },
  { id: 'trade.imports', family: 'exports', label: 'Imports of the sector’s traded goods', short: 'Imports', unit: 'usd', data_year: '2024', years: [2020, 2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', roster: 'comtrade_matrix', caveats: ['hs6_overlap', 'nominal_usd'], section: 'trade', scale: 'log', prov: 'X' },
  { id: 'trade.net', family: 'exports', label: 'Net trade (exports − imports)', short: 'Net trade', unit: 'usd', data_year: '2024', years: [2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'none', source: 'comtrade_matrix', roster: 'comtrade_matrix', section: 'trade', prov: 'D' },
  { id: 'trade.flow', family: 'exports', label: 'Bilateral flow', short: 'Flow', unit: 'usd', data_year: '2020, 2024', years: [2020, 2024], level: 'dyad', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', section: 'trade', prov: 'X' },
  { id: 'trade.rca', family: 'exports', label: 'Revealed comparative advantage', short: 'RCA', unit: 'ratio', data_year: '2024', years: [2024], level: 'country', axis: 'cell', agg: 'none', basis: 'none', source: 'comtrade_reported', caveats: ['exporter_reported'], section: 'trade', prov: 'P' },
  { id: 'trade.sub_exports', family: 'exports', label: 'Exports by sub-component', short: 'Exports (sub-component)', unit: 'usd', data_year: '2024', years: [2020, 2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', roster: 'comtrade_matrix', caveats: ['sub_component_rule', 'nominal_usd'], section: 'trade', scale: 'log', prov: 'X' },
  { id: 'trade.sub_imports', family: 'exports', label: 'Imports by sub-component', short: 'Imports (sub-component)', unit: 'usd', data_year: '2024', years: [2020, 2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', roster: 'comtrade_matrix', caveats: ['sub_component_rule', 'nominal_usd'], section: 'trade', scale: 'log', prov: 'X' },
  { id: 'trade.feasibility', family: 'exports', label: 'Export feasibility (related capabilities)', short: 'Export feasibility', unit: 'feas01', data_year: '2023', years: [2023], level: 'country', axis: 'cell', agg: 'none', basis: 'none', source: 'atlas', roster: 'atlas', caveats: ['feasibility_reading', 'atlas_source'], section: 'trade', prov: 'P', feeds: ['EO: Trade'], definition: 'How close the sector’s products are to what the economy already exports well: 1 minus the average Harvard Atlas product-space distance over the basket’s HS4 headings. Higher means the needed capabilities are nearer at hand.' },
  { id: 'trade.atlas_share', family: 'exports', label: 'Share of world exports (Atlas)', short: 'Atlas share', unit: 'share', data_year: '2023', years: [2023], level: 'country', axis: 'cell', agg: 'sum', basis: 'one', source: 'atlas', section: 'trade', prov: 'P' },
  { id: 'trade.critmin_exports', family: 'exports', label: 'Mineral exports', short: 'Mineral exports', unit: 'usd', data_year: '2024', years: [2024], level: 'country', axis: 'mineral_stage', agg: 'sum', basis: 'sum_all', source: 'comtrade_matrix', roster: 'comtrade_matrix', prov: 'P' },
  { id: 'resources.mineral_reserves', family: 'reserves', label: 'Reserves', short: 'Reserves', unit: 'kt', data_year: 'end-2025', years: [2025], level: 'country', axis: 'mineral', agg: 'sum', basis: 'source_world', source: 'usgs', roster: 'usgs', section: 'resources', prov: 'P' },
  { id: 'resources.fuel_reserves', family: 'reserves', label: 'Proved reserves', short: 'Reserves', unit: 'kt', data_year: 'end-2020 · not updated', years: [2020], level: 'country', axis: 'fuel', agg: 'sum', basis: 'source_world', source: 'ei', roster: 'ei', section: 'resources', prov: 'P' },
  { id: 'production.fuels', family: 'production', label: 'Fuel production', short: 'Production', unit: 'ej', data_year: '2025', years: [2025], level: 'country', axis: 'fuel', agg: 'sum', basis: 'source_world', source: 'ei', roster: 'ei', primary_for: ['oil-extraction', 'gas-extraction'], section: 'production', series_window: '2015-2025', prov: 'P' },
  { id: 'production.mining', family: 'production', label: 'Mine production, base case', short: 'Mining', unit: 'kt', data_year: '2025 · 2030-2040 projection', years: [2025, 2030, 2035, 2040], level: 'country', axis: 'mineral', agg: 'sum', basis: 'source_world', source: 'iea_cm', roster: 'iea_cm', caveats: ['iea_top6'], projection_from: 2030, prov: 'X' },
  { id: 'production.refining', family: 'production', label: 'Refined output, base case', short: 'Refining', unit: 'kt', data_year: '2025 · 2030-2040 projection', years: [2025, 2030, 2035, 2040], level: 'country', axis: 'mineral', agg: 'sum', basis: 'source_world', source: 'iea_cm', roster: 'iea_cm', caveats: ['iea_top6'], projection_from: 2030, prov: 'X' },
  { id: 'production.mine_usgs', family: 'production', label: 'Mine production (USGS)', short: 'Mine production', unit: 'kt', data_year: '2025e', years: [2025], level: 'country', axis: 'mineral', agg: 'sum', basis: 'source_world', source: 'usgs', roster: 'usgs', prov: 'X' },
  { id: 'production.ev_region', family: 'production', label: 'EV production', short: 'EV production', unit: 'mveh', data_year: '2024', years: [2024], level: 'region', axis: 'cell', agg: 'sum', basis: 'sum_listed', source: 'iea_ev', series_window: '2021-2024', prov: 'P' },
  { id: 'manufacturing.share_now', family: 'capacity', label: 'Share of global manufacturing capacity', short: 'Capacity share', unit: 'share', data_year: 'current (2024)', years: [2024], level: 'entity', axis: 'cell', agg: 'sum', basis: 'one', source: 'iea_etp', prov: 'P' },
  { id: 'manufacturing.share_2035', family: 'capacity', label: 'Share of announced 2035 capacity', short: '2035 share', unit: 'share', data_year: '2035 · projection', years: [2035], level: 'entity', axis: 'cell', agg: 'sum', basis: 'one', source: 'iea_etp', projection_from: 2035, prov: 'P' },
  { id: 'manufacturing.capacity', family: 'capacity', label: 'Manufacturing capacity by product', short: 'Capacity', unit: 'gwh', data_year: '2025', years: [2025], level: 'country', axis: 'cell', agg: 'none', basis: 'none', source: 'cim', prov: 'X' },
  { id: 'investment.annual', family: 'investment', label: 'Annual investment', short: 'Investment', unit: 'usd_bn_2024', data_year: '2018-2025', years: [2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'cim', roster: 'cim', caveats: ['est_q4', 'same_figure_pair'], primary_for: ['batteries-deployment'], section: 'investment', prov: 'P' },
  { id: 'investment.power_additions', family: 'investment', label: 'Power capacity additions', short: 'Additions', unit: 'gw', data_year: '2018-2025', years: [2025], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_all', source: 'cim', series_window: '2018-2025', prov: 'X' },
  { id: 'production.generation', family: 'production', label: 'Electricity generation', short: 'Generation', unit: 'twh', data_year: '2025', years: [2025], level: 'country', axis: 'cell', agg: 'sum', basis: 'source_world', source: 'ei', roster: 'ei', primary_for: ['solar-deployment'], prov: 'P' },
  { id: 'consumption.per_capita', family: 'consumption', label: 'Consumption per person', short: 'Per person', unit: 'gj_pc', data_year: '2025', years: [2025], level: 'country', axis: 'fuel', agg: 'ratio', basis: 'none', source: 'ei', roster: 'ei', prov: 'P' },
  { id: 'consumption.pc_change', family: 'consumption', label: 'Change in consumption per person', short: 'Change', unit: 'pct', data_year: '2020→2025', years: [2025], level: 'country', axis: 'fuel', agg: 'none', basis: 'none', source: 'ei', roster: 'ei', prov: 'P' },
  { id: 'consumption.balance', family: 'consumption', label: 'Net balance (production − consumption)', short: 'Net balance', unit: 'ej', data_year: '2025', years: [2025], level: 'country', axis: 'fuel', agg: 'sum', basis: 'none', source: 'ei', roster: 'ei', prov: 'P', caveats: ['production_assumed_zero'] },
  { id: 'consumption.self_sufficiency', family: 'consumption', label: 'Self-sufficiency (production as % of consumption)', short: 'Self-sufficiency', unit: 'pct', data_year: '2025', years: [2025], level: 'country', axis: 'fuel', agg: 'ratio', basis: 'none', source: 'ei', roster: 'ei', prov: 'P', scale: 'log', caveats: ['production_assumed_zero'] },
  { id: 'consumption.ev_sales', family: 'consumption', label: 'EV sales', short: 'EV sales', unit: 'veh', data_year: '2025', years: [2025], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_listed', source: 'iea_ev', roster: 'iea_ev', primary_for: ['electric-vehicles-deployment'], prov: 'P' },
  { id: 'consumption.total_ej', family: 'consumption', label: 'Primary energy consumption', short: 'Consumption', unit: 'ej', data_year: '1965-2025', years: [2025], level: 'country', axis: 'fuel', agg: 'sum', basis: 'source_world', source: 'ei', series_window: '2015-2025', prov: 'X' },
  { id: 'prices.latest', family: 'prices', label: 'Commodity price', short: 'Price', unit: 'usd_bbl', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.yoy', family: 'prices', label: 'Price change, year on year', short: 'YoY', unit: 'pct', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.vol_1y', family: 'prices', label: 'Volatility, 1 year', short: '1y vol', unit: 'vol', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.vol_5y', family: 'prices', label: 'Volatility, 5 years', short: '5y vol', unit: 'vol', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.vol_10y', family: 'prices', label: 'Volatility, 10 years', short: '10y vol', unit: 'vol', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.vol_20y', family: 'prices', label: 'Volatility, 20 years', short: '20y vol', unit: 'vol', data_year: 'Dec 2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', prov: 'P' },
  { id: 'prices.monthly', family: 'prices', label: 'Monthly price', short: 'Monthly', unit: 'usd_bbl', data_year: '2023-2025', years: [2025], level: 'world', axis: 'commodity', agg: 'none', basis: 'none', source: 'imf', series_window: '2023-2025', prov: 'X' },
  { id: 'prices.wages', family: 'context', label: 'Average monthly earnings', short: 'Earnings', unit: 'usd_month', data_year: 'latest per country', years: [2025], level: 'country', axis: 'activity', agg: 'none', basis: 'none', source: 'ilo', year_field: true, stale_years: 5, prov: 'P' },
  { id: 'prices.lending_rate', family: 'context', label: 'Lending rate', short: 'Lending rate', unit: 'pct', data_year: 'latest per country', years: [2025], level: 'country', axis: 'none', agg: 'none', basis: 'none', source: 'imf_rates', year_field: true, prov: 'P' },
  { id: 'prices.ppi_yoy', family: 'context', label: 'Producer-price inflation', short: 'PPI', unit: 'pct', data_year: 'latest per country', years: [2025], level: 'country', axis: 'none', agg: 'none', basis: 'none', source: 'imf_rates', year_field: true, prov: 'P' },
  { id: 'technology.trl', family: 'technology', label: 'Technology readiness', short: 'TRL', unit: 'trl', data_year: '2025', years: [2025], level: 'cell_global', axis: 'cell', agg: 'none', basis: 'none', source: 'iea_trl', prov: 'P' },
  { id: 'technology.trl_change', family: 'technology', label: 'Change in readiness 2020-25', short: 'ΔTRL', unit: 'trl', data_year: '2020-2025', years: [2025], level: 'cell_global', axis: 'cell', agg: 'none', basis: 'none', source: 'iea_trl', prov: 'P' },
  { id: 'policy.records', family: 'policy', label: 'Recorded policies', short: 'Policies', unit: 'count', data_year: '1990-2030', years: [2025], level: 'country', axis: 'policy_tag', agg: 'sum', basis: 'none', source: 'pams', projection_from: 2026, prov: 'X' },
  { id: 'consumption.mineral_demand', family: 'consumption', label: 'Clean-energy demand for minerals', short: 'Mineral demand', unit: 'kt', data_year: '2025 · 2040 STEPS', years: [2025, 2040], level: 'world', axis: 'mineral', agg: 'none', basis: 'none', source: 'iea_cm', projection_from: 2030, prov: 'X' },
  { id: 'policy.cat', family: 'policy', label: 'Climate Action Tracker rating', short: 'CAT rating', unit: 'count', data_year: '2025', years: [2025], level: 'country', axis: 'none', agg: 'none', basis: 'none', source: 'cat', prov: 'X' },
  { id: 'development.alignment', family: 'development', label: 'Position on the US-China UN-vote axis', short: 'UN alignment', unit: 'index01', data_year: '2019-2023', years: [2023], level: 'country', axis: 'none', agg: 'none', basis: 'none', source: 'unga', prov: 'P' },
  { id: 'production.uranium', family: 'production', label: 'Uranium mine production', short: 'Uranium', unit: 'kt', data_year: '2024', years: [2024], level: 'country', axis: 'cell', agg: 'sum', basis: 'sum_listed', source: 'redbook', primary_for: ['nuclear-extraction'], prov: 'P' },
];
const VI = new Map(VARS.map((v, i) => [v.id, i]));
const vi = (id: string): number => {
  const i = VI.get(id);
  if (i === undefined) throw new Error(`fixture var ${id}`);
  return i;
};

// ---- facts -----------------------------------------------------------------------------------------

type Status = 'obs' | 'zero' | 'nsr' | 'nq' | 'mirror' | 'est' | 'na_growth';
interface Fact {
  v: string;
  item: number;
  geo: string;
  year: number;
  value: number;
  status?: Status;
  obs_year?: number;
  obs_from?: number;
}

/** Trade flows of the fixture: [item, exporter, importer, v2020, v2024, s2020, s2024]. */
type FlowDef = [string, string, string, number | null, number | null, 'importer' | 'mirror' | 'none', 'importer' | 'mirror' | 'none'];
const B = 1e9;
const FLOWS: FlowDef[] = [
  // batteries-assembly
  ['batteries-assembly', 'CHN', 'USA', 10 * B, 21.4 * B, 'importer', 'importer'],
  ['batteries-assembly', 'CHN', 'DEU', 8 * B, 18 * B, 'importer', 'importer'],
  ['batteries-assembly', 'CHN', 'JPN', 3 * B, 6 * B, 'importer', 'importer'],
  ['batteries-assembly', 'CHN', 'VNM', 2 * B, 5 * B, 'mirror', 'mirror'],
  ['batteries-assembly', 'CHN', 'FRA', 1 * B, 3 * B, 'importer', 'importer'],
  ['batteries-assembly', 'KOR', 'USA', 4 * B, 6 * B, 'importer', 'importer'],
  ['batteries-assembly', 'KOR', 'DEU', 2 * B, 3 * B, 'importer', 'importer'],
  ['batteries-assembly', 'JPN', 'USA', 3 * B, 4 * B, 'importer', 'importer'],
  ['batteries-assembly', 'USA', 'JPN', 1 * B, 2 * B, 'importer', 'importer'],
  ['batteries-assembly', 'USA', 'KOR', 0.5 * B, 1.5 * B, 'importer', 'importer'],
  ['batteries-assembly', 'DEU', 'FRA', 1.5 * B, 2.5 * B, 'importer', 'importer'],
  ['batteries-assembly', 'FRA', 'DEU', 0.8 * B, 1.2 * B, 'importer', 'importer'],
  ['batteries-assembly', 'DEU', 'USA', 1 * B, 1.4 * B, 'importer', 'importer'],
  ['batteries-assembly', 'IDN', 'CHN', 0.2 * B, 2 * B, 'importer', 'importer'],
  ['batteries-assembly', 'BRA', 'USA', null, 0.3 * B, 'none', 'importer'],
  ['batteries-assembly', 'HKG', 'VNM', 0.1 * B, 0.2 * B, 'mirror', 'mirror'],
  ['batteries-assembly', 'VNM', 'USA', 0.5 * B, 1.1 * B, 'importer', 'importer'],
  ['batteries-assembly', 'CYP', 'DEU', 0.01 * B, 0.02 * B, 'importer', 'importer'],
  ['batteries-assembly', 'RUS', 'CHN', 0.05 * B, 0.04 * B, 'importer', 'importer'],
  // batteries-extraction
  ['batteries-extraction', 'BRA', 'CHN', 1 * B, 2 * B, 'importer', 'importer'],
  ['batteries-extraction', 'IDN', 'CHN', 2 * B, 4 * B, 'importer', 'importer'],
  ['batteries-extraction', 'USA', 'JPN', 0.5 * B, 0.6 * B, 'importer', 'importer'],
  ['batteries-extraction', 'CHN', 'KOR', 0.4 * B, 0.9 * B, 'importer', 'importer'],
  // oil-extraction
  ['oil-extraction', 'USA', 'JPN', 20 * B, 30 * B, 'importer', 'importer'],
  ['oil-extraction', 'RUS', 'CHN', 40 * B, 60 * B, 'importer', 'importer'],
  ['oil-extraction', 'RUS', 'DEU', 20 * B, null, 'importer', 'none'],
  ['oil-extraction', 'BRA', 'CHN', 10 * B, 15 * B, 'importer', 'importer'],
  ['oil-extraction', 'USA', 'KOR', 8 * B, 12 * B, 'importer', 'importer'],
  ['oil-extraction', 'USA', 'DEU', 5 * B, 9 * B, 'importer', 'importer'],
  // semiconductors-components (non-headline)
  ['semiconductors-components', 'KOR', 'CHN', 50 * B, 60 * B, 'importer', 'importer'],
  ['semiconductors-components', 'CHN', 'HKG', 30 * B, 40 * B, 'importer', 'importer'],
  ['semiconductors-components', 'JPN', 'CHN', 20 * B, 22 * B, 'importer', 'importer'],
  // solar-refining: silicon metal only (one sub-component)
  ['solar-refining', 'CHN', 'JPN', 0.8 * B, 1.1 * B, 'importer', 'importer'],
  ['solar-refining', 'CHN', 'KOR', 0.5 * B, 0.7 * B, 'importer', 'importer'],
  ['solar-refining', 'BRA', 'USA', 0.3 * B, 0.4 * B, 'importer', 'importer'],
  ['solar-refining', 'RUS', 'DEU', 0.2 * B, 0.1 * B, 'importer', 'importer'],
  ['solar-refining', 'FRA', 'DEU', 0.1 * B, 0.12 * B, 'importer', 'importer'],
  // semiconductors-refining: gallium, germanium and indium (non-headline)
  ['semiconductors-refining', 'CHN', 'JPN', 0.2 * B, 0.25 * B, 'importer', 'importer'],
  ['semiconductors-refining', 'CHN', 'USA', 0.1 * B, 0.05 * B, 'importer', 'importer'],
  ['semiconductors-refining', 'JPN', 'KOR', 0.03 * B, 0.04 * B, 'importer', 'importer'],
];

/**
 * The sub-components of each trade cell (trade.codes.sub_sector): [name, hs6, description, share of the
 * cell's flows]. A cell's sub-components add up to the cell (shares sum to 1); oil and the two materials
 * baskets have a single group, so the views hide the selector there.
 */
const CELL_SUBS: Record<string, [string, string, string, number][]> = {
  'batteries-assembly': [['Cells and packs', '850760', 'Lithium-ion accumulators', 0.7], ['Materials', '282520', 'Lithium oxide and hydroxide', 0.3]],
  'batteries-extraction': [['Lithium minerals', '253090', 'Mineral substances n.e.s. (lithium minerals)', 0.6], ['Cobalt ores', '260500', 'Cobalt ores and concentrates', 0.4]],
  'oil-extraction': [['Crude oil', '270900', 'Crude petroleum oils', 1]],
  'semiconductors-components': [['Devices', '854231', 'Processors and controllers', 0.8], ['Wafers', '381800', 'Doped chemical elements in discs (wafers)', 0.2]],
  'solar-refining': [['Silicon metal', '280469', 'Silicon, under 99.99% pure', 1]],
  'semiconductors-refining': [['Gallium, germanium and indium', '811292', 'Unwrought gallium, germanium, indium; powders', 1]],
};
/** Importers that did not declare (non-reporters): their flows are mirrors. */
const NON_DECLARERS = new Set(['VNM']);

const TRADE_TECH_OF: Record<string, TradeTechSlug> = {
  'batteries-assembly': 'batteries',
  'batteries-extraction': 'batteries',
  'oil-extraction': 'oil',
  'semiconductors-components': 'semiconductors',
  'solar-refining': 'solar',
  'semiconductors-refining': 'semiconductors',
};

function tradeFacts(): Fact[] {
  const out: Fact[] = [];
  const cells = [...new Set(FLOWS.map((f) => f[0]))];
  for (const c of cells) {
    for (const [year, k, sk] of [[2020, 3, 5], [2024, 4, 6]] as const) {
      const ex = new Map<string, number>();
      const im = new Map<string, number>();
      for (const f of FLOWS) {
        if (f[0] !== c) continue;
        const v = f[k] as number | null;
        if (v === null || f[sk] === 'none') continue;
        ex.set(f[1], (ex.get(f[1]) ?? 0) + v);
        im.set(f[2], (im.get(f[2]) ?? 0) + v);
      }
      for (const [g, v] of ex) out.push({ v: 'trade.exports', item: cell(c), geo: g, year, value: v });
      for (const [g, v] of im) out.push({ v: 'trade.imports', item: cell(c), geo: g, year, value: v, status: NON_DECLARERS.has(g) ? 'mirror' : 'obs' });
      if (year === 2024) {
        for (const g of new Set([...ex.keys(), ...im.keys()])) out.push({ v: 'trade.net', item: cell(c), geo: g, year, value: (ex.get(g) ?? 0) - (im.get(g) ?? 0) });
      }
    }
  }
  return out;
}

function otherFacts(): Fact[] {
  const f: Fact[] = [];
  const add = (v: string, item: number, year: number, rows: [string, number, Status?, number?][]) => {
    for (const [geo, value, status, oy] of rows) f.push({ v, item, geo, year, value, status, obs_year: oy });
  };
  const bm = cell('batteries-assembly');
  const bc = cell('batteries-components');
  // RCA (exporter-reported; HKG excluded)
  add('trade.rca', bm, 2024, [['CHN', 3.1], ['KOR', 2.4], ['JPN', 1.2], ['USA', 0.4], ['DEU', 0.9], ['VNM', 1.6]]);
  // export feasibility (Atlas relatedness, 0-1), 2023
  add('trade.feasibility', bm, 2023, [['CHN', 0.82], ['KOR', 0.71], ['JPN', 0.69], ['DEU', 0.66], ['USA', 0.58], ['FRA', 0.55], ['VNM', 0.47], ['IDN', 0.31], ['BRA', 0.29], ['RUS', 0.22]]);
  add('trade.feasibility', cell('solar-refining'), 2023, [['CHN', 0.74], ['BRA', 0.41], ['USA', 0.52], ['DEU', 0.5]]);
  add('trade.atlas_share', bm, 2023, [['CHN', 0.409], ['KOR', 0.08], ['JPN', 0.07], ['USA', 0.06], ['DEU', 0.09], ['FRA', 0.02], ['VNM', 0.03]]);
  // investment 2018-2025 (est in 2025)
  const inv: Record<string, number[]> = {
    CHN: [10, 12, 14, 20, 30, 34, 33, 32.99],
    USA: [2, 3, 4, 8, 15, 25, 28, 28.51],
    DEU: [1, 1.2, 1.5, 2, 3, 4, 4.2, 4.5],
    KOR: [0.5, 0.6, 0.8, 1, 2, 2.5, 3, 3.2],
    IDN: [0.1, 0.2, 0.3, 0.6, 1, 2, 2.5, 4.9],
    VNM: [0, 0, 0.1, 0.2, 0.3, 0.5, 0.8, 1],
  };
  // the manufacturing capex and capacity shares the sources give once for the technology: on both
  // Components and Assembly, every fact noted same_figure_pair (design C2)
  for (const c of [bc, bm]) {
    for (let y = 2018; y <= 2025; y++) add('investment.annual', c, y, Object.entries(inv).map(([g, xs]) => [g, xs[y - 2018], y === 2025 ? 'est' : xs[y - 2018] === 0 ? 'zero' : 'obs'] as [string, number, Status]));
    // IEA entities: manufacturing shares (sum to 1 with the residual)
    add('manufacturing.share_now', c, 2024, [['E:iea:China', 0.83], ['E:iea:EU', 0.07], ['E:iea:USA', 0.06], ['E:iea:IND', NaN, 'nq'], ['RES:iea', 0.04]]);
    add('manufacturing.share_2035', c, 2035, [['E:iea:China', 0.65], ['E:iea:EU', 0.13], ['E:iea:USA', 0.14], ['E:iea:IND', 0.03], ['RES:iea', 0.05]]);
  }
  // TRL (cell_global, on WLD)
  for (const c of ['batteries-assembly', 'batteries-extraction', 'geothermal-extraction', 'oil-extraction', 'solar-deployment']) {
    add('technology.trl', cell(c), 2025, [['WLD', c === 'geothermal-extraction' ? 7 : 9]]);
    add('technology.trl_change', cell(c), 2025, [['WLD', c === 'geothermal-extraction' ? 1 : 0]]);
  }
  // minerals (IEA, top listed + residual on WLD totals)
  const li = ii('mineral', 'lithium');
  const gr = ii('mineral', 'graphite');
  const iea = (v: string, item: number, rows: Record<number, [string, number][]>) => {
    for (const [y, rs] of Object.entries(rows)) add(v, item, Number(y), rs);
  };
  iea('production.refining', li, {
    2025: [['CHN', 235], ['USA', 10], ['KOR', 30], ['JPN', 15]],
    2030: [['CHN', 300], ['USA', 40], ['KOR', 50], ['JPN', 20]],
    2035: [['CHN', 350], ['USA', 70], ['KOR', 60], ['JPN', 25]],
    2040: [['CHN', 380], ['USA', 100], ['KOR', 70], ['JPN', 30]],
  });
  iea('production.mining', li, {
    2025: [['BRA', 40], ['CHN', 60], ['USA', 5], ['RUS', 2]],
    2030: [['BRA', 60], ['CHN', 80], ['USA', 20], ['RUS', 3]],
    2035: [['BRA', 80], ['CHN', 90], ['USA', 40], ['RUS', 4]],
    2040: [['BRA', 100], ['CHN', 95], ['USA', 60], ['RUS', 5]],
  });
  iea('production.refining', gr, { 2025: [['CHN', 2139], ['JPN', 60], ['KOR', 40]], 2030: [['CHN', 2500], ['JPN', 100], ['KOR', 80]], 2035: [['CHN', 2700], ['JPN', 150], ['KOR', 120]], 2040: [['CHN', 2900], ['JPN', 200], ['KOR', 150]] });
  add('production.mine_usgs', li, 2025, [['BRA', 42], ['CHN', 58], ['USA', 6], ['RUS', 0, 'zero'], ['IDN', NaN, 'nq']]);
  add('resources.mineral_reserves', li, 2025, [['BRA', 3000], ['CHN', 3500], ['USA', 1800], ['RUS', 0, 'zero'], ['IDN', NaN, 'nq']]);
  add('resources.mineral_reserves', gr, 2025, [['CHN', 81000], ['BRA', 74000], ['USA', 0, 'zero']]);
  // critical-minerals trade by stage (mineral_stage items): ores at Extraction, refined products at Refining
  add('trade.critmin_exports', ii('mineral_stage', 'lithium-extraction'), 2024, [['BRA', 2e9], ['CHN', 1e9, 'mirror'], ['USA', 0.5e9], ['HKG', 0.1e9]]);
  add('trade.critmin_exports', ii('mineral_stage', 'lithium-refining'), 2024, [['CHN', 4e9], ['KOR', 1.5e9], ['JPN', 0.8e9], ['USA', 0.3e9]]);
  // fuels (EI; EUU as reported; nsr zero-fills)
  const oil = ii('fuel', 'oil');
  const gas = ii('fuel', 'gas');
  add('production.fuels', oil, 2025, [['USA', 40.1], ['RUS', 22.3], ['CHN', 8.9], ['BRA', 7.4], ['VNM', 0, 'nsr'], ['RES:ei', 110.0]]);
  add('production.fuels', gas, 2025, [['USA', 38.0], ['RUS', 24.0], ['CHN', 8.5], ['DEU', 0.2], ['RES:ei', 70.0]]);
  add('resources.fuel_reserves', oil, 2020, [['RUS', 107000], ['USA', 69000], ['CHN', 26000], ['BRA', 12000]]);
  add('consumption.per_capita', oil, 2025, [['USA', 120.4], ['CHN', 28.1], ['DEU', 61.0], ['FRA', 52.3], ['JPN', 61.8], ['KOR', 110.2], ['BRA', 33.1], ['EUU', 50.2]]);
  add('consumption.pc_change', oil, 2025, [['USA', -2.1], ['CHN', 12.4], ['DEU', -8.0], ['VNM', NaN, 'na_growth']]);
  // Viet Nam's gas production sits in EI's "Other": assumed 0 (user rule 2026-10-11), nsr WITH the value
  add('consumption.balance', gas, 2025, [['USA', 5.2], ['RUS', 8.1], ['CHN', -6.0], ['DEU', -3.0], ['JPN', -3.6], ['KOR', -2.0], ['VNM', -0.4, 'nsr'], ['EUU', -11.0]]);
  add('consumption.self_sufficiency', gas, 2025, [['USA', 115.8], ['RUS', 151.0], ['CHN', 58.6], ['DEU', 6.3], ['JPN', 0, 'zero'], ['VNM', 0, 'nsr']]);
  // generation (the solar deployment cell)
  add('production.generation', cell('solar-deployment'), 2025, [['CHN', 1100], ['USA', 330], ['DEU', 75], ['JPN', 110], ['BRA', 60], ['RES:ei', 500]]);
  add('investment.annual', cell('solar-deployment'), 2025, [['CHN', 120, 'est'], ['USA', 40, 'est'], ['DEU', 15, 'est']]);
  // uranium
  add('production.uranium', cell('nuclear-extraction'), 2024, [['RUS', 2.6], ['CHN', 1.6], ['USA', 0.7], ['BRA', 0, 'zero']]);
  // EV regions (region-level) and EV sales (country)
  add('production.ev_region', cell('electric-vehicles-assembly'), 2024, [['R:iea:China', 12.4], ['R:iea:Europe', 3.0]]);
  add('consumption.ev_sales', cell('electric-vehicles-deployment'), 2025, [['CHN', 12_900_000], ['USA', 1_600_000], ['DEU', 600_000], ['FRA', 400_000], ['KOR', 200_000]]);
  // prices (world, on WLD)
  const crude = ii('commodity', 'crude-oil');
  const cob = ii('commodity', 'cobalt');
  add('prices.latest', crude, 2025, [['WLD', 79.4]]);
  add('prices.latest', cob, 2025, [['WLD', 33000]]);
  add('prices.yoy', crude, 2025, [['WLD', -5.1]]);
  add('prices.yoy', cob, 2025, [['WLD', 12.0]]);
  for (const [k, a, b2] of [['prices.vol_1y', 0.21, 0.4], ['prices.vol_5y', 0.33, 0.52], ['prices.vol_10y', 0.31, 0.45], ['prices.vol_20y', 0.3, 0.42]] as const) {
    add(k, crude, 2025, [['WLD', a]]);
    add(k, cob, 2025, [['WLD', b2]]);
  }
  // context
  const mfg = ii('activity', 'manufacturing');
  add('prices.wages', mfg, 2025, [['USA', 5200, 'obs', 2024], ['CHN', 1100, 'obs', 2023], ['DEU', 4600, 'obs', 2024], ['VNM', 320, 'obs', 2016], ['KOR', 3300, 'obs', 2024], ['IDN', 250, 'obs', 2022]]);
  const none = ii('none', 'none');
  add('prices.lending_rate', none, 2025, [['USA', 7.5, 'obs', 2025], ['CHN', 3.1, 'obs', 2025], ['DEU', 4.2, 'obs', 2024], ['KOR', 4.9, 'obs', 2025]]);
  add('prices.ppi_yoy', none, 2025, [['USA', 2.1, 'obs', 2025], ['CHN', -1.8, 'obs', 2025], ['DEU', -0.4, 'obs', 2025]]);
  add('development.alignment', none, 2023, [['USA', 1.0], ['CHN', 0.0], ['DEU', 0.86], ['FRA', 0.85], ['JPN', 0.88], ['KOR', 0.84], ['BRA', 0.4], ['IDN', 0.3], ['VNM', 0.1], ['RUS', 0.05]]);
  return f;
}

function worldRowsOf(facts: Fact[]): { v: string; item: number; year: number; value: number; basis: string }[] {
  const out: { v: string; item: number; year: number; value: number; basis: string }[] = [];
  const keys = new Map<string, Fact[]>();
  for (const x of facts) {
    const k = `${x.v}|${x.item}|${x.year}`;
    let l = keys.get(k);
    if (!l) keys.set(k, (l = []));
    l.push(x);
  }
  for (const [k, rows] of keys) {
    const [v, item, year] = k.split('|');
    const def = VARS[vi(v)];
    if (def.basis === 'none' || def.basis === 'sum_listed') continue;
    let value: number;
    if (def.basis === 'one') value = 1;
    else {
      value = 0;
      for (const r of rows) {
        const g = GEOS[gi(r.geo)];
        if (Number.isFinite(r.value) && g.kind !== 'eu' && g.kind !== 'world') value += r.value;
      }
      // IEA and USGS totals exceed the listed producers (unattributed residual)
      if (def.source === 'iea_cm' || def.source === 'usgs') value = Math.round(value * 1.08 * 1e6) / 1e6;
      if (def.id === 'production.refining' && Number(item) === ii('mineral', 'lithium') && Number(year) === 2025) value = 319;
      if (def.id === 'production.refining' && Number(item) === ii('mineral', 'graphite') && Number(year) === 2025) value = 2266;
    }
    out.push({ v, item: Number(item), year: Number(year), value, basis: def.basis });
  }
  return out;
}

// ---- assembling ----------------------------------------------------------------------------------

function obsAndIndex(facts: Fact[]) {
  const rows = facts
    .map((x) => ({ var: vi(x.v), item: x.item, geo: gi(x.geo), year: x.year, value: x.value, status: x.status ?? (x.value === 0 ? 'zero' : 'obs'), obs_year: x.obs_year ?? null, obs_from: x.obs_from ?? null }))
    .sort((a, z) => a.var - z.var || a.item - z.item || a.year - z.year || a.geo - z.geo);
  const index: RowObj[] = [];
  rows.forEach((r, i) => {
    const last = index[index.length - 1];
    if (last && last.var === r.var && last.item === r.item && last.year === r.year) last.n = (last.n as number) + 1;
    else index.push({ var: r.var, item: r.item, year: r.year, start: i, n: 1 });
  });
  return { rows, index };
}

function seriesFacts(): Fact[] {
  const out: Fact[] = [];
  const oil = ii('fuel', 'oil');
  const base: Record<string, number> = { USA: 30, RUS: 21, CHN: 8.2, BRA: 5 };
  const last: Record<string, number> = { USA: 40.1, RUS: 22.3, CHN: 8.9, BRA: 7.4 };
  for (let y = 2015; y <= 2025; y++) {
    for (const g of Object.keys(base)) out.push({ v: 'production.fuels', item: oil, geo: g, year: y, value: y === 2025 ? last[g] : Math.round((base[g] + ((last[g] - base[g]) * (y - 2015)) / 10) * 100) / 100 });
    out.push({ v: 'production.fuels', item: oil, geo: 'RES:ei', year: y, value: y === 2025 ? 110 : 100 + (y - 2015) });
    out.push({ v: 'consumption.total_ej', item: oil, geo: 'USA', year: y, value: 35 + (y - 2015) * 0.2 });
    out.push({ v: 'consumption.total_ej', item: oil, geo: 'CHN', year: y, value: 22 + (y - 2015) * 0.8 });
  }
  const sd = cell('solar-deployment');
  for (let y = 2018; y <= 2025; y++) {
    out.push({ v: 'investment.power_additions', item: sd, geo: 'CHN', year: y, value: 40 + (y - 2018) * 30, status: y === 2025 ? 'est' : 'obs' });
    out.push({ v: 'investment.power_additions', item: sd, geo: 'USA', year: y, value: 10 + (y - 2018) * 4, status: y === 2025 ? 'est' : 'obs' });
  }
  const evm = cell('electric-vehicles-assembly');
  for (let y = 2021; y <= 2024; y++) {
    out.push({ v: 'production.ev_region', item: evm, geo: 'R:iea:China', year: y, value: y === 2024 ? 12.4 : 3 + (y - 2021) * 3 });
    out.push({ v: 'production.ev_region', item: evm, geo: 'R:iea:Europe', year: y, value: y === 2024 ? 3.0 : 2 + (y - 2021) * 0.3 });
  }
  return out;
}

function seriesWorld(facts: Fact[]) {
  const out: RowObj[] = [];
  const oil = ii('fuel', 'oil');
  for (let y = 2015; y <= 2025; y++) {
    const s = facts.filter((x) => x.v === 'production.fuels' && x.year === y).reduce((a, x) => a + x.value, 0);
    out.push({ var: vi('production.fuels'), item: oil, year: y, value: s, basis: 'source_world' });
    const c = facts.filter((x) => x.v === 'consumption.total_ej' && x.year === y).reduce((a, x) => a + x.value, 0);
    out.push({ var: vi('consumption.total_ej'), item: oil, year: y, value: c * 3, basis: 'source_world' });
  }
  return out.sort((a, z) => (a.var as number) - (z.var as number) || (a.item as number) - (z.item as number) || (a.year as number) - (z.year as number));
}

const POLICY_TAGS = ['Batteries', 'Solar', 'Oil', 'Critical minerals', 'Cross-cutting'];
interface PolicyDef {
  geo: string;
  year: number | null;
  status: string;
  type: string;
  tags: string[];
  stages: StageSlug[];
  cross?: boolean;
  src?: 'iea' | 'text';
  title: string;
}
const POLICIES: PolicyDef[] = [
  { geo: 'USA', year: 2022, status: 'In force', type: 'Fiscal/financial incentives', tags: ['Batteries'], stages: ['assembly'], title: 'Advanced manufacturing production credit' },
  { geo: 'USA', year: 2021, status: 'In force', type: 'RD&D', tags: ['Batteries'], stages: ['refining', 'assembly'], title: 'Battery materials processing grants' },
  { geo: 'USA', year: 2015, status: 'Ended', type: 'Regulation', tags: ['Batteries', 'Solar'], stages: ['deployment'], title: 'Storage procurement mandate' },
  { geo: 'CHN', year: 2020, status: 'In force', type: 'Regulation', tags: ['Batteries'], stages: ['assembly'], title: 'Battery industry standard conditions' },
  { geo: 'CHN', year: 2027, status: 'Announced', type: 'Fiscal/financial incentives', tags: ['Batteries'], stages: ['assembly'], title: 'New energy storage action plan' },
  { geo: 'CHN', year: 2018, status: 'In force', type: 'Regulation', tags: [], stages: [], cross: true, title: 'Energy law' },
  { geo: 'DEU', year: 2019, status: 'In force', type: 'Fiscal/financial incentives', tags: ['Batteries'], stages: ['assembly'], src: 'text', title: 'Battery cell research factory' },
  { geo: 'EUU', year: 2023, status: 'In force', type: 'Regulation', tags: ['Batteries', 'Critical minerals'], stages: ['extraction', 'assembly'], title: 'EU Batteries Regulation' },
  { geo: 'EUU', year: 2024, status: 'In force', type: 'Regulation', tags: ['Critical minerals'], stages: ['extraction'], title: 'Critical Raw Materials Act' },
  { geo: 'JPN', year: 2028, status: 'Planned', type: 'RD&D', tags: ['Batteries'], stages: ['assembly'], title: 'Next-generation battery programme' },
  { geo: 'JPN', year: 2010, status: 'Ended', type: 'Other', tags: ['Solar'], stages: ['deployment'], title: 'Solar feed-in tariff' },
  { geo: 'KOR', year: 2023, status: 'In force', type: 'Fiscal/financial incentives', tags: ['Batteries'], stages: ['assembly'], title: 'K-battery strategy' },
  { geo: 'BRA', year: 2024, status: 'In force', type: 'Regulation', tags: ['Oil'], stages: ['extraction'], title: 'Oil licensing round' },
  { geo: 'USA', year: null, status: 'In force', type: 'Regulation', tags: [], stages: [], cross: true, title: 'Clean air rules' },
];

/** The chunk objects and manifest of the fixture. */
export interface SectorFixture {
  manifest: SectorManifest;
  chunks: Record<string, Record<string, unknown>>;
  /** names in the bundle, for tests */
  files: Record<string, string>;
}

let cached: SectorFixture | null = null;

/** The fixture (built once). */
export function buildSectorFixture(): SectorFixture {
  if (cached) return cached;
  const facts = [...tradeFacts(), ...otherFacts()];
  const { rows: obsRows, index } = obsAndIndex(facts);
  const worldRows = worldRowsOf(facts).map((w) => ({ var: vi(w.v), item: w.item, year: w.year, value: w.value, basis: w.basis })).sort((a, z) => a.var - z.var || a.item - z.item || a.year - z.year);

  // meta ------------------------------------------------------------------------------------------
  const dimsGeo = table(META_SPEC['dims.geo'], GEOS.map((g) => ({
    code: g.code, name: g.name, kind: g.kind, iso_n3: g.n3 ?? null, tier: g.tier, group: g.group, eu_member: !!g.eu, re_export_hub: !!g.hub,
    bloc: g.code === 'USA' || g.group === 'core' ? 'us' : g.code === 'CHN' ? 'china' : g.kind === 'country' ? 'nonaligned' : null,
    s_axis: null, alignment_basis: g.kind === 'country' ? 'own votes' : null, wb_region: g.kind === 'country' ? 'Region' : null, income: g.kind === 'country' ? 'High income' : null, lon: null, lat: null, reason: g.reason ?? null,
  })));
  const dimsItems = table(META_SPEC['dims.items'], ITEMS.map((x, i) => ({
    axis: x.axis, slug: x.slug, label: x.label, tech: x.tech ? TECH_LABELS[x.tech] : null, stage: x.stage ? STAGE_LABELS[x.stage] : null,
    headline: x.axis === 'cell' ? x.tech !== 'semiconductors' : false, sort: i, has_trade: !!x.has_trade,
  })));
  const links = [
    // minerals link to the Extraction and Refining cells of the technologies that use them (schema 2)
    { item: ii('mineral', 'lithium'), cell: cell('batteries-extraction'), weight: 0.62 },
    { item: ii('mineral', 'lithium'), cell: cell('batteries-refining'), weight: 0.62 },
    { item: ii('mineral', 'lithium'), cell: cell('electric-vehicles-extraction'), weight: 0.3 },
    { item: ii('mineral', 'graphite'), cell: cell('batteries-extraction'), weight: 0.9 },
    { item: ii('mineral', 'graphite'), cell: cell('batteries-refining'), weight: 0.9 },
    { item: ii('fuel', 'oil'), cell: cell('oil-extraction'), weight: 1 },
    { item: ii('fuel', 'oil'), cell: cell('oil-deployment'), weight: 1 },
    { item: ii('fuel', 'gas'), cell: cell('gas-extraction'), weight: 1 },
    { item: ii('commodity', 'crude-oil'), cell: cell('oil-extraction'), weight: 1 },
    { item: ii('commodity', 'cobalt'), cell: cell('batteries-extraction'), weight: 1 },
    { item: ii('activity', 'manufacturing'), cell: cell('batteries-assembly'), weight: 1 },
  ];
  const mineralDemand = [
    { mineral: ii('mineral', 'lithium'), tech: 'Batteries', label: 'Grid battery storage', scenario: 'observed', year: 2025, kt: 40 },
    { mineral: ii('mineral', 'lithium'), tech: 'Batteries', label: 'Grid battery storage', scenario: 'STEPS', year: 2040, kt: 150 },
    { mineral: ii('mineral', 'lithium'), tech: 'Electric Vehicles', label: 'Electric vehicles', scenario: 'observed', year: 2025, kt: 120 },
    { mineral: ii('mineral', 'lithium'), tech: 'Electric Vehicles', label: 'Electric vehicles', scenario: 'STEPS', year: 2040, kt: 520 },
    { mineral: ii('mineral', 'lithium'), tech: null, label: 'Total clean technologies', scenario: 'observed', year: 2025, kt: 165 },
    { mineral: ii('mineral', 'lithium'), tech: null, label: 'Total clean technologies', scenario: 'STEPS', year: 2040, kt: 690 },
    { mineral: ii('mineral', 'graphite'), tech: 'Electric Vehicles', label: 'Electric vehicles', scenario: 'observed', year: 2025, kt: 900 },
    { mineral: ii('mineral', 'graphite'), tech: 'Electric Vehicles', label: 'Electric vehicles', scenario: 'STEPS', year: 2040, kt: 3200 },
  ];
  const dictVars = table(META_SPEC['dict.vars'], VARS.map((v) => ({
    id: v.id, theme: v.id.split('.')[0], family: v.family, label: v.label, short: v.short, definition: v.definition ?? null, unit: v.unit, data_year: v.data_year, years: v.years.join('|'),
    year_field: !!v.year_field, native_level: v.level, item_axis: v.axis, agg: v.agg, world_basis: v.basis, norms: v.basis === 'none' ? 'value' : 'value|share', scale: v.scale ?? null, higher_is: 'neutral',
    sign_note: v.id === 'trade.net' ? 'positive = net exporter' : null, display_scale: 1, source: v.source, roster: v.roster ?? null, licence: SOURCES.find((s) => s.id === v.source)?.licence ?? 'open',
    caveats: (v.caveats ?? []).join('|') || null, primary_for: (v.primary_for ?? []).join('|') || null, section: v.section ?? null, stale_years: v.stale_years ?? null, min_base: null, feeds: (v.feeds ?? []).join('|') || null,
    prov: v.prov ?? 'P', prov_detail: null, phase: '1', projection_from: v.projection_from ?? null, series_window: v.series_window ?? null,
  })));
  const rosterRows: RowObj[] = [];
  const rosterOf: Record<string, string[]> = {
    comtrade_matrix: COUNTRIES.filter((c) => c !== 'CYP' || true),
    usgs: ['BRA', 'CHN', 'USA', 'RUS', 'IDN'],
    ei: ['USA', 'RUS', 'CHN', 'BRA', 'DEU', 'FRA', 'JPN', 'KOR', 'VNM'],
    iea_cm: ['BRA', 'CHN', 'USA', 'RUS', 'KOR', 'JPN'],
    cim: ['CHN', 'USA', 'DEU', 'KOR', 'IDN', 'VNM', 'JPN'],
    iea_ev: ['CHN', 'USA', 'DEU', 'FRA', 'KOR', 'JPN'],
    atlas: ['CHN', 'KOR', 'JPN', 'DEU', 'USA', 'FRA', 'VNM', 'IDN', 'BRA', 'RUS'],
  };
  for (const [r, gs] of Object.entries(rosterOf)) for (const g of gs) rosterRows.push({ roster: r, geo: gi(g) });

  const coverageRows: RowObj[] = [];
  const cov = (slug: string, family: string, status: string, n: number, reason: string | null, sources: string[]) => coverageRows.push({ item: cell(slug), family, status, n_geo: n, reason, sources: sources.join('|') || null });
  for (const it of ITEMS.filter((x) => x.axis === 'cell')) {
    const s = it.slug;
    if (s === 'batteries-assembly') {
      cov(s, 'exports', 'country', 13, null, ['comtrade_matrix']);
      cov(s, 'investment', 'country', 6, null, ['cim']);
      cov(s, 'capacity', 'region', 4, null, ['iea_etp']);
      cov(s, 'technology', 'global', 0, null, ['iea_trl']);
      cov(s, 'reserves', 'none', 0, 'not_applicable', []);
      cov(s, 'policy', 'country', 6, null, ['pams']);
      cov(s, 'context', 'country', 6, null, ['ilo']);
    } else if (s === 'batteries-extraction') {
      cov(s, 'exports', 'country', 5, null, ['comtrade_matrix']);
      cov(s, 'production', 'none', 0, 'mineral_level', ['iea_cm']);
      cov(s, 'reserves', 'none', 0, 'mineral_level', ['usgs']);
      cov(s, 'technology', 'global', 0, null, ['iea_trl']);
      cov(s, 'prices', 'global', 0, null, ['imf']);
    } else if (s === 'oil-extraction') {
      cov(s, 'exports', 'country', 7, null, ['comtrade_matrix']);
      cov(s, 'production', 'country', 4, null, ['ei']);
      cov(s, 'reserves', 'country', 4, null, ['ei']);
      cov(s, 'prices', 'global', 0, null, ['imf']);
      cov(s, 'technology', 'global', 0, null, ['iea_trl']);
    } else if (s === 'solar-deployment') {
      cov(s, 'production', 'country', 5, null, ['ei']);
      cov(s, 'investment', 'country', 3, null, ['cim']);
      cov(s, 'technology', 'global', 0, null, ['iea_trl']);
      cov(s, 'prices', 'licensed', 0, 'licensed', []);
    } else if (s === 'geothermal-extraction') {
      cov(s, 'technology', 'global', 0, null, ['iea_trl']);
      cov(s, 'exports', 'none', 0, 'no_basket', []);
    } else if (s === 'semiconductors-components') {
      cov(s, 'exports', 'country', 4, null, ['comtrade_matrix']);
    } else if (s === 'solar-refining') {
      cov(s, 'exports', 'country', 7, null, ['comtrade_matrix']);
    } else if (s === 'semiconductors-refining') {
      cov(s, 'exports', 'country', 4, null, ['comtrade_matrix']);
    } else if (s === 'nuclear-extraction') {
      cov(s, 'production', 'country', 4, null, ['redbook']);
      cov(s, 'exports', 'none', 0, 'no_basket', []);
    } else if (s === 'electric-vehicles-assembly') {
      cov(s, 'production', 'region', 2, null, ['iea_ev']);
    } else if (s === 'electric-vehicles-deployment') {
      cov(s, 'consumption', 'country', 5, null, ['iea_ev']);
    } else {
      cov(s, 'exports', 'none', 0, 'no_source', []);
    }
  }

  // availability, from facts + series + trade + policy
  const availMap = new Map<string, { var: number; item: number; chunk: string; years: Set<number>; geos: Set<number> }>();
  const touch = (chunk: string, v: number, item: number, year: number, geo: number | null) => {
    const k = `${v}|${item}|${chunk}`;
    let a = availMap.get(k);
    if (!a) availMap.set(k, (a = { var: v, item, chunk, years: new Set(), geos: new Set() }));
    a.years.add(year);
    if (geo !== null && GEOS[geo]?.kind === 'country') a.geos.add(geo);
  };
  for (const r of obsRows) touch('facts', r.var, r.item, r.year, r.geo);
  const sFacts = seriesFacts();
  for (const r of sFacts) touch('series', vi(r.v), r.item, r.year, gi(r.geo));
  for (const f of FLOWS) {
    if (f[3] !== null) touch('trade', vi('trade.flow'), cell(f[0]), 2020, gi(f[1]));
    if (f[4] !== null) touch('trade', vi('trade.flow'), cell(f[0]), 2024, gi(f[1]));
  }
  const tagSlug = (t: string) => t.toLowerCase().replace(/[^a-z0-9]+/g, '-');
  for (const p of POLICIES) for (const t of p.cross ? ['cross-cutting'] : p.tags.map(tagSlug)) touch('policy', vi('policy.records'), ii('policy_tag', t), p.year ?? 2025, gi(p.geo));
  for (const d of mineralDemand) touch('meta', vi('consumption.mineral_demand'), d.mineral, d.year, null);
  touch('policy', vi('policy.cat'), ii('none', 'none'), 2025, gi('USA'));
  for (let y = 2023; y <= 2025; y++) {
    touch('series', vi('prices.monthly'), ii('commodity', 'crude-oil'), y, gi('WLD'));
    touch('series', vi('prices.monthly'), ii('commodity', 'cobalt'), y, gi('WLD'));
  }
  touch('series', vi('manufacturing.capacity'), cell('batteries-assembly'), 2025, gi('CHN'));
  const availRows = [...availMap.values()].map((a) => {
    const ys = [...a.years].sort((x, z) => x - z);
    return { var: a.var, item: a.item, chunk: a.chunk, year_min: ys[0], year_max: ys[ys.length - 1], n_years: ys.length, n_geo: a.geos.size };
  });

  const metaPartial: Record<string, unknown> = {
    'dims.geo': dimsGeo,
    'dims.region_members': table(META_SPEC['dims.region_members'], [
      { geo: gi('R:iea:Europe'), member: gi('DEU') },
      { geo: gi('R:iea:Europe'), member: gi('FRA') },
      { geo: gi('R:iea:Europe'), member: gi('CYP') },
      { geo: gi('R:iea:China'), member: gi('CHN') },
    ]),
    'dims.items': dimsItems,
    'dims.item_links': table(META_SPEC['dims.item_links'], links),
    'dims.mineral_demand': table(META_SPEC['dims.mineral_demand'], mineralDemand),
    'dims.eu_outside_coalition': ['CYP'],
    'dict.vars': dictVars,
    'dict.units': table(META_SPEC['dict.units'], UNITS),
    'dict.sources': table(META_SPEC['dict.sources'], SOURCES),
    'dict.caveats': table(META_SPEC['dict.caveats'], CAVEATS),
    'dict.themes': table(META_SPEC['dict.themes'], [
      { slug: 'trade', label: 'Trade', description: 'Exports, imports and flows of each technology’s HS6 basket.', order: 1, default_var: 'trade.exports' },
      { slug: 'resources', label: 'Reserves & resources', description: 'Reserves of fuels and minerals; solar, wind and geothermal potential.', order: 2, default_var: 'resources.mineral_reserves' },
      { slug: 'production', label: 'Production', description: 'Fuels, electricity, mining and refining.', order: 3, default_var: 'production.fuels' },
      { slug: 'consumption', label: 'Consumption & dependence', description: 'Energy use per person, net balances and EV uptake.', order: 4, default_var: 'consumption.per_capita' },
      { slug: 'investment', label: 'Investment', description: 'Clean investment by sector.', order: 5, default_var: 'investment.annual' },
      { slug: 'manufacturing', label: 'Manufacturing capacity', description: 'Shares of global manufacturing capacity.', order: 6, default_var: 'manufacturing.share_now' },
      { slug: 'prices', label: 'Prices & costs', description: 'Commodity prices, wages, rates.', order: 7, default_var: 'prices.latest' },
      { slug: 'technology', label: 'Technology & outlook', description: 'Technology readiness.', order: 8, default_var: 'technology.trl' },
      { slug: 'policy', label: 'Policy', description: 'Recorded policies and CAT ratings.', order: 9, default_var: 'policy.records' },
      { slug: 'development', label: 'Development & finance', description: 'UN-vote alignment.', order: 10, default_var: 'development.alignment' },
    ]),
    'dict.families': table(META_SPEC['dict.families'], [
      { id: 'exports', label: 'Trade', section: 'trade' },
      { id: 'production', label: 'Production', section: 'production' },
      { id: 'capacity', label: 'Capacity', section: 'production' },
      { id: 'reserves', label: 'Reserves', section: 'resources' },
      { id: 'investment', label: 'Investment', section: 'investment' },
      { id: 'consumption', label: 'Consumption', section: 'demand' },
      { id: 'prices', label: 'Prices', section: 'prices' },
      { id: 'policy', label: 'Policy', section: 'policy' },
      { id: 'technology', label: 'Technology', section: 'technology' },
      { id: 'context', label: 'Stage context', section: 'context' },
      { id: 'development', label: 'Development', section: null },
    ]),
    'dict.cells': table(META_SPEC['dict.cells'], [
      // section_cards: the var ids each cell's profile can show (scripts/87 convention, build_log A 1)
      { slug: 'batteries-assembly', cell_blurb: 'Cathode and anode materials, lithium chemicals, electrolytes, separators, cells and packs.', section_cards: 'trade.exports|trade.imports|trade.net|trade.flow|trade.rca|trade.atlas_share|manufacturing.share_now|manufacturing.share_2035|investment.annual|prices.wages|prices.lending_rate|prices.ppi_yoy|technology.trl|technology.trl_change|policy.records' },
      { slug: 'oil-extraction', cell_blurb: 'Crude oil production, reserves and trade.', section_cards: 'trade.exports|trade.imports|trade.net|trade.flow|resources.fuel_reserves|production.fuels|consumption.balance|prices.latest|prices.yoy|prices.monthly|prices.lending_rate|technology.trl|policy.records' },
      { slug: 'oil-deployment', cell_blurb: 'Refined products and oil-fired power: consumption and generation.', section_cards: 'consumption.per_capita|consumption.pc_change|prices.lending_rate|policy.records' },
      { slug: 'gas-extraction', cell_blurb: 'Natural gas production, reserves and trade.', section_cards: 'resources.fuel_reserves|production.fuels|consumption.balance|consumption.self_sufficiency' },
      { slug: 'solar-refining', cell_blurb: 'The materials behind solar: trade in silicon metal, the feedstock of polysilicon.', section_cards: 'trade.exports|trade.imports|trade.net|trade.flow|trade.feasibility' },
      { slug: 'semiconductors-refining', cell_blurb: 'Non-headline basket: unwrought gallium, germanium and indium, their powders and scrap.', section_cards: 'trade.exports|trade.imports|trade.net|trade.flow' },
    ]),
    'dict.cell_caveats': table(META_SPEC['dict.cell_caveats'], [
      { slug: 'semiconductors-refining', caveat: 'basket_semis_refining' },
      { slug: 'solar-refining', caveat: 'basket_solar_refining' },
    ]),
    'dict.hhi_refs': table(META_SPEC['dict.hhi_refs'], [
      { id: 'eu_2004_unlikely', value: 0.1, points: 1000, side: 'below', label: 'concentration concerns unlikely', authority: 'European Commission', document: 'Guidelines on the assessment of horizontal mergers (2004/C 31/03), paragraph 19', year: 2004 },
      { id: 'us_2023_high', value: 0.18, points: 1800, side: 'above', label: 'highly concentrated', authority: 'US Department of Justice and Federal Trade Commission', document: 'Merger Guidelines (2023), Guideline 1', year: 2023 },
    ]),
    'dict.techs': table(META_SPEC['dict.techs'], TECH_SLUGS.map((t) => ({ slug: t, label: TECH_LABELS[t], tech_blurb: `${TECH_LABELS[t]}: from raw materials to deployment.` }))),
    'dict.item_units': table(META_SPEC['dict.item_units'], [
      { var: vi('production.fuels'), item: ii('fuel', 'oil'), unit: 'kbd' },
      { var: vi('prices.latest'), item: ii('commodity', 'cobalt'), unit: 'usd_t' },
      { var: vi('prices.monthly'), item: ii('commodity', 'cobalt'), unit: 'usd_t' },
    ]),
    rosters: table(META_SPEC.rosters, rosterRows),
    coverage: table(META_SPEC.coverage, coverageRows),
    avail: table(META_SPEC.avail, availRows),
    summary: table(META_SPEC.summary, []),
    reconcile: table(META_SPEC.reconcile, [
      { item: cell('batteries-assembly'), year: 2024, matrix_total: facts.filter((x) => x.v === 'trade.exports' && x.year === 2024 && x.item === cell('batteries-assembly')).reduce((a, x) => a + x.value, 0), matrix_china: 0.65, matrix_hhi: 0.45, reported_total: 70e9, reported_china: 0.6, reported_hhi: 0.4, n_reporters: 11, warn: true },
      { item: cell('oil-extraction'), year: 2024, matrix_total: 126e9, matrix_china: 0, matrix_hhi: 0.31, reported_total: 120e9, reported_china: 0, reported_hhi: 0.3, n_reporters: 11, warn: false },
    ]),
  };

  // facts -------------------------------------------------------------------------------------------
  // facts.notes: the assumed-zero balance of Viet Nam gas, and an EIA-filled German gas production
  // same_figure_pair on every investment and capacity-share fact Components and Assembly both carry (design C2)
  const pairRows = obsRows
    .filter((r) => (r.item === cell('batteries-components') || r.item === cell('batteries-assembly')) && ['investment.annual', 'manufacturing.share_now', 'manufacturing.share_2035'].includes(VARS[r.var].id))
    .map((r) => ({ var: r.var, item: r.item, geo: r.geo, year: r.year, note: 'same_figure_pair' }));
  const noteRows = [
    ...pairRows,
    { var: vi('consumption.balance'), item: ii('fuel', 'gas'), geo: gi('VNM'), year: 2025, note: 'production_assumed_zero' },
    { var: vi('consumption.self_sufficiency'), item: ii('fuel', 'gas'), geo: gi('VNM'), year: 2025, note: 'production_assumed_zero' },
    { var: vi('production.fuels'), item: ii('fuel', 'gas'), geo: gi('DEU'), year: 2025, note: 'eia_production' },
  ].sort((a, z) => a.var - z.var || a.item - z.item || a.year - z.year || a.geo - z.geo);
  const factsChunk: Record<string, unknown> = {
    'facts.obs': table(FACTS_SPEC['facts.obs'], obsRows),
    'facts.index': table(FACTS_SPEC['facts.index'], index),
    'facts.world': table(FACTS_SPEC['facts.world'], worldRows),
    'facts.notes': table(FACTS_SPEC['facts.notes'], noteRows),
  };

  // trade -------------------------------------------------------------------------------------------
  const tradeChunks: Record<string, Record<string, unknown>> = {};
  for (const tech of [...new Set(Object.values(TRADE_TECH_OF))]) {
    const cells = Object.entries(TRADE_TECH_OF).filter(([, t]) => t === tech).map(([c]) => c);
    const flows = FLOWS.filter((f) => cells.includes(f[0]))
      .map((f) => ({ item: cell(f[0]), exporter: gi(f[1]), importer: gi(f[2]), v2020: f[3], v2024: f[4], s2020: f[5], s2024: f[6] }))
      .sort((a, z) => a.item - z.item || a.exporter - z.exporter || a.importer - z.importer);
    const declaredRows: RowObj[] = [];
    for (const c of cells) for (const y of [2020, 2024]) for (const g of COUNTRIES) declaredRows.push({ item: cell(c), geo: gi(g), year: y, imports_declared: !NON_DECLARERS.has(g), exports_declared: !NON_DECLARERS.has(g) && g !== 'HKG' });
    const codes: RowObj[] = [];
    const excluded: RowObj[] = [];
    const ctySub: RowObj[] = [];
    const subIntra: RowObj[] = [];
    const isEu = (g: string) => !!GEOS[gi(g)].eu;
    for (const c of cells) {
      for (const [sub, hs6, description, w] of CELL_SUBS[c]) {
        codes.push({ item: cell(c), sub_sector: sub, hs6, description, shared_n: sub === 'Materials' || sub === 'Lithium minerals' ? 2 : 0, caveat: sub === 'Materials' ? 'kept with caveat' : null });
        // trade.cty_sub: the cell's flows times the sub-component's share (so they add up to the cell)
        for (const [year, k, sk] of [[2020, 3, 5], [2024, 4, 6]] as const) {
          const ex = new Map<string, number>();
          const im = new Map<string, number>();
          let W = 0;
          let intra = 0;
          for (const f of FLOWS) {
            if (f[0] !== c) continue;
            const v = f[k] as number | null;
            if (v === null || f[sk] === 'none') continue;
            ex.set(f[1], (ex.get(f[1]) ?? 0) + v * w);
            im.set(f[2], (im.get(f[2]) ?? 0) + v * w);
            W += v * w;
            if (isEu(f[1]) && isEu(f[2])) intra += v * w;
          }
          for (const g of [...new Set([...ex.keys(), ...im.keys()])].sort()) ctySub.push({ item: cell(c), sub_sector: sub, geo: gi(g), year, exports: ex.get(g) ?? 0, imports: im.get(g) ?? 0 });
          if (W > 0) ctySub.push({ item: cell(c), sub_sector: sub, geo: gi('WLD'), year, exports: W, imports: W });
          if (intra > 0) subIntra.push({ item: cell(c), sub_sector: sub, year, intra });
        }
      }
    }
    excluded.push({ tech: TECH_LABELS[tech], sub_sector: 'Parts', hs6: '851762', reason: 'General-purpose communication equipment' });
    ctySub.sort((a, z) => (a.item as number) - (z.item as number));
    tradeChunks[`sector-trade-${tech}`] = {
      'trade.flows': table(TRADE_SPEC['trade.flows'], flows),
      'trade.declared': table(TRADE_SPEC['trade.declared'], declaredRows),
      'trade.codes': table(TRADE_SPEC['trade.codes'], codes),
      'trade.excluded': table(TRADE_SPEC['trade.excluded'], excluded),
      'trade.cty_sub': table(TRADE_SPEC['trade.cty_sub'], ctySub),
      'trade.sub_intra': table(TRADE_SPEC['trade.sub_intra'], subIntra),
    };
  }

  // policy ------------------------------------------------------------------------------------------
  const pams = POLICIES.map((p, i) => ({
    id: 1000 + i,
    geo: gi(p.geo),
    year: p.year,
    status: p.status,
    jurisdiction: p.geo === 'EUU' ? 'Supranational' : 'National',
    type: p.type,
    tech_mask: p.tags.reduce((m, t) => m | (1 << POLICY_TAGS.indexOf(t)), 0),
    tech_src: p.src ?? (p.tags.length ? 'iea' : null),
    stage_mask: p.stages.reduce((m, s) => m | (1 << STAGE_SLUGS.indexOf(s)), 0),
    cross_cutting: !!p.cross,
  }));
  const policyChunk = {
    'policy.pams': table(POLICY_SPEC['policy.pams'], pams),
    'policy.tags': POLICY_TAGS,
    'policy.cat': table(POLICY_SPEC['policy.cat'], [
      { geo: gi('USA'), overall: 'Insufficient', policies_action: 'Insufficient', domestic_target: 'Almost sufficient', fair_share: 'Insufficient', climate_finance: 'Critically insufficient', ndc: null, updated: '2025-11' },
      { geo: gi('CHN'), overall: 'Highly insufficient', policies_action: 'Highly insufficient', domestic_target: 'Insufficient', fair_share: 'Highly insufficient', climate_finance: null, ndc: null, updated: '2025-10' },
      { geo: gi('EUU'), overall: 'Insufficient', policies_action: 'Insufficient', domestic_target: 'Insufficient', fair_share: 'Insufficient', climate_finance: 'Insufficient', ndc: null, updated: '2025-09' },
    ]),
  };
  const policyText = {
    'policy.text': table(POLICY_TEXT_SPEC['policy.text'], POLICIES.map((p, i) => ({ id: 1000 + i, title: p.title, url: `https://www.iea.org/policies/${1000 + i}-${p.title.toLowerCase().replace(/[^a-z0-9]+/g, '-')}` }))),
  };

  // series ------------------------------------------------------------------------------------------
  const sRows = sFacts
    .map((x) => ({ var: vi(x.v), item: x.item, geo: gi(x.geo), year: x.year, value: x.value, status: x.status ?? 'obs', obs_year: null, obs_from: null }))
    .sort((a, z) => a.var - z.var || a.item - z.item || a.year - z.year || a.geo - z.geo);
  const sIndex: RowObj[] = [];
  sRows.forEach((r, i) => {
    const last = sIndex[sIndex.length - 1];
    if (last && last.var === r.var && last.item === r.item && last.year === r.year) last.n = (last.n as number) + 1;
    else sIndex.push({ var: r.var, item: r.item, year: r.year, start: i, n: 1 });
  });
  const monthly: RowObj[] = [];
  for (const [slug, base] of [['crude-oil', 80], ['cobalt', 30000]] as const) {
    for (let y = 2023; y <= 2025; y++) for (let m = 1; m <= 12; m++) monthly.push({ var: vi('prices.monthly'), item: ii('commodity', slug), geo: gi('WLD'), year: y, month: m, value: Math.round(base * (1 + 0.05 * Math.sin((y * 12 + m) / 3)) * 100) / 100, status: 'obs' });
  }
  monthly.sort((a, z) => (a.var as number) - (z.var as number) || (a.item as number) - (z.item as number) || (a.geo as number) - (z.geo as number) || (a.year as number) - (z.year as number) || (a.month as number) - (z.month as number));
  const capacity: RowObj[] = [];
  for (const [g, op, con, ann] of [['CHN', 2400, 600, 900], ['USA', 200, 300, 500], ['DEU', 120, 60, 90], ['KOR', 150, 40, 30]] as const) {
    for (const [st, v] of [['operational', op], ['construction', con], ['announced', ann]] as const) capacity.push({ item: cell('batteries-assembly'), product: 'Battery cells', unit: 'GWh', geo: gi(g), year: 2025, cap_status: st, value: v, status: 'obs' });
  }
  const seriesChunk = {
    'series.obs': table(SERIES_SPEC['series.obs'], sRows),
    'series.index': table(SERIES_SPEC['series.index'], sIndex),
    'series.world': table(SERIES_SPEC['series.world'], seriesWorld(sFacts)),
    'series.monthly': table(SERIES_SPEC['series.monthly'], monthly),
    'series.capacity': table(SERIES_SPEC['series.capacity'], capacity),
  };

  // summary: every (item, var with a world basis, year) in both EU modes, recomputed with the client
  // derivation (the R exporter computes the real one; the parity test compares the two)
  const tmpModel = new SectorModel(metaPartial as unknown as SectorMetaChunk);
  const fc = factsChunk as unknown as SectorFactsChunk;
  const fidx = new FactsIndex(fc['facts.obs'], fc['facts.index'], fc['facts.world']);
  const tradeOf = (item: number): SectorTradeChunk | null => {
    const slug = ITEMS[item]?.slug ?? '';
    const tech = TRADE_TECH_OF[slug];
    return tech ? (tradeChunks[`sector-trade-${tech}`] as unknown as SectorTradeChunk) : null;
  };
  const summaryRows: RowObj[] = [];
  const seen = new Set<string>();
  for (const r of index) {
    const v = tmpModel.vars[r.var as number];
    const key = `${r.var}|${r.item}|${r.year}`;
    if (seen.has(key) || v.world_basis === 'none' || !['country', 'entity', 'region'].includes(v.native_level)) continue;
    seen.add(key);
    for (const eu of ['members', 'bloc'] as const) {
      const x = summaryFromFacts(tmpModel, fidx, v, r.item as number, r.year as number, eu, tradeOf(r.item as number));
      summaryRows.push({ item: r.item, var: r.var, year: r.year, eu_mode: eu, W: x.W, basis: x.basis, n: x.n, top1_geo: x.top1_geo, top1_share: x.top1_share, top3_share: x.top3_share, hhi: x.hhi, hhi_lower_bound: x.hhi_lower_bound, eff_n: x.eff_n, china: x.china, core: x.core, partner: x.partner, rest: x.rest, unattributed: x.unattributed, growth: x.growth, growth_kind: x.growth_kind });
    }
  }
  summaryRows.sort((a, z) => (a.item as number) - (z.item as number) || (a.var as number) - (z.var as number) || (a.year as number) - (z.year as number) || String(a.eu_mode).localeCompare(String(z.eu_mode)));
  metaPartial.summary = table(META_SPEC.summary, summaryRows);

  const chunks: Record<string, Record<string, unknown>> = {
    'sector-meta': metaPartial,
    'sector-facts': factsChunk,
    ...tradeChunks,
    'sector-policy': policyChunk,
    'sector-policy-text': policyText,
    'sector-series': seriesChunk,
  };
  const hex = (n: number) => (n * 2654435761 >>> 0).toString(16).padStart(8, '0').slice(0, 8);
  const files: Record<string, string> = {};
  Object.keys(chunks).forEach((name, i) => (files[name] = `${name}.${hex(i + 1)}.osb`));
  const manifest: SectorManifest = {
    schema: 'osi-sector',
    schema_version: 2,
    bundle_id: 'fixture-ts-0001',
    created: '2026-10-10T12:00:00Z',
    git_commit: null,
    generator: 'src/sector/__fixtures__/build.ts',
    phase: 1,
    strategic_bundle_id: null,
    processed_mtime_max: '2026-10-10T14:55:00Z',
    pipeline_marker: null,
    flags: { include_restricted: false, trade: true, atlas: false, policy_text: true, series: true },
    chunks: Object.keys(chunks).map((name, i) => ({
      name: name as SectorChunkName,
      file: files[name],
      bytes: 10_000 + i * 1000,
      bytes_gz: 4_000 + i * 500,
      md5: 'f'.repeat(31) + String(i % 10),
      tech: name.startsWith('sector-trade-') ? (name.slice('sector-trade-'.length) as TradeTechSlug) : null,
    })),
    headlines: [
      { id: 'supply', template: 'supply@1', values: { share: 235 / 319, value_kt: 235, total_kt: 319 }, labels: { geo: 'CHN', geo_name: 'China', item: 'lithium', item_text: 'lithium' }, source: 'IEA Critical Minerals Dataset 2026', year: '2025', href: '/sector/minerals/lithium' },
      { id: 'dependence', template: 'dependence@1', values: { share: 0.65, total_usd: 80e9 }, labels: { cell: 'batteries-assembly', cell_label: 'Batteries — Assembly' }, source: 'UN Comtrade, reconciled matrix', year: '2024', href: '/sector/batteries/assembly#trade' },
      { id: 'policy', template: 'policy@1', values: { n: 2 }, labels: {}, source: 'IEA Policies and Measures', year: '2026-2030', href: '/sector/explore/policy' },
      { id: 'prices', template: 'prices@0', values: { vol: 0.52 }, labels: { item: 'cobalt', item_text: 'cobalt' }, source: 'IMF', year: 'Dec 2025', href: '/sector/explore/prices' },
    ],
    coverage_glyph: HEADLINE_TECH_SLUGS.flatMap((t) => STAGE_SLUGS.map((s) => ({ tech: t, stage: s, country_data: coverageRows.some((r) => r.item === cell(cellSlug(t, s)) && r.status === 'country') }))),
    notes: ['Synthetic TS fixture: invented values.'],
    validation: [{ check: 'fixture', status: 'PASS', n: 1, detail: null }],
    sources: [{ path: 'data/processed/fixture.csv', mtime: '2026-10-10T12:00:00Z', bytes: 1, md5: 'e'.repeat(32) }],
    budget: { sector_gz: 50_000, combined_gz: null, limit_sector_gz: 9 * 1024 * 1024, limit_combined_gz: 26.5 * 1024 * 1024 },
    encoding: { lossy: false },
  };
  cached = { manifest, chunks, files };
  return cached;
}

export { GEOS as FIXTURE_GEOS, ITEMS as FIXTURE_ITEMS, VARS as FIXTURE_VARS, POLICY_TAGS as FIXTURE_POLICY_TAGS, FLOWS as FIXTURE_FLOWS, gi as fixtureGeo, ii as fixtureItem, vi as fixtureVar };

/** A SectorSource over a fixture (optionally failing or delaying named chunks). */
export function fixtureSource(fx: SectorFixture, opts: { fail?: Set<string>; delayMs?: number; noManifest?: boolean } = {}): SectorSource & { calls: string[] } {
  const calls: string[] = [];
  const wait = () => new Promise((r) => setTimeout(r, opts.delayMs ?? 0));
  const byFile = new Map(Object.entries(fx.files).map(([name, file]) => [file, name]));
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      await wait();
      if (opts.noManifest) throw new SectorLoadError('NO_DATA', 'No sector data: data/sector/manifest.json does not exist.');
      return JSON.parse(JSON.stringify(fx.manifest));
    },
    async chunk(file) {
      calls.push(file);
      await wait();
      const name = byFile.get(file);
      if (!name) throw new SectorLoadError('NETWORK', `${file} could not be fetched (HTTP 404).`);
      if (opts.fail?.has(name)) throw new SectorLoadError('NETWORK', `${file} could not be fetched (HTTP 500).`);
      return fx.chunks[name];
    },
  };
}
