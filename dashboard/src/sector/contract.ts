// Sector Insights data contract (spec_sector.md 8.6; frozen in phase 0 on 2026-10-10). This file is
// the interface between builder A (scripts/87_export_sector_data.R writes these objects) and builders
// B and C (the app reads them). Only the integrator edits it, and only additively; every change is
// logged with its date and reason in scratch_dash/sector/build_log.md. Pure: no IO, no React.
//
// What it defines:
//   * slugs and labels (2.3): TECH_SLUGS, STAGE_SLUGS, MINERAL_SLUGS, RESERVED_SEGMENTS, cellSlug();
//   * the vocabularies of every factor column (3.4, 3.5, 3.6, 8.5);
//   * the measure catalogue (3.2): VAR_CATALOGUE, VAR_IDS, VarId;
//   * the headline rules behind manifest.headlines (2.8, 8.8): HEADLINE_RULES;
//   * manifest.json: SectorManifest, isSectorManifest(), sectorManifestProblems();
//   * every OSB chunk object, as a runtime spec (META_SPEC, ...) from which the TypeScript types are
//     derived, so the checks and the types cannot drift apart: validateChunk(), checkChunk().
//
// Conventions of the OSB objects (spec_react 4.2; R/dashboard/dash_osb.R): a table decodes to
// { nrow, cols }; an R double column to Float64Array (NA = NaN), an R integer column to Int32Array
// (NA = NA_INT), an R logical column to Uint8Array (0 / 1, NA = 255), an R character column (fct or
// strv node) to (string | null)[]. Builder A must write the R types listed here exactly (as.integer()
// for every i32 column, as.double() for every f64 column), even when a column is all NA.
// i32 columns named in `ref` are 0-based row indices into that meta table (dims.geo, dims.items or
// dict.vars). Pipe-joined text columns ("2020|2024") hold the arrays of the dictionary YAML.

import type { Column, OsbDecoded, Table } from '../engine/codec.ts';
import { NA_BOOL, NA_INT } from '../engine/numeric.ts';

// ---- Schema ------------------------------------------------------------------------------------

export const SECTOR_SCHEMA = 'osi-sector';
export const SECTOR_SCHEMA_VERSION = 1;
/** Relative to the app's base URL. Written only by scripts/87. */
export const SECTOR_DATA_DIR = 'data/sector/';
export const SECTOR_MANIFEST_FILE = 'manifest.json';
/** File names of the chunks (8.2); scripts/87 deletes only stale files that match this pattern. */
export const SECTOR_CHUNK_FILE_RE = /^sector-[a-z-]+\.[0-9a-f]{8}\.osb$/;

// ---- Slugs and labels (2.3) --------------------------------------------------------------------

/** The 11 headline technologies, in the order the module lists them (alphabetical, as in 2.3). */
export const HEADLINE_TECH_SLUGS = [
  'batteries',
  'coal',
  'electric-grid',
  'electric-vehicles',
  'gas',
  'geothermal',
  'green-hydrogen',
  'nuclear',
  'oil',
  'solar',
  'wind',
] as const;
/**
 * Every technology slug: the headline techs, then Semiconductors (non-headline, trade only: Midstream,
 * and Upstream where the basket has it; the basket review of 2026-10-11 moved gallium, germanium and
 * indium there; dims.items lists the Semiconductors stages the build has).
 */
export const TECH_SLUGS = [...HEADLINE_TECH_SLUGS, 'semiconductors'] as const;
export type HeadlineTechSlug = (typeof HEADLINE_TECH_SLUGS)[number];
export type TechSlug = (typeof TECH_SLUGS)[number];

/** Slug -> the Strategic vocabulary (dims.techs of the Strategic bundle; the `tech` URL key). */
export const TECH_LABELS: Readonly<Record<TechSlug, string>> = {
  batteries: 'Batteries',
  coal: 'Coal',
  'electric-grid': 'Electric Grid',
  'electric-vehicles': 'Electric Vehicles',
  gas: 'Gas',
  geothermal: 'Geothermal',
  'green-hydrogen': 'Green Hydrogen',
  nuclear: 'Nuclear',
  oil: 'Oil',
  solar: 'Solar',
  wind: 'Wind',
  semiconductors: 'Semiconductors',
};

export const STAGE_SLUGS = ['upstream', 'midstream', 'downstream'] as const;
export type StageSlug = (typeof STAGE_SLUGS)[number];
export const STAGE_LABELS: Readonly<Record<StageSlug, 'Upstream' | 'Midstream' | 'Downstream'>> = {
  upstream: 'Upstream',
  midstream: 'Midstream',
  downstream: 'Downstream',
};

/**
 * Mineral slugs (axis `mineral` of dims.items). The data are the authority (a mineral without data
 * has no page); this is the expected set, and a slug outside it is a validation warning.
 */
export const MINERAL_SLUGS = [
  'lithium',
  'cobalt',
  'nickel',
  'graphite',
  'copper',
  'manganese',
  'rare-earths',
  'zinc',
  'pgms',
  'silver',
  'silicon',
  'chromium',
  'molybdenum',
] as const;
export type MineralSlug = (typeof MINERAL_SLUGS)[number];
/** Mineral slugs that are not mineral pages: /sector/minerals/<slug> redirects here. */
export const MINERAL_REDIRECTS: Readonly<Record<string, string>> = { uranium: '/sector/nuclear/upstream' };

/** Static path segments under /sector; they outrank `:tech`, so no tech slug may equal one. */
export const RESERVED_SEGMENTS = ['explore', 'concentration', 'minerals', 'sources', 'country', 'data'] as const;
export type ReservedSegment = (typeof RESERVED_SEGMENTS)[number];

/** Technologies with a `sector-trade-<tech>` chunk (8.2): the 14 headline basket cells and Semiconductors. */
export const TRADE_TECH_SLUGS = [
  'batteries',
  'coal',
  'electric-grid',
  'electric-vehicles',
  'gas',
  'nuclear',
  'oil',
  'solar',
  'wind',
  'semiconductors',
] as const satisfies readonly TechSlug[];
export type TradeTechSlug = (typeof TRADE_TECH_SLUGS)[number];
/** The two trade years of the matrix; trade.flows has one value and one status column per year. */
export const TRADE_YEARS = [2020, 2024] as const;

/** "batteries-midstream": the cell slug, the same rule as lib/format.ts stageSlug(). */
export function cellSlug(tech: TechSlug, stage: StageSlug): string {
  return `${tech}-${stage}`;
}

/** The (tech, stage) of a cell slug, or null. */
export function parseCellSlug(slug: string): { tech: TechSlug; stage: StageSlug } | null {
  for (const stage of STAGE_SLUGS) {
    const suffix = `-${stage}`;
    if (slug.endsWith(suffix)) {
      const tech = slug.slice(0, -suffix.length);
      return isTechSlug(tech) ? { tech, stage } : null;
    }
  }
  return null;
}

export const isTechSlug = (s: string): s is TechSlug => (TECH_SLUGS as readonly string[]).includes(s);
export const isStageSlug = (s: string): s is StageSlug => (STAGE_SLUGS as readonly string[]).includes(s);
export const isReservedSegment = (s: string): s is ReservedSegment => (RESERVED_SEGMENTS as readonly string[]).includes(s);
export const isTradeTechSlug = (s: string): s is TradeTechSlug => (TRADE_TECH_SLUGS as readonly string[]).includes(s);

/** The slug of a Strategic tech label ("Electric Vehicles" -> "electric-vehicles"), or null. */
export function techSlugOf(label: string): TechSlug | null {
  for (const s of TECH_SLUGS) if (TECH_LABELS[s] === label) return s;
  return null;
}

/** The slug of a stage label ("Midstream" -> "midstream"), or null. */
export function stageSlugOf(label: string): StageSlug | null {
  for (const s of STAGE_SLUGS) if (STAGE_LABELS[s] === label) return s;
  return null;
}

// ---- Themes, families, profile sections (3.2, 4.3) ---------------------------------------------

export const THEMES = [
  { slug: 'trade', label: 'Trade' },
  { slug: 'resources', label: 'Reserves & resources' },
  { slug: 'production', label: 'Production' },
  { slug: 'consumption', label: 'Consumption & dependence' },
  { slug: 'investment', label: 'Investment' },
  { slug: 'manufacturing', label: 'Manufacturing capacity' },
  { slug: 'prices', label: 'Prices & costs' },
  { slug: 'technology', label: 'Technology & outlook' },
  { slug: 'policy', label: 'Policy' },
  { slug: 'development', label: 'Development & finance' },
] as const;
export const THEME_SLUGS = THEMES.map((t) => t.slug) as readonly ThemeSlug[];
export type ThemeSlug = (typeof THEMES)[number]['slug'];

/** Profile section anchors, in rendering order (4.3). */
export const SECTION_IDS = [
  'snapshot',
  'who',
  'leaders',
  'trade',
  'investment',
  'production',
  'resources',
  'demand',
  'prices',
  'policy',
  'technology',
  'context',
  'countries',
] as const;
export type SectionId = (typeof SECTION_IDS)[number];

/** Measure families (3.2). A chart never mixes families. */
export const FAMILY_IDS = [
  'exports',
  'production',
  'capacity',
  'reserves',
  'investment',
  'consumption',
  'prices',
  'policy',
  'technology',
  'context',
  'development',
] as const;
export type FamilyId = (typeof FAMILY_IDS)[number];

/** The profile section of each family (3.2); development appears on explore and country pages only. */
export const FAMILY_SECTION: Readonly<Record<FamilyId, SectionId | null>> = {
  exports: 'trade',
  production: 'production',
  capacity: 'production',
  reserves: 'resources',
  investment: 'investment',
  consumption: 'demand',
  prices: 'prices',
  policy: 'policy',
  technology: 'technology',
  context: 'context',
  development: null,
};

// ---- Vocabularies of the factor columns --------------------------------------------------------

/** Value states shipped per fact row (3.5). "Not covered", "regional" and "old" are derived on the client. */
export const STATUS_CODES = ['obs', 'zero', 'nsr', 'nq', 'mirror', 'est', 'na_growth'] as const;
export type StatusCode = (typeof STATUS_CODES)[number];
/**
 * States whose value may be NaN by design; every other state carries a finite value. nsr ("not
 * reported separately") may carry the source's zero-fill (EI generation; fuel production in a table
 * built before the EIA fill), a value derived from an assumed zero, or NaN: the EI series of
 * successor states before they exist, and fuel production neither EI nor EIA reports (integrator
 * change 2026-10-11, additive). Since the user rule of 2026-10-11 (sector-data track) the balance and
 * self-sufficiency of a fuel whose production neither EI nor EIA reports carry the value production
 * = 0 gives (minus consumption; 0%) with status nsr and the facts.notes id production_assumed_zero,
 * instead of NaN: show the value (it is negative for the balance) and say it assumes zero production.
 */
export const NAN_STATUS_CODES = ['nq', 'na_growth', 'nsr'] as const satisfies readonly StatusCode[];

export const GEO_KINDS = ['country', 'eu', 'world', 'region', 'entity', 'residual'] as const;
export type GeoKind = (typeof GEO_KINDS)[number];
export const TIERS = ['core', 'partner', 'none'] as const;
export type Tier = (typeof TIERS)[number];
/**
 * dims.geo `group` (3.4): `na` for the world and residual rows, and for a region whose members span
 * groups (the IEA EV regions North America, Other Asia Pacific and Rest of World; integrator change
 * 2026-10-11): such a region is a holder, but its share counts as unattributed in the group split.
 */
export const GEO_GROUPS = ['china', 'core', 'partner', 'rest', 'na'] as const;
export type GeoGroup = (typeof GEO_GROUPS)[number];
/** The parts of a share split, in the fixed stacking order (3.6, 5.3 ShareSplitChart). */
export const SPLIT_PARTS = ['china', 'core', 'partner', 'rest', 'unattributed'] as const;
export type SplitPart = (typeof SPLIT_PARTS)[number];

/**
 * Item axes. `indicator`: the World Bank WDI series of development.wdi; `aid_sector`: the OECD CRS
 * sectors of the aid measures (all sectors, energy, industry, mining), linked to the cells of the
 * stage the Development Potential builder maps them to (development data change 2026-10-15, additive).
 */
export const ITEM_AXES = ['cell', 'mineral', 'fuel', 'commodity', 'activity', 'policy_tag', 'indicator', 'aid_sector', 'none'] as const;
export type ItemAxis = (typeof ITEM_AXES)[number];
export const NATIVE_LEVELS = ['country', 'eu', 'region', 'entity', 'world', 'cell_global', 'dyad'] as const;
export type NativeLevel = (typeof NATIVE_LEVELS)[number];
export const AGG_RULES = ['sum', 'ratio', 'none'] as const;
export const WORLD_BASES = ['source_world', 'sum_all', 'one', 'sum_listed', 'none'] as const;
export type WorldBasis = (typeof WORLD_BASES)[number];
export const EU_MODES = ['members', 'bloc'] as const;
export type EuMode = (typeof EU_MODES)[number];
/** Licence classes on the public build; `restricted` (BNEF, BCG) never ships (user decision 2026-10-10). */
export const LICENCES = ['open', 'attribution'] as const;
export const PROVENANCES = ['P', 'D', 'X'] as const;
export const PHASES = ['1', '2', 'hold'] as const;
export const GROWTH_KINDS = [
  'pct_2020_2024',
  'cagr_2018_2025',
  'cagr_2022_2025',
  'cagr_2015_2025',
  'pct_2024_2025',
  'cagr_2013_2023',
] as const;
export const COVERAGE_STATUSES = ['country', 'region', 'global', 'licensed', 'none'] as const;
/** Reason codes of coverage chips (4.3 S0); open vocabulary (unknown codes warn). */
export const COVERAGE_REASONS = ['no_basket', 'index_only', 'not_applicable', 'no_source', 'licensed', 'mineral_level', 'thin'] as const;
export const TRADE_FLOW_STATUSES = ['importer', 'mirror', 'none'] as const;
export const POLICY_TECH_SOURCES = ['iea', 'text'] as const;
/** Bit i of policy.pams `stage_mask` is STAGE_LABELS[STAGE_SLUGS[i]]. */
export const STAGE_MASK_ORDER = STAGE_SLUGS;
/** Chunk families a measure's data can live in (`fdi`: sector-fdi, integrator change 2026-10-11, additive). */
export const CHUNK_KINDS = ['meta', 'facts', 'trade', 'policy', 'series', 'atlas', 'dev', 'fdi'] as const;
export type ChunkKind = (typeof CHUNK_KINDS)[number];
/**
 * Number formats by unit kind (3.9; src/sector/format.ts). Open vocabulary: an unknown kind is a
 * validation warning and formats as a plain number.
 */
export const UNIT_KINDS = [
  'usd',
  'usd_bn_real',
  'usd_m',
  'usd_month',
  'share',
  'pct',
  'hhi',
  'kbd',
  'ej',
  'pj',
  'twh',
  'gw',
  'gwh',
  'mw',
  'kt',
  't',
  'tu',
  'kg',
  'gj_pc',
  'price',
  'count',
  'vehicles',
  'mvehicles',
  'ratio',
  'trl',
  'years',
  'index',
  'ordinal',
  'kwh_kwp_day',
  'w_m2',
  'potential',
  'lcoe',
  'number',
  // a bounded 0-1 index printed with two decimals (trade.feasibility; sector_data change 2026-10-12, additive)
  'index01',
] as const;

// ---- Measure catalogue (3.2) -------------------------------------------------------------------

/**
 * One row per measure id. `phase` is the spec's (1, 2 or hold); `chunks` says where its values ship;
 * `scope` is this build's (user decision 2026-10-10: phase 1 plus the sector-series chunk):
 * `build` = shipped now, `later` = a later phase (ids reserved), `hold` = held until a unit is
 * confirmed. Ids are `theme.name`; the dictionary's `theme` must equal the prefix.
 *
 * Deviations from the 3.2 table, decided in phase 0 so that every id has one quantity and one unit:
 *   consumption.ev -> ev_sales, ev_stock, ev_sales_share, ev_sales_growth, ev_stock_growth;
 *   production.ev_region -> production.ev_region (production) + production.ev_region_sales;
 *   prices.vol_1y/5y/10y/20y, resources.geo_lcoe_500/_5000, technology.trl/_change spelled out;
 *   consumption.mineral_demand added (IEA CM demand by technology, the caption of dims.mineral_demand);
 *   development.wdi_* is the template type WdiVarId;
 *   resources.wind_share ships in phase 1 (spec: hold) with its unit labelled "unit to confirm", as
 *   do the geothermal LCOE currency and the unverified IMF units (user decision 2026-10-10).
 * Restricted measures (BNEF, BCG) have no ids: they are never built.
 */
export const VAR_CATALOGUE = [
  // trade
  { id: 'trade.exports', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.imports', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.net', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.flow', phase: '1', level: 'dyad', chunks: ['trade'], scope: 'build' },
  // trade.rca rows with status nq (value NaN) are "too small to compare": exporter-reported basket
  // exports below the dictionary min_base (USD 50 m; caveat rca_small).
  { id: 'trade.rca', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.atlas_share', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  // Export feasibility (Harvard Atlas relatedness: 1 - mean product-space distance over the basket's
  // HS4 headings), country x cell, a 0-1 index (unit kind index01). sector_data change 2026-10-12.
  { id: 'trade.feasibility', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.atlas_exports', phase: '2', level: 'country', chunks: ['atlas'], scope: 'later' },
  // Trade by sub-component (trade.cty_sub of the trade chunks; never in facts): exports and imports of
  // the HS6 codes of one sub-component of a cell, on the cell's reconciled basis, so a cell's
  // sub-components add up to its trade.exports / trade.imports. sector_data change 2026-10-12: scope
  // later -> build, and trade.sub_imports added.
  { id: 'trade.sub_exports', phase: '2', level: 'country', chunks: ['trade'], scope: 'build' },
  { id: 'trade.sub_imports', phase: '2', level: 'country', chunks: ['trade'], scope: 'build' },
  { id: 'trade.critmin_exports', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'trade.critmin_dependence', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  // resources
  { id: 'resources.fuel_reserves', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.mineral_reserves', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.uranium_resources', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.pv_quality', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.pv_area', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.wind_density', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.wind_area', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.wind_share', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.geo_potential', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.geo_lcoe_500', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.geo_lcoe_5000', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'resources.rp_years', phase: '2', level: 'country', chunks: ['facts'], scope: 'later' },
  // production
  { id: 'production.fuels', phase: '1', level: 'country', chunks: ['facts', 'series'], scope: 'build' },
  { id: 'production.generation', phase: '1', level: 'country', chunks: ['facts', 'series'], scope: 'build' },
  { id: 'production.mining', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'production.refining', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'production.mining_ei', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'production.mine_usgs', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'production.uranium', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'production.ev_region', phase: '1', level: 'region', chunks: ['facts', 'series'], scope: 'build' },
  { id: 'production.ev_region_sales', phase: '1', level: 'region', chunks: ['facts', 'series'], scope: 'build' },
  // manufacturing
  { id: 'manufacturing.share_now', phase: '1', level: 'entity', chunks: ['facts'], scope: 'build' },
  { id: 'manufacturing.share_2035', phase: '1', level: 'entity', chunks: ['facts'], scope: 'build' },
  { id: 'manufacturing.ev_net_export_ratio', phase: '1', level: 'region', chunks: ['facts'], scope: 'build' },
  { id: 'manufacturing.capacity', phase: '2', level: 'country', chunks: ['series'], scope: 'build' },
  // investment
  { id: 'investment.annual', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'investment.power_additions', phase: '2', level: 'country', chunks: ['series'], scope: 'build' },
  // CM-FDI cross-border projects (sector-fdi; integrator change 2026-10-11, additive): projects and
  // their announced capital (home -> destination dyads) and the destination's split by the parent's
  // headquarters group. Item = the Midstream cell of the project's technology. Not in facts.
  { id: 'investment.fdi_projects', phase: '1', level: 'dyad', chunks: ['fdi'], scope: 'build' },
  { id: 'investment.fdi_value', phase: '1', level: 'dyad', chunks: ['fdi'], scope: 'build' },
  { id: 'investment.fdi_parent_share', phase: '1', level: 'country', chunks: ['fdi'], scope: 'build' },
  // consumption
  { id: 'consumption.per_capita', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.pc_change', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.balance', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.self_sufficiency', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.ev_sales', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.ev_stock', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.ev_sales_share', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.ev_sales_growth', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.ev_stock_growth', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.weo', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'consumption.mineral_demand', phase: '1', level: 'world', chunks: ['meta'], scope: 'build' },
  { id: 'consumption.total_ej', phase: '2', level: 'country', chunks: ['series'], scope: 'build' },
  // prices
  { id: 'prices.latest', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.yoy', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.vol_1y', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.vol_5y', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.vol_10y', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.vol_20y', phase: '1', level: 'world', chunks: ['facts'], scope: 'build' },
  { id: 'prices.monthly', phase: '2', level: 'world', chunks: ['series'], scope: 'build' },
  { id: 'prices.wages', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'prices.lending_rate', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'prices.ppi_yoy', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'prices.equipment_cost', phase: '2', level: 'region', chunks: ['series'], scope: 'later' },
  // technology
  { id: 'technology.trl', phase: '1', level: 'cell_global', chunks: ['facts'], scope: 'build' },
  { id: 'technology.trl_change', phase: '1', level: 'cell_global', chunks: ['facts'], scope: 'build' },
  { id: 'technology.trl_items', phase: '2', level: 'cell_global', chunks: ['series'], scope: 'later' },
  // policy
  { id: 'policy.records', phase: '1', level: 'country', chunks: ['policy'], scope: 'build' },
  { id: 'policy.cat', phase: '1', level: 'country', chunks: ['policy'], scope: 'build' },
  // development
  { id: 'development.alignment', phase: '1', level: 'country', chunks: ['facts'], scope: 'build' },
  // The raw data behind Development Potential and Friendshore's outbound component (development data
  // change 2026-10-15): World Bank WDI series (item axis indicator) and the Doing Business score, per
  // economy; OECD CRS official development assistance received and given per aid sector and year, and
  // the builder's 2013-2024 total per stage (and per GDP); the donor -> recipient flows (sector-dev
  // dev.oda) and the IMF bilateral direct-investment positions, conduit centres flagged (dev.dip).
  { id: 'development.wdi', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.doing_business', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.oda_received', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.oda_given', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.oda_received_total', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.oda_received_gdp', phase: '2', level: 'country', chunks: ['facts'], scope: 'build' },
  { id: 'development.gdp', phase: '2', level: 'country', chunks: ['dev'], scope: 'later' },
  { id: 'development.oda', phase: '2', level: 'dyad', chunks: ['dev'], scope: 'build' },
  { id: 'development.fdi_out', phase: '2', level: 'dyad', chunks: ['dev'], scope: 'build' },
  { id: 'development.ideal_series', phase: '2', level: 'country', chunks: ['dev'], scope: 'later' },
] as const satisfies readonly VarEntry[];

export interface VarEntry {
  id: string;
  phase: (typeof PHASES)[number];
  level: NativeLevel;
  chunks: readonly ChunkKind[];
  scope: 'build' | 'later' | 'hold';
}

/** The ~12 WDI context series of phase 2 (`development.wdi_<name>`). */
export type WdiVarId = `development.wdi_${string}`;
export type VarId = (typeof VAR_CATALOGUE)[number]['id'] | WdiVarId;
export const VAR_IDS: readonly VarId[] = VAR_CATALOGUE.map((v) => v.id);
/** Ids shipped by this build (phase 1 + the series chunk). */
export const BUILD_VAR_IDS: readonly VarId[] = VAR_CATALOGUE.filter((v) => v.scope === 'build').map((v) => v.id);
/** Ids whose over-time values ship in sector-series (series.obs, series.monthly or series.capacity). */
export const SERIES_VAR_IDS: readonly VarId[] = VAR_CATALOGUE.filter((v) => (v.chunks as readonly ChunkKind[]).includes('series') && v.scope === 'build').map((v) => v.id);

const WDI_RE = /^development\.wdi_[a-z0-9_]+$/;
export function isVarId(s: string): s is VarId {
  return (VAR_IDS as readonly string[]).includes(s) || WDI_RE.test(s);
}

/** The theme of a var id ("trade.exports" -> "trade"). */
export function themeOfVar(id: string): string {
  const i = id.indexOf('.');
  return i < 0 ? '' : id.slice(0, i);
}

// ---- Headline rules (2.8, 8.8) -----------------------------------------------------------------

/**
 * The fixed rules scripts/87 (`sector_headlines()`) computes into manifest.headlines. A manifest
 * headline is used only when its `id` is a key here and its `template` equals `rule` (the rule
 * version); otherwise the app omits the fact. `values` are numbers in the units given here, `labels`
 * strings; the exporter writes exactly these keys (no more, no fewer) and omits a headline whose rule
 * finds no data. The wording lives in src/content/questions.ts; `href` points at the item the rule
 * found and wins over the question's fallback link.
 */
export const HEADLINE_RULES = {
  supply: {
    rule: 'supply@1',
    doc: 'Among the IEA Critical Minerals refining blocks (production.refining, 2025, base case), the mineral whose top-1 economy has the largest share of the IEA world total (the "Total" row). href: /sector/minerals/<mineral>.',
    values: { share: 'share 0-1', value_kt: 'kt, the top-1 output', total_kt: 'kt, the IEA total' },
    labels: { geo: 'ISO3 of the top-1 economy', geo_name: 'its display name ("China")', item: 'mineral slug', item_text: 'running-text name ("battery-grade graphite")' },
  },
  concentration: {
    rule: 'concentration@1',
    doc: 'meta.summary rows with eu_mode members, latest year, item axis cell and var = the cell\'s primary var of each family with country data: the number with top1_share >= 0.5, of all such rows. href: /sector/concentration?lens=top1.',
    values: { n: 'count', n_total: 'count' },
    labels: {},
  },
  dependence: {
    rule: 'dependence@1',
    doc: 'The headline Midstream trade cell with the largest 2024 matrix world total (trade.exports W; Semiconductors excluded): China\'s share of its 2024 exports. href: /sector/<tech>/midstream#trade.',
    values: { share: 'share 0-1', total_usd: 'USD, the cell\'s 2024 world total' },
    labels: { cell: 'cell slug', cell_label: 'display label ("Electric Vehicles — Midstream")' },
  },
  investment: {
    rule: 'investment@1',
    doc: 'investment.annual, 2025, summed over every CIM cell and economy (includes estimated Q4); China\'s share and the coalition\'s (core + partner members). href: /sector/explore/investment.',
    values: { total_bn: 'USD bn (2024$)', china: 'share 0-1', coalition: 'share 0-1' },
    labels: {},
  },
  reserves: {
    rule: 'reserves@1',
    doc: 'Among resources.mineral_reserves (USGS MCS 2026, end-2025), the mineral whose top-1 holder has the largest share of the USGS world total. href: /sector/explore/resources?m=resources.mineral_reserves&item=<mineral>.',
    values: { share: 'share 0-1' },
    labels: { geo: 'ISO3', geo_name: 'display name', item: 'mineral slug', item_text: 'running-text name ("platinum-group metals")' },
  },
  energy: {
    rule: 'energy@1',
    doc: 'consumption.balance, item gas, 2025, countries only (EU members as members): the largest positive balance (net exporter) and the most negative (net importer); both magnitudes positive. href: /sector/explore/consumption?m=consumption.balance&item=gas.',
    values: { exporter_ej: 'EJ, positive', importer_ej: 'EJ, positive (the magnitude of the deficit)' },
    labels: { exporter: 'ISO3', exporter_name: 'display name', importer: 'ISO3', importer_name: 'display name' },
  },
  policy: {
    rule: 'policy@1',
    doc: 'Distinct IEA PAMS records (policy.records, before expand_cross_cutting; a record listed for several jurisdictions counts once) with at least one technology tag, not cross-cutting only, whose IEA PAMS year (in force or announced; not always the promulgation date) is 2026-2030. href: /sector/explore/policy.',
    values: { n: 'count' },
    labels: {},
  },
  prices: {
    rule: 'prices@1',
    doc: 'The commodity with the largest prices.vol_5y (annualised SD of monthly log returns, a fraction per year). href: /sector/explore/prices.',
    values: { vol: 'fraction per year' },
    labels: { item: 'commodity slug', item_text: 'running-text name ("cobalt")' },
  },
} as const;
export type HeadlineId = keyof typeof HEADLINE_RULES;
export const HEADLINE_IDS = Object.keys(HEADLINE_RULES) as HeadlineId[];

// ---- manifest.json -----------------------------------------------------------------------------

export interface SectorChunkInfo {
  name: SectorChunkName;
  /** "sector-meta.<h8>.osb", relative to SECTOR_DATA_DIR */
  file: string;
  bytes: number;
  bytes_gz: number;
  md5: string;
  /** the trade tech slug of a sector-trade-<tech> chunk, else null */
  tech: TradeTechSlug | null;
}

export interface SectorHeadline {
  id: string;
  /** the rule version (HEADLINE_RULES[id].rule), not wording */
  template: string;
  values: Record<string, number>;
  labels: Record<string, string>;
  /** source short name and edition, for the muted line ("IEA Critical Minerals Dataset 2026") */
  source: string;
  /** data year as text ("2025", "end-2025") */
  year: string;
  /** an in-app path under /sector */
  href: string;
}

/** manifest.json (schema "osi-sector", schema_version 1; 8.6). Unknown extra keys are allowed. */
export interface SectorManifest {
  schema: 'osi-sector';
  schema_version: 1;
  bundle_id: string;
  created: string;
  git_commit: string | null;
  generator: string;
  /** 1 for this build (phase 1 + series, flagged by flags.series); 2 when atlas, dev and the country snapshot ship */
  phase: 1 | 2;
  strategic_bundle_id: string | null;
  processed_mtime_max: string;
  pipeline_marker: string | null;
  flags: {
    /** always false: BNEF and BCG never ship */
    include_restricted: false;
    trade: boolean;
    atlas: boolean;
    policy_text: boolean;
    /** the sector-series chunk is present (added in phase 0 for the time-series scope) */
    series: boolean;
    /**
     * the sector-fdi chunk (CM-FDI) is present. Optional (integrator change 2026-10-11, additive): a
     * manifest without the key has no sector-fdi chunk.
     */
    fdi?: boolean;
    /**
     * the sector-dev chunk (CRS aid flows, IMF DIP positions) is present. Optional (development data
     * change 2026-10-15, additive): a manifest without the key has no sector-dev chunk.
     */
    dev?: boolean;
    /**
     * the sector-climate chunk (Climate TRACE sector emissions, IPCC lifecycle intensity) is present.
     * Optional (climate data change 2026-10-17, additive): a manifest without the key has none.
     */
    climate?: boolean;
  };
  chunks: SectorChunkInfo[];
  headlines: SectorHeadline[];
  /** 33 rows, one per headline cell */
  coverage_glyph: { tech: HeadlineTechSlug; stage: StageSlug; country_data: boolean }[];
  notes: string[];
  validation: { check: string; status: 'PASS' | 'WARN'; n: number; detail: string | null }[];
  /** inputs; `path` relative to the repository root (never absolute) */
  sources: { path: string; mtime: string; bytes: number; md5: string }[];
  budget: { sector_gz: number; combined_gz: number | null; limit_sector_gz: number; limit_combined_gz: number };
  encoding: { lossy: false };
}

/** Per-chunk gzip budgets in bytes (8.3) and the totals (11). */
export const SECTOR_BUDGET_GZ = {
  'sector-meta': 150 * 1024,
  'sector-facts': 600 * 1024,
  'sector-trade': 450 * 1024,
  'sector-policy': 250 * 1024,
  'sector-policy-text': 700 * 1024,
  'sector-series': 900 * 1024,
  'sector-atlas': 1.3 * 1024 * 1024,
  'sector-dev': 1.2 * 1024 * 1024,
  /** CM-FDI projects, sources and aggregates (integrator change 2026-10-11, additive) */
  'sector-fdi': 300 * 1024,
  /** Climate TRACE emissions mapped to technology x stage (climate data change 2026-10-17, additive) */
  'sector-climate': 300 * 1024,
  sector_total: 9 * 1024 * 1024,
  combined_total: 25 * 1024 * 1024,
} as const;

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string';
const isStrOrNull = (x: unknown): boolean => x === null || typeof x === 'string';
const isNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);
const isAbsolutePath = (p: string): boolean => /^([a-zA-Z]:|[\\/]|~)/.test(p);
const MD5_RE = /^[0-9a-f]{32}$/;

/** Every way `x` falls short of SectorManifest (empty when it conforms). */
export function sectorManifestProblems(x: unknown): string[] {
  const p: string[] = [];
  if (!isObj(x)) return ['manifest.json is not an object'];
  if (x.schema !== SECTOR_SCHEMA) p.push(`schema is ${JSON.stringify(x.schema)}, expected "${SECTOR_SCHEMA}"`);
  if (x.schema_version !== SECTOR_SCHEMA_VERSION) {
    p.push(
      isNum(x.schema_version) && x.schema_version > SECTOR_SCHEMA_VERSION
        ? 'this sector data build needs a newer app'
        : `schema_version is ${JSON.stringify(x.schema_version)}, expected ${SECTOR_SCHEMA_VERSION}`,
    );
  }
  for (const k of ['bundle_id', 'created', 'generator', 'processed_mtime_max']) if (!isStr(x[k]) || !x[k]) p.push(`${k} must be a non-empty string`);
  for (const k of ['git_commit', 'strategic_bundle_id', 'pipeline_marker']) if (!isStrOrNull(x[k])) p.push(`${k} must be a string or null`);
  if (x.phase !== 1 && x.phase !== 2) p.push('phase must be 1 or 2');

  const f = x.flags;
  if (!isObj(f)) p.push('flags must be an object');
  else {
    if (f.include_restricted !== false) p.push('flags.include_restricted must be false (restricted data never ship)');
    for (const k of ['trade', 'atlas', 'policy_text', 'series']) if (typeof f[k] !== 'boolean') p.push(`flags.${k} must be a boolean`);
    if (f.fdi !== undefined && typeof f.fdi !== 'boolean') p.push('flags.fdi must be a boolean when present');
    if (f.dev !== undefined && typeof f.dev !== 'boolean') p.push('flags.dev must be a boolean when present');
    if (f.climate !== undefined && typeof f.climate !== 'boolean') p.push('flags.climate must be a boolean when present');
  }

  if (!Array.isArray(x.chunks)) p.push('chunks must be an array');
  else {
    const names = new Set<string>();
    x.chunks.forEach((c, i) => {
      if (!isObj(c)) return void p.push(`chunks[${i}] is not an object`);
      const name = c.name;
      if (!isStr(name) || !isSectorChunkName(name)) p.push(`chunks[${i}].name ${JSON.stringify(name)} is not a sector chunk name`);
      else {
        if (names.has(name)) p.push(`chunk ${name} is listed twice`);
        names.add(name);
        const tech = name.startsWith('sector-trade-') ? name.slice('sector-trade-'.length) : null;
        if (c.tech !== tech) p.push(`chunks[${i}].tech must be ${JSON.stringify(tech)}`);
        if (isStr(c.file) && !c.file.startsWith(`${name}.`)) p.push(`chunks[${i}].file ${c.file} does not belong to ${name}`);
      }
      if (!isStr(c.file) || !SECTOR_CHUNK_FILE_RE.test(c.file)) p.push(`chunks[${i}].file ${JSON.stringify(c.file)} does not match sector-<name>.<h8>.osb`);
      if (!isNum(c.bytes) || !isNum(c.bytes_gz)) p.push(`chunks[${i}] needs numeric bytes and bytes_gz`);
      if (!isStr(c.md5) || !MD5_RE.test(c.md5)) p.push(`chunks[${i}].md5 is not an md5 hex digest`);
    });
    for (const req of ['sector-meta', 'sector-facts', 'sector-policy'] as const) if (!names.has(req)) p.push(`chunk ${req} is missing`);
    if (isObj(f)) {
      if (f.series === true && !names.has('sector-series')) p.push('flags.series is true but sector-series is missing');
      if (f.policy_text === true && !names.has('sector-policy-text')) p.push('flags.policy_text is true but sector-policy-text is missing');
      if (f.atlas === true && !names.has('sector-atlas')) p.push('flags.atlas is true but sector-atlas is missing');
      if (f.trade === true && ![...names].some((n) => n.startsWith('sector-trade-'))) p.push('flags.trade is true but no sector-trade-<tech> chunk is listed');
      if (f.fdi === true && !names.has('sector-fdi')) p.push('flags.fdi is true but sector-fdi is missing');
      if (f.fdi !== true && names.has('sector-fdi')) p.push('sector-fdi is listed but flags.fdi is not true');
      if (f.dev === true && !names.has('sector-dev')) p.push('flags.dev is true but sector-dev is missing');
      if (f.dev !== true && names.has('sector-dev')) p.push('sector-dev is listed but flags.dev is not true');
      if (f.climate === true && !names.has('sector-climate')) p.push('flags.climate is true but sector-climate is missing');
      if (f.climate !== true && names.has('sector-climate')) p.push('sector-climate is listed but flags.climate is not true');
    }
  }

  if (!Array.isArray(x.headlines)) p.push('headlines must be an array');
  else {
    x.headlines.forEach((h, i) => {
      if (!isObj(h)) return void p.push(`headlines[${i}] is not an object`);
      for (const k of ['id', 'template', 'source', 'year', 'href']) if (!isStr(h[k])) p.push(`headlines[${i}].${k} must be a string`);
      if (isStr(h.href) && !h.href.startsWith('/sector')) p.push(`headlines[${i}].href must be a /sector path`);
      if (!isObj(h.values) || !Object.values(h.values).every(isNum)) p.push(`headlines[${i}].values must map keys to finite numbers`);
      if (!isObj(h.labels) || !Object.values(h.labels).every(isStr)) p.push(`headlines[${i}].labels must map keys to strings`);
    });
  }

  if (!Array.isArray(x.coverage_glyph)) p.push('coverage_glyph must be an array');
  else {
    if (x.coverage_glyph.length !== HEADLINE_TECH_SLUGS.length * STAGE_SLUGS.length) {
      p.push(`coverage_glyph has ${x.coverage_glyph.length} rows, expected ${HEADLINE_TECH_SLUGS.length * STAGE_SLUGS.length}`);
    }
    x.coverage_glyph.forEach((g, i) => {
      if (!isObj(g) || !isStr(g.tech) || !(HEADLINE_TECH_SLUGS as readonly string[]).includes(g.tech) || !isStr(g.stage) || !isStageSlug(g.stage) || typeof g.country_data !== 'boolean') {
        p.push(`coverage_glyph[${i}] must be { tech: headline tech slug, stage: stage slug, country_data: boolean }`);
      }
    });
  }

  if (!Array.isArray(x.notes) || !x.notes.every(isStr)) p.push('notes must be an array of strings');
  if (!Array.isArray(x.validation)) p.push('validation must be an array');
  else {
    x.validation.forEach((v, i) => {
      if (!isObj(v) || !isStr(v.check) || (v.status !== 'PASS' && v.status !== 'WARN') || !isNum(v.n) || !isStrOrNull(v.detail)) {
        p.push(`validation[${i}] must be { check, status: PASS | WARN, n, detail }`);
      }
    });
  }
  if (!Array.isArray(x.sources)) p.push('sources must be an array');
  else {
    x.sources.forEach((s, i) => {
      if (!isObj(s) || !isStr(s.path) || !isStr(s.mtime) || !isNum(s.bytes) || !isStr(s.md5)) p.push(`sources[${i}] must be { path, mtime, bytes, md5 }`);
      else if (isAbsolutePath(s.path)) p.push(`sources[${i}].path must be relative to the repository root`);
    });
  }
  const b = x.budget;
  if (!isObj(b) || !isNum(b.sector_gz) || !(b.combined_gz === null || isNum(b.combined_gz)) || !isNum(b.limit_sector_gz) || !isNum(b.limit_combined_gz)) {
    p.push('budget must be { sector_gz, combined_gz | null, limit_sector_gz, limit_combined_gz }');
  }
  if (!isObj(x.encoding) || x.encoding.lossy !== false) p.push('encoding.lossy must be false');
  return p;
}

export function isSectorManifest(x: unknown): x is SectorManifest {
  return sectorManifestProblems(x).length === 0;
}

/** The manifest entry of a chunk, or null. */
export function chunkInfo(manifest: SectorManifest, name: SectorChunkName): SectorChunkInfo | null {
  return manifest.chunks.find((c) => c.name === name) ?? null;
}

// ---- Chunk object specs ------------------------------------------------------------------------

type ColKind = 'f64' | 'i32' | 'bool' | 'str';
type RefTable = 'dims.geo' | 'dims.items' | 'dict.vars';

interface ColSpec {
  readonly t: ColKind;
  /** no NA allowed */
  readonly req?: boolean;
  /** i32: a 0-based row index into this meta table (NA allowed unless req) */
  readonly ref?: RefTable;
  /** str: the allowed values (NA allowed unless req) */
  readonly values?: readonly string[];
  /** values outside `values` only warn (an open vocabulary) */
  readonly soft?: boolean;
}

interface TableSpec {
  readonly kind: 'table';
  readonly cols: Readonly<Record<string, ColSpec>>;
  /** rows are non-decreasing in these i32 columns, compared left to right */
  readonly sorted?: readonly string[];
  /** no two rows share the values of these columns */
  readonly unique?: readonly string[];
  readonly optional?: boolean;
}

interface StrvSpec {
  readonly kind: 'strv';
  readonly optional?: boolean;
}

type NodeSpec = TableSpec | StrvSpec;
type ChunkSpec = Readonly<Record<string, NodeSpec>>;

/** A character column. */
export type Str = (string | null)[];
type ColOf<C> = C extends { t: 'f64' } ? Float64Array : C extends { t: 'i32' } ? Int32Array : C extends { t: 'bool' } ? Uint8Array : Str;
/** A decoded table with typed columns (assignable to engine Table). */
export type TableOf<C> = { nrow: number; cols: { -readonly [K in keyof C]: ColOf<C[K]> } };
type NodeOf<N> = N extends { kind: 'table'; cols: infer C } ? TableOf<C> : Str;
type ChunkOf<S> = { -readonly [P in keyof S as S[P] extends { optional: true } ? never : P]: NodeOf<S[P]> } & {
  -readonly [P in keyof S as S[P] extends { optional: true } ? P : never]?: NodeOf<S[P]>;
};

// shorthands
const I = { t: 'i32' } as const;
const IR = { t: 'i32', req: true } as const;
const F = { t: 'f64' } as const;
const FR = { t: 'f64', req: true } as const;
const B = { t: 'bool' } as const;
const BR = { t: 'bool', req: true } as const;
const S = { t: 'str' } as const;
const SR = { t: 'str', req: true } as const;
const GEO = { t: 'i32', req: true, ref: 'dims.geo' } as const;
const ITEM = { t: 'i32', req: true, ref: 'dims.items' } as const;
const VAR = { t: 'i32', req: true, ref: 'dict.vars' } as const;
const STATUS = { t: 'str', req: true, values: STATUS_CODES } as const;

/** facts.obs / series.obs (one fact per row). */
const OBS = {
  kind: 'table',
  cols: { var: VAR, item: ITEM, geo: GEO, year: IR, value: F, status: STATUS, obs_year: I, obs_from: I },
  sorted: ['var', 'item', 'year', 'geo'],
  unique: ['var', 'item', 'year', 'geo'],
} as const satisfies TableSpec;
/** facts.index / series.index: obs rows [start, start + n) share (var, item, year); the slices tile obs in order. */
const INDEX = {
  kind: 'table',
  cols: { var: VAR, item: ITEM, year: IR, start: IR, n: IR },
  sorted: ['var', 'item', 'year'],
  unique: ['var', 'item', 'year'],
} as const satisfies TableSpec;
/** facts.world / series.world: the world total W of (var, item, year) and its basis (3.6). */
const WORLD = {
  kind: 'table',
  cols: { var: VAR, item: ITEM, year: IR, value: FR, basis: { t: 'str', req: true, values: WORLD_BASES } },
  sorted: ['var', 'item', 'year'],
  unique: ['var', 'item', 'year'],
} as const satisfies TableSpec;

/**
 * sector-meta: dims, dictionary, rosters, coverage, availability, summary, reconcile.
 *
 * dims.geo: one row per economy or aggregate. `code` is an ISO3 (kind country), "EUU" (eu), "WLD"
 *   (world), "R:<src>:<id>" (region), "E:<src>:<id>" (entity) or "RES:<src>" (a source's residual,
 *   "Rest of world"). World-native and cell_global facts use the WLD row. `group` follows 3.4
 *   (entities and regions get the group of what they stand for, e.g. IEA "China" -> china, "EU" ->
 *   core; residuals and WLD -> na). `reason` is the coalition reason sentence (NA outside it).
 *   Rows of kind `residual` (RES:<src>, e.g. EI "Other" = WLD minus the listed economies) appear only
 *   in series.obs and are never holders of a share (3.6).
 * dims.items: one row per item; (axis, slug) is unique. Cell rows: slug = cellSlug(), tech / stage =
 *   the Strategic labels, headline = FALSE only for Semiconductors; other axes: tech / stage NA.
 *   Exactly one row has axis "none": the item of measures with item_axis none.
 * dims.mineral_demand: IEA CM demand (kt) by mineral, technology (`tech` = the Strategic tech label,
 *   as in dims.items.tech, or NA for IEA categories outside the techs and for totals; `label` =
 *   IEA's own label, e.g. "Total clean technologies"), scenario ("observed", "STEPS", ...) and year.
 * dict.vars: one row per shipped dictionary entry (8.5) plus `projection_from` (the first projected
 *   year, NA when none) and `series_window` (e.g. "1965-2025" when sector-series holds the var).
 * dict.item_units: per-item unit overrides (oil kb/d vs gas EJ in production.fuels; the IMF units).
 * dict.cells.section_cards: pipe-joined var ids, in dictionary order, of every measure the cell's
 *   profile can show, generated by scripts/87 (never hand-written): measures with item axis `cell`
 *   that have data for the cell, plus measures on other axes whose dictionary `link_stage` matches the
 *   cell's stage (or is `any`) and that have data for an item linked to the cell in dims.item_links.
 * avail: one row per (var, item, chunk) with data: the year range and the number of country-kind
 *   economies with a row, so views know what exists before facts or series load.
 * dict.cell_caveats (optional; sector_data change 2026-10-12, additive): the notes on a cell's HS6
 *   basket as a whole (what it holds and leaves out, codes moved between stages in the 2026-10 basket
 *   review), one row per (cell slug, dict.caveats id), in dictionary order. Show them with the
 *   cell's trade cards and in its data notes. Per-code notes stay in trade.codes.caveat.
 * dict.hhi_refs (optional; sector_data change 2026-10-12, additive): concentration thresholds that
 *   competition authorities use for mergers, as context for the HHI explainer (C5.3): `value` on the
 *   0-1 scale of meta.summary.hhi, `points` the same on the authorities' 0-10,000 scale, `label` what
 *   the document calls a market at or above it (or below it, for `side` "below"), `authority`,
 *   `document` (with section) and `year`. They are written for markets of firms, not for world trade
 *   between economies: show them as context (caveat hhi_refs_context), never as a verdict.
 */
export const META_SPEC = {
  'dims.geo': {
    kind: 'table',
    cols: {
      code: SR,
      name: SR,
      kind: { t: 'str', req: true, values: GEO_KINDS },
      iso_n3: I,
      tier: { t: 'str', req: true, values: TIERS },
      group: { t: 'str', req: true, values: GEO_GROUPS },
      eu_member: BR,
      re_export_hub: BR,
      bloc: S,
      s_axis: F,
      alignment_basis: S,
      wb_region: S,
      income: S,
      lon: F,
      lat: F,
      reason: S,
    },
    unique: ['code'],
  },
  'dims.region_members': {
    kind: 'table',
    cols: { geo: GEO, member: GEO },
    unique: ['geo', 'member'],
  },
  'dims.items': {
    kind: 'table',
    cols: {
      axis: { t: 'str', req: true, values: ITEM_AXES },
      slug: SR,
      label: SR,
      tech: { t: 'str', values: Object.values(TECH_LABELS) },
      stage: { t: 'str', values: Object.values(STAGE_LABELS) },
      headline: BR,
      sort: IR,
      has_trade: BR,
    },
    unique: ['axis', 'slug'],
  },
  'dims.item_links': {
    kind: 'table',
    cols: { item: ITEM, cell: ITEM, weight: F },
    unique: ['item', 'cell'],
  },
  'dims.mineral_demand': {
    kind: 'table',
    cols: { mineral: ITEM, tech: { t: 'str', values: Object.values(TECH_LABELS) }, label: SR, scenario: SR, year: IR, kt: F },
    unique: ['mineral', 'label', 'scenario', 'year'],
  },
  'dims.eu_outside_coalition': { kind: 'strv' },
  'dict.vars': {
    kind: 'table',
    cols: {
      id: SR,
      theme: { t: 'str', req: true, values: THEMES.map((t) => t.slug) },
      family: { t: 'str', req: true, values: FAMILY_IDS },
      label: SR,
      short: SR,
      definition: S,
      unit: SR,
      data_year: SR,
      years: S,
      year_field: BR,
      native_level: { t: 'str', req: true, values: NATIVE_LEVELS },
      item_axis: { t: 'str', req: true, values: ITEM_AXES },
      agg: { t: 'str', req: true, values: AGG_RULES },
      world_basis: { t: 'str', req: true, values: WORLD_BASES },
      norms: S,
      scale: { t: 'str', values: ['linear', 'log'] },
      higher_is: S,
      sign_note: S,
      display_scale: FR,
      source: SR,
      roster: S,
      licence: { t: 'str', req: true, values: LICENCES },
      caveats: S,
      primary_for: S,
      section: { t: 'str', values: SECTION_IDS },
      stale_years: I,
      min_base: F,
      feeds: S,
      prov: { t: 'str', req: true, values: PROVENANCES },
      prov_detail: S,
      phase: { t: 'str', req: true, values: PHASES },
      projection_from: I,
      series_window: S,
    },
    unique: ['id'],
  },
  'dict.units': {
    kind: 'table',
    cols: { id: SR, short: SR, long: SR, unit_kind: { t: 'str', req: true, values: UNIT_KINDS, soft: true } },
    unique: ['id'],
  },
  'dict.sources': {
    kind: 'table',
    cols: {
      id: SR,
      short: SR,
      citation: SR,
      publisher: S,
      edition: S,
      url: S,
      licence: { t: 'str', req: true, values: LICENCES },
      attribution: S,
      accessed: S,
      transform: S,
      issues: S,
    },
    unique: ['id'],
  },
  'dict.caveats': { kind: 'table', cols: { id: SR, text: SR }, unique: ['id'] },
  'dict.themes': {
    kind: 'table',
    cols: { slug: { t: 'str', req: true, values: THEMES.map((t) => t.slug) }, label: SR, description: S, order: IR, default_var: SR },
    unique: ['slug'],
  },
  'dict.families': {
    kind: 'table',
    cols: { id: { t: 'str', req: true, values: FAMILY_IDS }, label: SR, section: { t: 'str', values: SECTION_IDS } },
    unique: ['id'],
  },
  'dict.cells': { kind: 'table', cols: { slug: SR, cell_blurb: S, section_cards: S }, unique: ['slug'] },
  'dict.cell_caveats': { kind: 'table', optional: true, cols: { slug: SR, caveat: SR }, unique: ['slug', 'caveat'] },
  'dict.hhi_refs': {
    kind: 'table',
    optional: true,
    cols: {
      id: SR,
      value: FR,
      points: IR,
      side: { t: 'str', req: true, values: ['above', 'below'] },
      label: SR,
      authority: SR,
      document: SR,
      year: IR,
    },
    unique: ['id'],
  },
  'dict.techs': { kind: 'table', cols: { slug: { t: 'str', req: true, values: TECH_SLUGS }, label: SR, tech_blurb: S }, unique: ['slug'] },
  'dict.item_units': { kind: 'table', cols: { var: VAR, item: ITEM, unit: SR }, unique: ['var', 'item'] },
  rosters: { kind: 'table', cols: { roster: SR, geo: GEO }, unique: ['roster', 'geo'] },
  coverage: {
    kind: 'table',
    cols: {
      item: ITEM,
      family: { t: 'str', req: true, values: FAMILY_IDS },
      status: { t: 'str', req: true, values: COVERAGE_STATUSES },
      n_geo: IR,
      reason: { t: 'str', values: COVERAGE_REASONS, soft: true },
      sources: S,
    },
    unique: ['item', 'family'],
  },
  avail: {
    kind: 'table',
    cols: {
      var: VAR,
      item: ITEM,
      chunk: { t: 'str', req: true, values: CHUNK_KINDS },
      year_min: I,
      year_max: I,
      n_years: IR,
      n_geo: IR,
    },
    unique: ['var', 'item', 'chunk'],
  },
  summary: {
    kind: 'table',
    cols: {
      item: ITEM,
      var: VAR,
      year: IR,
      eu_mode: { t: 'str', req: true, values: EU_MODES },
      W: F,
      basis: { t: 'str', req: true, values: WORLD_BASES },
      n: IR,
      top1_geo: { t: 'i32', ref: 'dims.geo' },
      top1_share: F,
      top3_share: F,
      hhi: F,
      hhi_lower_bound: B,
      eff_n: F,
      china: F,
      core: F,
      partner: F,
      rest: F,
      unattributed: F,
      growth: F,
      growth_kind: { t: 'str', values: GROWTH_KINDS, soft: true },
    },
    unique: ['item', 'var', 'year', 'eu_mode'],
  },
  reconcile: {
    kind: 'table',
    cols: {
      item: ITEM,
      year: IR,
      matrix_total: F,
      matrix_china: F,
      matrix_hhi: F,
      reported_total: F,
      reported_china: F,
      reported_hhi: F,
      n_reporters: I,
      warn: BR,
    },
    unique: ['item', 'year'],
  },
} as const satisfies ChunkSpec;

/**
 * sector-facts: every cross-section and short-series fact, the slice index and the world totals.
 *
 * facts.notes (optional; integrator change 2026-10-11, additive, sector-data track): per-fact notes,
 * one row per (fact, note); every row names a facts.obs row (var, item, year, geo) and `note` is a
 * dict.caveats id whose text is the note. Sorted like facts.obs. Show them with the fact (tooltip,
 * table "Source note"); the measure's own caveat list carries the same ids only when the build has
 * such a fact. Ids of this build:
 *   eia_production           production (production.fuels; for consumption.balance /
 *                            consumption.self_sufficiency the production behind them) from the EIA
 *                            International Energy Statistics where EI does not report it separately:
 *                            an observed value (obs / zero), obs_year = the EIA year (gas and coal
 *                            2024 for 2025: EIA publishes them a year after oil);
 *   eia_consumption          the economy's consumption is EIA's (EI lists its production only);
 *   production_assumed_zero  neither EI nor EIA reports the production separately (inside an EI
 *                            "Other" residual): assumed 0 (user rule 2026-10-11), status nsr with the
 *                            value that gives: the balance = minus consumption, self-sufficiency 0;
 *   fossil_assumed_zero      fossil sums the fuels; one of them is not reported and counts as 0;
 *   ei_oil_shale             Estonia coal: EI counts oil shale as coal, EIA's coal series leave it out.
 */
export const FACTS_SPEC = {
  'facts.obs': OBS,
  'facts.index': INDEX,
  'facts.world': WORLD,
  'facts.notes': {
    kind: 'table',
    optional: true,
    cols: { var: VAR, item: ITEM, geo: GEO, year: IR, note: SR },
    sorted: ['var', 'item', 'year', 'geo'],
    unique: ['var', 'item', 'year', 'geo', 'note'],
  },
} as const satisfies ChunkSpec;

/**
 * sector-trade-<tech>: the reconciled Comtrade matrix of the tech's cells (3.8). A pair absent from
 * trade.flows in a year is an observed zero when the importer declared imports of the cell that year
 * (trade.declared.imports_declared) or, when it did not, when the exporter declared exports of the
 * cell that year (a mirror zero; the rule of partnership_strength_build_trade_cells(), which
 * scripts/87 verifies on every matrix pair-year); otherwise it is unobserved.
 * v<year> NaN <=> s<year> "none".
 */
export const TRADE_SPEC = {
  'trade.flows': {
    kind: 'table',
    cols: {
      item: ITEM,
      exporter: GEO,
      importer: GEO,
      v2020: F,
      v2024: F,
      s2020: { t: 'str', req: true, values: TRADE_FLOW_STATUSES },
      s2024: { t: 'str', req: true, values: TRADE_FLOW_STATUSES },
    },
    sorted: ['item', 'exporter'],
    unique: ['item', 'exporter', 'importer'],
  },
  'trade.declared': {
    kind: 'table',
    cols: { item: ITEM, geo: GEO, year: IR, imports_declared: BR, exports_declared: BR },
    unique: ['item', 'geo', 'year'],
  },
  'trade.codes': {
    kind: 'table',
    cols: { item: ITEM, sub_sector: SR, hs6: SR, description: S, shared_n: IR, caveat: S },
    unique: ['item', 'hs6'],
  },
  'trade.excluded': {
    kind: 'table',
    cols: { item: ITEM, sub_sector: S, hs6: SR, reason: SR },
  },
  /**
   * Trade by sub-component (measures trade.sub_exports / trade.sub_imports; shipped since the
   * sector_data change of 2026-10-12, additive; optional so that a chunk without a basket still
   * validates). `sub_sector` = a trade.codes.sub_sector of the same item (the names and their HS6
   * codes are in trade.codes; no dims table). Values on the reconciled matrix of the cell: for each
   * (pair, year) the cell's own source (the importer's declaration when the importer declared the
   * cell that year, else the exporter's x1.06), summed over the sub-component's HS6 codes, so a
   * cell's sub-components add up to its facts trade.exports / trade.imports for every (geo, year)
   * (scripts/87 checks it within 1e-9 relative). A code sits in one sub-component per cell, so they
   * never overlap within a cell; codes shared with other cells count in each cell. Rows: years 2020
   * and 2024; an economy has a row where its exports or imports of the sub-component are > 0; the
   * other side is 0 when the economy's cell total on that side was observed that year and NaN when
   * not; geo WLD rows hold the world totals (exports = imports = the sum of all flows). Sorted by item.
   */
  'trade.cty_sub': {
    kind: 'table',
    optional: true,
    cols: { item: ITEM, sub_sector: SR, geo: GEO, year: IR, exports: F, imports: F },
    sorted: ['item'],
    unique: ['item', 'sub_sector', 'geo', 'year'],
  },
  /**
   * Intra-EU flows of each sub-component and year (both sides EU members; absent = 0), for the EU
   * bloc mode of meta.summary's trade rule: EUU = sum(members) - intra, W = W - intra.
   */
  'trade.sub_intra': {
    kind: 'table',
    optional: true,
    cols: { item: ITEM, sub_sector: SR, year: IR, intra: FR },
    unique: ['item', 'sub_sector', 'year'],
  },
  /**
   * meta.summary per sub-component (the same rules, R sector_summary(), on trade.cty_sub): one row per
   * (item, sub_sector, var = trade.sub_exports | trade.sub_imports, year, eu_mode). W = the WLD row;
   * holders = the country rows; growth pct_2020_2024 on the 2024 rows.
   */
  'trade.sub_summary': {
    kind: 'table',
    optional: true,
    cols: {
      item: ITEM,
      sub_sector: SR,
      var: VAR,
      year: IR,
      eu_mode: { t: 'str', req: true, values: EU_MODES },
      W: F,
      n: IR,
      top1_geo: { t: 'i32', ref: 'dims.geo' },
      top1_share: F,
      top3_share: F,
      hhi: F,
      hhi_lower_bound: B,
      eff_n: F,
      china: F,
      core: F,
      partner: F,
      rest: F,
      unattributed: F,
      growth: F,
    },
    sorted: ['item'],
    unique: ['item', 'sub_sector', 'var', 'year', 'eu_mode'],
  },
} as const satisfies ChunkSpec;

/**
 * sector-policy: IEA PAMS records as masks (bit i of tech_mask = policy.tags[i]) and the CAT ratings.
 * policy.cat.ndc holds CAT's "Net zero target" sub-rating (the CAT file has no NDC column).
 */
export const POLICY_SPEC = {
  'policy.pams': {
    kind: 'table',
    cols: {
      id: IR,
      geo: GEO,
      year: I,
      status: S,
      jurisdiction: S,
      type: S,
      tech_mask: IR,
      tech_src: { t: 'str', values: POLICY_TECH_SOURCES },
      stage_mask: IR,
      cross_cutting: BR,
    },
    unique: ['id'],
  },
  'policy.tags': { kind: 'strv' },
  'policy.cat': {
    kind: 'table',
    cols: {
      geo: GEO,
      overall: S,
      policies_action: S,
      domestic_target: S,
      fair_share: S,
      climate_finance: S,
      ndc: S,
      updated: S,
    },
    unique: ['geo'],
  },
} as const satisfies ChunkSpec;

/** sector-policy-text: titles and IEA URLs of the PAMS records (ids as in policy.pams). */
export const POLICY_TEXT_SPEC = {
  'policy.text': { kind: 'table', cols: { id: IR, title: S, url: S }, unique: ['id'] },
} as const satisfies ChunkSpec;

/**
 * sector-series (time-series scope, user decision 2026-10-10):
 *   series.obs / series.index / series.world: annual series, the facts schema. EI production,
 *     generation and consumption 1965-2025 (production.fuels, production.generation,
 *     consumption.total_ej; W = EI WLD in series.world, the EI residual as a RES:ei geo), IEA EV
 *     regions 2021-2024 (production.ev_region, production.ev_region_sales), CIM power capacity
 *     additions 2018-2025 (investment.power_additions). A (var, item, geo, year) also in facts.obs
 *     carries the identical value (scripts/87 checks it); views take cross-sections from facts and
 *     lines from series.
 *   series.monthly: IMF monthly prices 2003-2025 (prices.monthly; world x commodity, geo WLD),
 *     units per commodity in dict.item_units.
 *   series.capacity: CIM manufacturing capacity by product and status (manufacturing.capacity;
 *     native units per product, never summed across products). Optional: shipped when built. One
 *     snapshot year (2025) for every status; construction / announced / paused are capacity CIM
 *     anticipates by 2030; `operational` includes CIM's "operational yet to come online".
 */
export const SERIES_SPEC = {
  'series.obs': OBS,
  'series.index': INDEX,
  'series.world': WORLD,
  'series.monthly': {
    kind: 'table',
    cols: { var: VAR, item: ITEM, geo: GEO, year: IR, month: IR, value: F, status: STATUS },
    sorted: ['var', 'item', 'geo', 'year', 'month'],
    unique: ['var', 'item', 'geo', 'year', 'month'],
  },
  'series.capacity': {
    kind: 'table',
    optional: true,
    cols: {
      item: ITEM,
      product: SR,
      unit: SR,
      geo: GEO,
      year: IR,
      cap_status: { t: 'str', req: true, values: ['operational', 'construction', 'announced', 'paused'], soft: true },
      value: F,
      status: STATUS,
    },
    unique: ['item', 'product', 'geo', 'year', 'cap_status'],
  },
} as const satisfies ChunkSpec;

/** sector-atlas (phase 2; not in this build). */
export const ATLAS_SPEC = {
  'atlas.obs': { kind: 'table', cols: { item: ITEM, geo: GEO, year: IR, exports: F }, sorted: ['item', 'geo', 'year'], unique: ['item', 'geo', 'year'] },
  'atlas.world': { kind: 'table', cols: { item: ITEM, year: IR, total: F }, unique: ['item', 'year'] },
  'atlas.match': { kind: 'table', cols: { item: ITEM, k: IR, n: IR }, unique: ['item'] },
} as const satisfies ChunkSpec;

/** Which side of a DIP position is a conduit financial centre (dev.dip `conduit`). */
export const DEV_CONDUIT = ['none', 'counterpart', 'investor', 'both'] as const;
export type DevConduit = (typeof DEV_CONDUIT)[number];

/**
 * sector-dev (development data change 2026-10-15; R/dashboard/sector/sector_dev.R). Optional chunk
 * (manifest flags.dev), loaded only by the Development & finance flow layouts and the Sources page.
 *   dev.oda: OECD CRS official development assistance, gross disbursements, USD millions at constant
 *     prices of the CRS base year (development.oda; dict source oecd_crs), one row per (aid-sector
 *     item, year, donor, recipient): the sum of the extract's rows, the mining item net of its coal and
 *     oil-and-gas (upstream) sub-rows, so a value can be negative. Recipients are WDI countries.
 *   dev.dip: IMF Direct Investment Positions, the reporter's outward debt-instrument position in the
 *     counterpart (USD millions, current), the mean of the years reported in the window
 *     (development.fdi_out; dict source imf_dip). `conduit` says which side is a conduit financial
 *     centre (dev.conduits): the Friendshore measure treats those positions as unobserved; they ship,
 *     flagged. A reported zero is a real zero; an absent pair was not reported.
 *   dev.conduits: the conduit financial centres (dej_top10: among the ten economies that host more
 *     than 85% of phantom FDI, Damgaard, Elkjaer and Johannesen 2019).
 */
export const DEV_SPEC = {
  'dev.oda': {
    kind: 'table',
    cols: { item: ITEM, year: IR, donor: GEO, recipient: GEO, value: FR },
    sorted: ['item', 'year', 'donor', 'recipient'],
    unique: ['item', 'year', 'donor', 'recipient'],
  },
  'dev.dip': {
    kind: 'table',
    cols: { reporter: GEO, counterpart: GEO, value: FR, year_min: IR, year_max: IR, n_years: IR, conduit: { t: 'str', req: true, values: DEV_CONDUIT } },
    sorted: ['reporter', 'counterpart'],
    unique: ['reporter', 'counterpart'],
  },
  'dev.conduits': { kind: 'table', cols: { geo: GEO, dej_top10: BR }, sorted: ['geo'], unique: ['geo'] },
} as const satisfies ChunkSpec;

// ---- sector-climate: Climate TRACE sector emissions (climate data change 2026-10-17, additive) ---------

/** How a Climate TRACE source is read: country totals, or the summed tracked assets of one group. */
export const CLIMATE_BASES = ['country', 'assets'] as const;
/** own: counted as the cell's emissions (with a match quality); context: a material the cell's products are made of. */
export const CLIMATE_ROLES = ['own', 'context'] as const;
/** Match quality of an own source (config/climate_trace_map.yml). */
export const CLIMATE_MATCHES = ['direct', 'partial', 'proxy'] as const;
/** Match quality of a cell: the best of its own sources, none without one. */
export const CLIMATE_CELL_MATCHES = ['direct', 'partial', 'proxy', 'none'] as const;
export type ClimateMatch = (typeof CLIMATE_CELL_MATCHES)[number];
/** ipcc: the technology has its own IPCC rows; proxy: the paper scores it with other technologies' values. */
export const CLIMATE_LIFECYCLE_BASES = ['ipcc', 'proxy'] as const;

/**
 * sector-climate (R/dashboard/sector/sector_climate.R; processed by scripts/16_build_climate_trace.R from
 * Climate TRACE, https://climatetrace.org, CC BY 4.0, dict source `climate_trace`; IPCC lifecycle medians,
 * dict source `ipcc_lifecycle`). Optional chunk (manifest flags.climate), loaded by the Sector Profile's
 * Climate lens and section and the Country Profile's Policy & climate section.
 *   climate.src: one row per Climate TRACE source the map uses: id ("coal-mining",
 *     "electricity-generation:coal"), subsector, sector, label, asset_group (null: the whole subsector),
 *     basis, repeated (the latest year's world total equals the previous year's: carried forward).
 *   climate.map: (cell item, src row, role, match (null for context), order).
 *   climate.cells: every cell's match quality and the note that explains it.
 *   climate.lifecycle: per technology label, the IPCC lifecycle gCO2e/kWh range (g_min..g_max, NaN for
 *     a proxy), the IPCC rows behind it, and for a proxy the technologies it is scored with (proxy_of,
 *     pipe-separated labels).
 *   climate.em: tonnes CO2e (100-year GWP) per (src row, geo, year); WLD = Climate TRACE's world total
 *     (country basis) or every country's tracked assets (assets basis).
 *   climate.intensity: the tracked assets' emissions over their activity per (subsector, asset group,
 *     geo, year): n assets with an activity, emissions (t CO2e), activity (activity_unit), value (unit,
 *     "t CO2e per MWh").
 */
export const CLIMATE_SPEC = {
  'climate.src': {
    kind: 'table',
    cols: { id: SR, subsector: SR, sector: SR, label: SR, asset_group: S, basis: { t: 'str', req: true, values: CLIMATE_BASES }, repeated: BR },
    unique: ['id'],
  },
  'climate.map': {
    kind: 'table',
    cols: { item: ITEM, src: IR, role: { t: 'str', req: true, values: CLIMATE_ROLES }, match: { t: 'str', values: CLIMATE_MATCHES }, order: IR },
    sorted: ['item', 'order'],
    unique: ['item', 'src'],
  },
  'climate.cells': { kind: 'table', cols: { item: ITEM, match: { t: 'str', req: true, values: CLIMATE_CELL_MATCHES }, note: SR }, sorted: ['item'], unique: ['item'] },
  'climate.lifecycle': {
    kind: 'table',
    cols: { tech: SR, basis: { t: 'str', req: true, values: CLIMATE_LIFECYCLE_BASES }, g_min: F, g_max: F, rows: S, proxy_of: S },
    unique: ['tech'],
  },
  'climate.em': { kind: 'table', cols: { src: IR, geo: GEO, year: IR, value: FR }, sorted: ['src', 'year', 'geo'], unique: ['src', 'year', 'geo'] },
  'climate.intensity': {
    kind: 'table',
    cols: { subsector: SR, asset_group: SR, geo: GEO, year: IR, n: IR, emissions: FR, activity: FR, value: FR, unit: SR, activity_unit: SR },
    unique: ['subsector', 'asset_group', 'year', 'geo'],
  },
} as const satisfies ChunkSpec;

// ---- sector-fdi: CM-FDI (integrator change 2026-10-11, additive) ---------------------------------
//
// The Cleantech Manufacturing FDI Dataset (CM-FDI): P. Bandara and the Net Zero Industrial Policy Lab,
// Johns Hopkins University, https://cmfdi.netzeropolicylab.com, edition 2026.10.05, CC BY 4.0 (licence
// `attribution`; the dict.sources row `cmfdi` holds the citation and attribution every view must show).
// Cross-border greenfield and expansion projects in solar, wind, battery and EV manufacturing (plus a
// few R&D, design and logistics sites), one row per project. Written by scripts/87
// (R/dashboard/sector/sector_fdi.R); captions through the measures investment.fdi_projects,
// investment.fdi_value and investment.fdi_parent_share (dictionary source `cmfdi`, caveats cmfdi_*).

/** CM-FDI `Project Status`, verbatim. Open vocabulary (a new edition may add one; scripts/87 stops first). */
export const FDI_PROJECT_STATUSES = ['Announced', 'Under Construction', 'Completed', 'Operational', 'Paused', 'Cancelled', 'Closed'] as const;
/** CM-FDI `Operational`, verbatim (e.g. a Completed project that has since closed reads "Ceased operations"). */
export const FDI_OPERATIONAL_STATES = ['Operational', 'Announced only', 'Under construction', 'Ceased operations', 'Paused'] as const;
/** CM-FDI `Industry activity`, verbatim. The indices count "Manufacturing" only (CM-FDI's count of 974). */
export const FDI_ACTIVITIES = [
  'Manufacturing',
  'R&D & Manufacturing',
  'Research & Development',
  'Design, Development & Testing',
  'Logistics, Distribution & Transportation',
] as const;
/** fdi.sources `field`: CM-FDI's nine source columns, in the CSV's order. */
export const FDI_SOURCE_FIELDS = ['parent', 'status', 'location', 'amount', 'ownership', 'announced', 'completed', 'activity', 'jv'] as const;
export type FdiSourceField = (typeof FDI_SOURCE_FIELDS)[number];
/** CM-FDI's own column headings of the source fields (for labels). */
export const FDI_SOURCE_LABELS: Readonly<Record<FdiSourceField, string>> = {
  parent: 'Parent company name and HQ',
  status: 'Project status',
  location: 'Project location, country and city',
  amount: 'Amount',
  ownership: 'Ownership share',
  announced: 'Date announced',
  completed: 'Date completed',
  activity: 'Industry activity',
  jv: 'Joint venture',
};
/**
 * The project sets the aggregates are computed over: `all` = every project (all statuses and
 * activities); `index_use` = the projects the Partnership indices use (fdi.projects.index_use; the
 * rules of config/partnership.yml cm_fdi, shown in the dictionary caveat cmfdi_status).
 */
export const FDI_BASES = ['all', 'index_use'] as const;
export type FdiBasis = (typeof FDI_BASES)[number];
/**
 * The investor's group (fdi.projects.home_group): `china` for a parent headquartered in mainland
 * China, Hong Kong or Macao (config cm_fdi china_hq_iso3, as the indices' Chinese-parent exposure
 * counts them; dictionary caveat cmfdi_hq), else the dims.geo group of the HQ economy. FDI views
 * colour by home_group, not by dims.geo (where HKG is rest).
 */
export const FDI_GROUPS = ['china', 'core', 'partner', 'rest'] as const satisfies readonly GeoGroup[];
/** Value states of a project's capital investment: obs = disclosed; nq = "Investment value not publicly disclosed" (NaN, never 0). */
export const FDI_VALUE_STATUSES = ['obs', 'nq'] as const satisfies readonly StatusCode[];
/** The measures whose data live in sector-fdi (VAR_CATALOGUE chunks ['fdi']). */
export const FDI_VAR_IDS = VAR_CATALOGUE.filter((v) => (v.chunks as readonly ChunkKind[]).includes('fdi')).map((v) => v.id);

/**
 * sector-fdi (lazy; load it only for the CM-FDI views). Five tables:
 *
 * fdi.projects: one row per CM-FDI project, every status and activity, sorted by (item, year_announced,
 *   pid); pid unique. `item` = the Midstream cell (Battery -> batteries-midstream, EV ->
 *   electric-vehicles-midstream, Solar, Wind); `technology` = CM-FDI's own label. `home` = the parent's
 *   HQ economy, `dest` = the destination (never equal). Text fields are the CSV's, trimmed (city,
 *   phase, jv_company NA when blank). `year_completed` NA = "N/A" or blank; for projects not yet
 *   complete it is the planned year. `value` = Capital Investment (US$m, current, as published; unit
 *   usd_m_announced): NaN with value_status nq when not disclosed, else > 0 with obs. jv_local_state NA
 *   = blank in CM-FDI. Flags by the indices' settings (config/partnership.yml cm_fdi):
 *   `manufacturing` = activity in its manufacturing activities ("Manufacturing"); `active` = status
 *   kept (Announced, Under Construction, Completed, Operational) and Operational not "Ceased
 *   operations" or "Paused"; `index_use` = manufacturing, active and inside the announcement window
 *   (every year by default): the projects the Partnership indices use. `home_group` (FDI_GROUPS) = the
 *   parent's HQ group. Values are identical() to the CSV after this cleaning (scripts/87 checks it).
 * fdi.sources: one row per non-empty CM-FDI source cell: `project` = 0-based row of fdi.projects,
 *   `field` (FDI_SOURCE_FIELDS), `text` verbatim (may hold dates, "(year only)", "fDi Markets (FT)",
 *   "Investment value not publicly disclosed"), `url` = the http(s) links found in it, pipe-joined (NA
 *   when none; only http:// and https://, characters unsafe in an attribute percent-encoded: render
 *   only these as links), `unverified` = the text carries CM-FDI's "[UNVERIFIED]" mark.
 * fdi.flows: projects summed by (item, home, dest, basis): n projects, n_value with a disclosed value,
 *   value = the sum of disclosed values (US$m; NaN when n_value = 0, never 0), year_min / year_max of
 *   the announcement years.
 * fdi.groups: per (item, dest, basis), the split of the destination's projects by home_group (raw
 *   counts: a joint venture counts wholly under its listed parent, whereas the indices attribute a
 *   share of it): one row per FDI_GROUPS entry (n = 0 rows included, so share_n sums to 1). dest = WLD is
 *   every destination together. value = disclosed sum (0 when n = 0; NaN when n > 0 and nothing is
 *   disclosed); share_n = n / the destination's n; share_value = value / the destination's disclosed
 *   sum (NaN when either is NaN).
 * fdi.years: announcements by (item, year announced, basis, group of the parent's HQ): n, n_value,
 *   value as in fdi.flows; only rows with n >= 1 (a missing row = no project announced).
 * Sums are taken over fdi.projects in table order; a view that re-sums the same projects in that order
 * reproduces counts exactly and value sums and shares to within 1e-12 relative (R's sum() accumulates
 * in long double: 17 of the 626 disclosed flow sums of the 2026-10-11 export differ from a double
 * re-sum in the last bit).
 */
export const FDI_SPEC = {
  'fdi.projects': {
    kind: 'table',
    cols: {
      pid: SR,
      item: ITEM,
      technology: SR,
      parent: SR,
      home: GEO,
      dest: GEO,
      city: S,
      status: { t: 'str', req: true, values: FDI_PROJECT_STATUSES, soft: true },
      operational: { t: 'str', req: true, values: FDI_OPERATIONAL_STATES, soft: true },
      activity: { t: 'str', req: true, values: FDI_ACTIVITIES, soft: true },
      year_announced: IR,
      year_completed: I,
      value: F,
      value_status: { t: 'str', req: true, values: FDI_VALUE_STATUSES },
      expansion: BR,
      phase: S,
      jv: BR,
      jv_local: BR,
      jv_local_state: B,
      jv_company: S,
      manufacturing: BR,
      active: BR,
      index_use: BR,
      home_group: { t: 'str', req: true, values: FDI_GROUPS },
    },
    sorted: ['item', 'year_announced'],
    unique: ['pid'],
  },
  'fdi.sources': {
    kind: 'table',
    cols: { project: IR, field: { t: 'str', req: true, values: FDI_SOURCE_FIELDS }, text: SR, url: S, unverified: BR },
    sorted: ['project'],
    unique: ['project', 'field'],
  },
  'fdi.flows': {
    kind: 'table',
    cols: { item: ITEM, home: GEO, dest: GEO, basis: { t: 'str', req: true, values: FDI_BASES }, n: IR, n_value: IR, value: F, year_min: IR, year_max: IR },
    sorted: ['item', 'home', 'dest'],
    unique: ['item', 'home', 'dest', 'basis'],
  },
  'fdi.groups': {
    kind: 'table',
    cols: {
      item: ITEM,
      dest: GEO,
      basis: { t: 'str', req: true, values: FDI_BASES },
      group: { t: 'str', req: true, values: FDI_GROUPS },
      n: IR,
      n_value: IR,
      value: F,
      share_n: FR,
      share_value: F,
    },
    sorted: ['item', 'dest'],
    unique: ['item', 'dest', 'basis', 'group'],
  },
  'fdi.years': {
    kind: 'table',
    cols: {
      item: ITEM,
      year: IR,
      basis: { t: 'str', req: true, values: FDI_BASES },
      group: { t: 'str', req: true, values: FDI_GROUPS },
      n: IR,
      n_value: IR,
      value: F,
    },
    sorted: ['item', 'year'],
    unique: ['item', 'year', 'basis', 'group'],
  },
} as const satisfies ChunkSpec;

export type SectorMetaChunk = ChunkOf<typeof META_SPEC>;
export type SectorFactsChunk = ChunkOf<typeof FACTS_SPEC>;
export type SectorTradeChunk = ChunkOf<typeof TRADE_SPEC>;
export type SectorPolicyChunk = ChunkOf<typeof POLICY_SPEC>;
export type SectorPolicyTextChunk = ChunkOf<typeof POLICY_TEXT_SPEC>;
export type SectorSeriesChunk = ChunkOf<typeof SERIES_SPEC>;
export type SectorAtlasChunk = ChunkOf<typeof ATLAS_SPEC>;
export type SectorDevChunk = ChunkOf<typeof DEV_SPEC>;
export type SectorClimateChunk = ChunkOf<typeof CLIMATE_SPEC>;
export type SectorFdiChunk = ChunkOf<typeof FDI_SPEC>;
export type FdiProjectTable = SectorFdiChunk['fdi.projects'];
export type FdiSourceTable = SectorFdiChunk['fdi.sources'];
export type FdiFlowTable = SectorFdiChunk['fdi.flows'];
export type FdiGroupTable = SectorFdiChunk['fdi.groups'];
export type FdiYearTable = SectorFdiChunk['fdi.years'];

/** Row types of the most used tables, for derive signatures. */
export type GeoTable = SectorMetaChunk['dims.geo'];
export type ItemTable = SectorMetaChunk['dims.items'];
export type VarTable = SectorMetaChunk['dict.vars'];
export type SummaryTable = SectorMetaChunk['summary'];
export type ObsTable = SectorFactsChunk['facts.obs'];
export type IndexTable = SectorFactsChunk['facts.index'];
export type WorldTable = SectorFactsChunk['facts.world'];
export type FlowTable = SectorTradeChunk['trade.flows'];
/** Trade by sub-component and its summaries (optional trade tables; sector_data change 2026-10-12). */
export type CtySubTable = NonNullable<SectorTradeChunk['trade.cty_sub']>;
export type SubIntraTable = NonNullable<SectorTradeChunk['trade.sub_intra']>;
export type SubSummaryTable = NonNullable<SectorTradeChunk['trade.sub_summary']>;
export type HhiRefTable = NonNullable<SectorMetaChunk['dict.hhi_refs']>;
export type CellCaveatTable = NonNullable<SectorMetaChunk['dict.cell_caveats']>;

export type SectorChunkName =
  | 'sector-meta'
  | 'sector-facts'
  | `sector-trade-${TradeTechSlug}`
  | 'sector-policy'
  | 'sector-policy-text'
  | 'sector-series'
  | 'sector-atlas'
  | 'sector-dev'
  | 'sector-fdi'
  | 'sector-climate';

export type ChunkFor<N extends SectorChunkName> = N extends 'sector-meta'
  ? SectorMetaChunk
  : N extends 'sector-facts'
    ? SectorFactsChunk
    : N extends `sector-trade-${string}`
      ? SectorTradeChunk
      : N extends 'sector-policy'
        ? SectorPolicyChunk
        : N extends 'sector-policy-text'
          ? SectorPolicyTextChunk
          : N extends 'sector-series'
            ? SectorSeriesChunk
            : N extends 'sector-atlas'
              ? SectorAtlasChunk
              : N extends 'sector-fdi'
                ? SectorFdiChunk
                : N extends 'sector-climate'
                  ? SectorClimateChunk
                  : SectorDevChunk;

export function tradeChunkName(tech: TradeTechSlug): SectorChunkName {
  return `sector-trade-${tech}`;
}

export function isSectorChunkName(s: string): s is SectorChunkName {
  if (s.startsWith('sector-trade-')) return isTradeTechSlug(s.slice('sector-trade-'.length));
  return ['sector-meta', 'sector-facts', 'sector-policy', 'sector-policy-text', 'sector-series', 'sector-atlas', 'sector-dev', 'sector-fdi', 'sector-climate'].includes(s);
}

/** The object spec of a chunk name. */
export function chunkSpec(name: SectorChunkName): ChunkSpec {
  if (name.startsWith('sector-trade-')) return TRADE_SPEC;
  switch (name) {
    case 'sector-meta':
      return META_SPEC;
    case 'sector-facts':
      return FACTS_SPEC;
    case 'sector-policy':
      return POLICY_SPEC;
    case 'sector-policy-text':
      return POLICY_TEXT_SPEC;
    case 'sector-series':
      return SERIES_SPEC;
    case 'sector-atlas':
      return ATLAS_SPEC;
    case 'sector-fdi':
      return FDI_SPEC;
    case 'sector-climate':
      return CLIMATE_SPEC;
    default:
      return DEV_SPEC;
  }
}

// ---- Validation --------------------------------------------------------------------------------

/** Row counts of the meta tables that i32 `ref` columns point into. */
export interface SectorRefSizes {
  'dims.geo': number;
  'dims.items': number;
  'dict.vars': number;
}

export function refSizes(meta: SectorMetaChunk): SectorRefSizes {
  return { 'dims.geo': meta['dims.geo'].nrow, 'dims.items': meta['dims.items'].nrow, 'dict.vars': meta['dict.vars'].nrow };
}

export interface ChunkCheck {
  errors: string[];
  warnings: string[];
}

export class SectorContractError extends Error {
  readonly code = 'SECTOR_CONTRACT';
  readonly problems: string[];
  constructor(name: string, problems: string[]) {
    const head = problems.slice(0, 8).join('; ');
    super(`${name} does not match the sector data contract: ${head}${problems.length > 8 ? `; and ${problems.length - 8} more` : ''}`);
    this.name = 'SectorContractError';
    this.problems = problems;
  }
}

const MAX_REPORTED = 5;

function isDecoded(x: unknown): x is OsbDecoded {
  return isObj(x) && isObj(x.header) && isObj(x.flat);
}

/** A dotted path from the flat map (decoded.flat) or the nested tree (decoded.objects). */
function pick(objects: Record<string, unknown>, path: string): unknown {
  if (Object.prototype.hasOwnProperty.call(objects, path)) return objects[path];
  let cur: unknown = objects;
  for (const part of path.split('.')) {
    if (!isObj(cur) || !Object.prototype.hasOwnProperty.call(cur, part)) return undefined;
    cur = cur[part];
  }
  return cur;
}

function kindOf(c: unknown): ColKind | null {
  if (c instanceof Float64Array) return 'f64';
  if (c instanceof Int32Array) return 'i32';
  if (c instanceof Uint8Array) return 'bool';
  if (Array.isArray(c)) return 'str';
  return null;
}

function isNA(c: Column, i: number): boolean {
  if (c instanceof Float64Array) return c[i] !== c[i];
  if (c instanceof Int32Array) return c[i] === NA_INT;
  if (c instanceof Uint8Array) return c[i] === NA_BOOL;
  return c[i] === null;
}

/** Collects at most MAX_REPORTED example rows per problem. */
class Tally {
  private n = 0;
  private rows: number[] = [];
  hit(i: number) {
    this.n++;
    if (this.rows.length < MAX_REPORTED) this.rows.push(i);
  }
  report(out: string[], msg: string) {
    if (this.n) out.push(`${msg} (${this.n} row${this.n === 1 ? '' : 's'}, e.g. row ${this.rows.join(', ')})`);
  }
}

function checkTable(path: string, spec: TableSpec, t: unknown, ref: Partial<SectorRefSizes>, out: ChunkCheck): t is Table {
  if (!isObj(t) || typeof t.nrow !== 'number' || !isObj(t.cols)) {
    out.errors.push(`${path} is not a table`);
    return false;
  }
  const tbl = t as unknown as Table;
  let ok = true;
  for (const [name, cs] of Object.entries(spec.cols)) {
    const c = tbl.cols[name];
    const where = `${path}.${name}`;
    if (c === undefined) {
      out.errors.push(`${where} is missing`);
      ok = false;
      continue;
    }
    const k = kindOf(c);
    if (k !== cs.t) {
      out.errors.push(`${where} is ${k ?? typeof c}, expected ${cs.t}`);
      ok = false;
      continue;
    }
    if (c.length !== tbl.nrow) {
      out.errors.push(`${where} has ${c.length} rows, the table ${tbl.nrow}`);
      ok = false;
      continue;
    }
    const na = new Tally();
    const bad = new Tally();
    const inf = new Tally();
    const softBad = new Tally();
    const allowed = cs.values ? new Set<string>(cs.values) : null;
    const size = cs.ref ? ref[cs.ref] : undefined;
    for (let i = 0; i < tbl.nrow; i++) {
      if (isNA(c, i)) {
        if (cs.req) na.hit(i);
        continue;
      }
      if (c instanceof Float64Array) {
        if (!Number.isFinite(c[i])) inf.hit(i);
      } else if (c instanceof Uint8Array) {
        if (c[i] > 1) bad.hit(i);
      } else if (c instanceof Int32Array) {
        if (cs.ref && (c[i] < 0 || (size !== undefined && c[i] >= size))) bad.hit(i);
      } else {
        const v = (c as Str)[i];
        if (typeof v !== 'string') bad.hit(i);
        else if (allowed && !allowed.has(v)) (cs.soft ? softBad : bad).hit(i);
      }
    }
    na.report(out.errors, `${where} has NA values`);
    inf.report(out.errors, `${where} has infinite values`);
    if (c instanceof Uint8Array) bad.report(out.errors, `${where} has values other than 0, 1 and NA`);
    else if (c instanceof Int32Array) bad.report(out.errors, `${where} has indices outside ${cs.ref}${size === undefined ? '' : ` (0..${size - 1})`}`);
    else bad.report(out.errors, `${where} has values outside ${allowed ? `{${[...allowed].join(', ')}}` : 'strings'}`);
    softBad.report(out.warnings, `${where} has values outside the known vocabulary {${cs.values?.join(', ')}}`);
  }
  if (!ok) return false;

  const keyCols = (names: readonly string[]) => names.map((n) => tbl.cols[n]);
  if (spec.sorted) {
    const cols = keyCols(spec.sorted) as Int32Array[];
    const unsorted = new Tally();
    for (let i = 1; i < tbl.nrow; i++) {
      for (const c of cols) {
        if (c[i] > c[i - 1]) break;
        if (c[i] < c[i - 1]) {
          unsorted.hit(i);
          break;
        }
      }
    }
    unsorted.report(out.errors, `${path} is not sorted by (${spec.sorted.join(', ')})`);
  }
  if (spec.unique) {
    const cols = keyCols(spec.unique);
    const seen = new Set<string>();
    const dup = new Tally();
    for (let i = 0; i < tbl.nrow; i++) {
      const key = cols.map((c) => String((c as ArrayLike<unknown>)[i])).join('\u0001');
      if (seen.has(key)) dup.hit(i);
      else seen.add(key);
    }
    dup.report(out.errors, `${path} repeats the key (${spec.unique.join(', ')})`);
  }
  return true;
}

/** facts.obs + facts.index (or the series pair): slices tile obs in order and match their keys; NaN only for nq / na_growth / nsr. */
function checkObsIndex(prefix: string, obs: unknown, index: unknown, out: ChunkCheck) {
  const o = obs as SectorFactsChunk['facts.obs'];
  const x = index as SectorFactsChunk['facts.index'];
  const nanOk = new Set<string>(NAN_STATUS_CODES);
  const badNaN = new Tally();
  for (let i = 0; i < o.nrow; i++) if (o.cols.value[i] !== o.cols.value[i] && !nanOk.has(o.cols.status[i] ?? '')) badNaN.hit(i);
  badNaN.report(out.errors, `${prefix}.obs has NA values with a status other than ${NAN_STATUS_CODES.join(' / ')}`);

  let next = 0;
  const gap = new Tally();
  const mismatch = new Tally();
  for (let s = 0; s < x.nrow; s++) {
    const start = x.cols.start[s];
    const n = x.cols.n[s];
    if (start !== next || n < 1 || start + n > o.nrow) {
      gap.hit(s);
      next = start + Math.max(n, 0);
      continue;
    }
    for (let r = start; r < start + n; r++) {
      if (o.cols.var[r] !== x.cols.var[s] || o.cols.item[r] !== x.cols.item[s] || o.cols.year[r] !== x.cols.year[s]) {
        mismatch.hit(s);
        break;
      }
    }
    next = start + n;
  }
  gap.report(out.errors, `${prefix}.index slices do not tile ${prefix}.obs in order`);
  mismatch.report(out.errors, `${prefix}.index slices hold rows of another (var, item, year)`);
  if (x.nrow > 0 && next !== o.nrow) out.errors.push(`${prefix}.index covers ${next} of ${o.nrow} ${prefix}.obs rows`);
  if (x.nrow === 0 && o.nrow > 0) out.errors.push(`${prefix}.index is empty but ${prefix}.obs has ${o.nrow} rows`);
}

/** facts.notes: every row names a facts.obs row, and the note id is not empty (dict.caveats ids are checked by scripts/87). */
function checkFactNotes(obs: unknown, notes: unknown, out: ChunkCheck) {
  const o = obs as SectorFactsChunk['facts.obs'];
  const n = notes as NonNullable<SectorFactsChunk['facts.notes']>;
  const keys = new Set<string>();
  for (let i = 0; i < o.nrow; i++) keys.add(`${o.cols.var[i]}|${o.cols.item[i]}|${o.cols.year[i]}|${o.cols.geo[i]}`);
  const orphan = new Tally();
  const empty = new Tally();
  for (let i = 0; i < n.nrow; i++) {
    if (!keys.has(`${n.cols.var[i]}|${n.cols.item[i]}|${n.cols.year[i]}|${n.cols.geo[i]}`)) orphan.hit(i);
    if (!n.cols.note[i]) empty.hit(i);
  }
  orphan.report(out.errors, 'facts.notes has rows without a facts.obs row');
  empty.report(out.errors, 'facts.notes.note has empty ids');
}

function splitPipe(s: string | null): string[] {
  return s ? s.split('|').filter((v) => v !== '') : [];
}

/** Cross-table rules of sector-meta. */
function checkMeta(m: SectorMetaChunk, out: ChunkCheck) {
  const geo = m['dims.geo'];
  const codes = geo.cols.code;
  const kinds = geo.cols.kind;
  const codeRe: Record<GeoKind, RegExp> = {
    country: /^[A-Z]{3}$/,
    eu: /^EUU$/,
    world: /^WLD$/,
    region: /^R:[^:]+:.+$/,
    entity: /^E:[^:]+:.+$/,
    residual: /^RES:.+$/,
  };
  const badCode = new Tally();
  for (let i = 0; i < geo.nrow; i++) {
    const k = kinds[i] as GeoKind | null;
    if (k && codeRe[k] && !codeRe[k].test(codes[i] ?? '')) badCode.hit(i);
  }
  badCode.report(out.errors, 'dims.geo.code does not match its kind (ISO3, EUU, WLD, R:<src>:<id>, E:<src>:<id>, RES:<src>)');
  const wld = codes.indexOf('WLD');
  if (wld < 0) out.errors.push('dims.geo has no WLD row');
  const chn = codes.indexOf('CHN');
  if (chn < 0) out.errors.push('dims.geo has no CHN row');
  else if (geo.cols.group[chn] !== 'china') out.errors.push('dims.geo: CHN must be in group china');
  const badGroup = new Tally();
  for (let i = 0; i < geo.nrow; i++) {
    if (kinds[i] === 'country' && geo.cols.group[i] === 'na') badGroup.hit(i);
    if (kinds[i] === 'country' && codes[i] !== 'CHN' && geo.cols.group[i] === 'china') badGroup.hit(i);
  }
  badGroup.report(out.errors, 'dims.geo: a country must be in china (CHN only), core, partner or rest');

  const items = m['dims.items'];
  const nNone = items.cols.axis.filter((a) => a === 'none').length;
  if (nNone !== 1) out.errors.push(`dims.items has ${nNone} rows with axis none, expected 1`);
  const badCell = new Tally();
  const badMineral = new Tally();
  let nCells = 0;
  let nHeadline = 0;
  for (let i = 0; i < items.nrow; i++) {
    const axis = items.cols.axis[i];
    if (axis === 'cell') {
      nCells++;
      if (items.cols.headline[i] === 1) nHeadline++;
      const tech = techSlugOf(items.cols.tech[i] ?? '');
      const stage = stageSlugOf(items.cols.stage[i] ?? '');
      if (!tech || !stage || items.cols.slug[i] !== cellSlug(tech, stage) || (items.cols.headline[i] === 1) !== (tech !== 'semiconductors')) badCell.hit(i);
    } else if (axis === 'mineral' && !(MINERAL_SLUGS as readonly string[]).includes(items.cols.slug[i] ?? '')) {
      badMineral.hit(i);
    }
  }
  badCell.report(out.errors, 'dims.items cell rows need slug = cellSlug(tech, stage) of known labels, headline FALSE only for Semiconductors');
  badMineral.report(out.warnings, `dims.items mineral slugs outside MINERAL_SLUGS`);
  if (nHeadline !== HEADLINE_TECH_SLUGS.length * STAGE_SLUGS.length) out.warnings.push(`dims.items has ${nHeadline} headline cells, expected 33`);
  if (nCells - nHeadline < 1 || nCells - nHeadline > STAGE_SLUGS.length) {
    out.warnings.push(`dims.items has ${nCells - nHeadline} non-headline cells, expected the Semiconductors stages of the basket (Midstream; Upstream since 2026-10-11)`);
  }

  const vars = m['dict.vars'];
  const unitIds = new Set(m['dict.units'].cols.id);
  const sourceIds = new Set(m['dict.sources'].cols.id);
  const caveatIds = new Set(m['dict.caveats'].cols.id);
  const rosterIds = new Set(m.rosters.cols.roster);
  const unknownVar = new Tally();
  const themeMismatch = new Tally();
  const badUnit = new Tally();
  const badSource = new Tally();
  const badCaveat = new Tally();
  const badRoster = new Tally();
  for (let i = 0; i < vars.nrow; i++) {
    const id = vars.cols.id[i] ?? '';
    if (!isVarId(id)) unknownVar.hit(i);
    if (themeOfVar(id) !== vars.cols.theme[i]) themeMismatch.hit(i);
    if (!unitIds.has(vars.cols.unit[i])) badUnit.hit(i);
    if (!sourceIds.has(vars.cols.source[i])) badSource.hit(i);
    if (splitPipe(vars.cols.caveats[i]).some((c) => !caveatIds.has(c))) badCaveat.hit(i);
    const r = vars.cols.roster[i];
    if (r !== null && !rosterIds.has(r)) badRoster.hit(i);
  }
  unknownVar.report(out.errors, 'dict.vars.id is not in the measure catalogue (VAR_IDS); a new id needs a contract change');
  themeMismatch.report(out.errors, 'dict.vars.theme differs from the id prefix');
  badUnit.report(out.errors, 'dict.vars.unit is not a dict.units id');
  badSource.report(out.errors, 'dict.vars.source is not a dict.sources id');
  badCaveat.report(out.errors, 'dict.vars.caveats names an id missing from dict.caveats');
  badRoster.report(out.warnings, 'dict.vars.roster names a roster missing from rosters');
  const cc = m['dict.cell_caveats'];
  if (cc) {
    const cellSlugs = new Set(m['dict.cells'].cols.slug);
    const badCc = new Tally();
    for (let i = 0; i < cc.nrow; i++) if (!caveatIds.has(cc.cols.caveat[i]) || !cellSlugs.has(cc.cols.slug[i])) badCc.hit(i);
    badCc.report(out.errors, 'dict.cell_caveats names a cell missing from dict.cells or a caveat missing from dict.caveats');
  }
  const iu = m['dict.item_units'];
  const badIu = new Tally();
  for (let i = 0; i < iu.nrow; i++) if (!unitIds.has(iu.cols.unit[i])) badIu.hit(i);
  badIu.report(out.errors, 'dict.item_units.unit is not a dict.units id');

  const varIds = new Set(vars.cols.id);
  const themes = m['dict.themes'];
  for (const t of THEMES) if (!themes.cols.slug.includes(t.slug)) out.errors.push(`dict.themes has no row for ${t.slug}`);
  const badDefault = new Tally();
  for (let i = 0; i < themes.nrow; i++) if (!varIds.has(themes.cols.default_var[i])) badDefault.hit(i);
  badDefault.report(out.errors, 'dict.themes.default_var is not a dict.vars id');
  for (const s of TECH_SLUGS) if (!m['dict.techs'].cols.slug.includes(s)) out.errors.push(`dict.techs has no row for ${s}`);
}

function checkTrade(t: SectorTradeChunk, out: ChunkCheck) {
  const f = t['trade.flows'];
  for (const y of TRADE_YEARS) {
    const v = f.cols[`v${y}`];
    const s = f.cols[`s${y}`];
    const bad = new Tally();
    const neg = new Tally();
    const zero = new Tally();
    for (let i = 0; i < f.nrow; i++) {
      if ((v[i] !== v[i]) !== (s[i] === 'none')) bad.hit(i);
      if (v[i] < 0) neg.hit(i);
      else if (v[i] === 0) zero.hit(i);
    }
    bad.report(out.errors, `trade.flows: v${y} is NaN exactly when s${y} is "none" is violated`);
    neg.report(out.errors, `trade.flows.v${y} has negative values`);
    zero.report(out.warnings, `trade.flows.v${y} ships zeros (zeros are rebuilt from trade.declared, not shipped)`);
  }
  const none = new Tally();
  for (let i = 0; i < f.nrow; i++) if (f.cols.s2020[i] === 'none' && f.cols.s2024[i] === 'none') none.hit(i);
  none.report(out.errors, 'trade.flows has rows with no observed value in any year');
  // trade.cty_sub (optional): no negative values; a row has a positive value on at least one side;
  // sub-components are trade.codes sub-sectors of the same item.
  const cs = t['trade.cty_sub'];
  if (cs) {
    const subs = new Set<string>();
    const codes = t['trade.codes'];
    for (let i = 0; i < codes.nrow; i++) subs.add(`${codes.cols.item[i]}|${codes.cols.sub_sector[i]}`);
    const neg = new Tally();
    const empty = new Tally();
    const unknown = new Tally();
    for (let i = 0; i < cs.nrow; i++) {
      const x = cs.cols.exports[i];
      const m = cs.cols.imports[i];
      if (x < 0 || m < 0) neg.hit(i);
      if (!(x > 0) && !(m > 0)) empty.hit(i);
      if (!subs.has(`${cs.cols.item[i]}|${cs.cols.sub_sector[i]}`)) unknown.hit(i);
    }
    neg.report(out.errors, 'trade.cty_sub has negative values');
    empty.report(out.errors, 'trade.cty_sub has rows with neither exports nor imports above 0');
    unknown.report(out.errors, 'trade.cty_sub.sub_sector is not a trade.codes sub-sector of the item');
  }
}

/** A link fdi.sources may carry: http(s), no white space, nothing unsafe in an attribute or a pipe list. */
const FDI_URL_RE = /^https?:\/\/[^\s"<>|`\\]+$/;

/** Cross-table rules of sector-dev (DEV_SPEC); the R mirror is sector_check_dev(). */
function checkDev(c: SectorDevChunk, out: ChunkCheck) {
  const o = c['dev.oda'];
  const selfAid = new Tally();
  for (let i = 0; i < o.nrow; i++) if (o.cols.donor[i] === o.cols.recipient[i]) selfAid.hit(i);
  selfAid.report(out.errors, 'dev.oda: a donor funding itself');
  const d = c['dev.dip'];
  const cg = new Set<number>(Array.from(c['dev.conduits'].cols.geo));
  const selfPos = new Tally();
  const badYears = new Tally();
  const badFlag = new Tally();
  for (let i = 0; i < d.nrow; i++) {
    const r = d.cols.reporter[i];
    const k = d.cols.counterpart[i];
    if (r === k) selfPos.hit(i);
    const y0 = d.cols.year_min[i];
    const y1 = d.cols.year_max[i];
    const n = d.cols.n_years[i];
    if (y0 > y1 || n < 1 || n > y1 - y0 + 1) badYears.hit(i);
    const want = cg.has(k) && cg.has(r) ? 'both' : cg.has(k) ? 'counterpart' : cg.has(r) ? 'investor' : 'none';
    if (d.cols.conduit[i] !== want) badFlag.hit(i);
  }
  selfPos.report(out.errors, 'dev.dip: a position in the reporter itself');
  badYears.report(out.errors, 'dev.dip: years out of order (year_min <= year_max, 1 <= n_years <= the window)');
  badFlag.report(out.errors, 'dev.dip.conduit disagrees with dev.conduits');
}

/** Cross-table rules of sector-climate (CLIMATE_SPEC); the R mirror is sector_check_climate(). */
function checkClimate(c: SectorClimateChunk, out: ChunkCheck) {
  const ns = c['climate.src'].nrow;
  const m = c['climate.map'];
  const badSrc = new Tally();
  const badMatch = new Tally();
  const rank = (x: string | null) => (x === null ? -1 : CLIMATE_CELL_MATCHES.indexOf(x as ClimateMatch));
  const best = new Map<number, number>();
  for (let i = 0; i < m.nrow; i++) {
    if (m.cols.src[i] >= ns) badSrc.hit(i);
    const own = m.cols.role[i] === 'own';
    if (own !== (m.cols.match[i] !== null)) badMatch.hit(i);
    if (own) {
      const r = rank(m.cols.match[i]);
      const b = best.get(m.cols.item[i]);
      if (r >= 0 && (b === undefined || r < b)) best.set(m.cols.item[i], r);
    }
  }
  const e = c['climate.em'];
  for (let i = 0; i < e.nrow; i++) if (e.cols.src[i] >= ns) badSrc.hit(i);
  badSrc.report(out.errors, 'climate: a src index past climate.src');
  badMatch.report(out.errors, 'climate.map: match must be set exactly on own rows');
  const cells = c['climate.cells'];
  const badCell = new Tally();
  for (let i = 0; i < cells.nrow; i++) {
    const b = best.get(cells.cols.item[i]);
    if (cells.cols.match[i] !== (b === undefined ? 'none' : CLIMATE_CELL_MATCHES[b])) badCell.hit(i);
  }
  badCell.report(out.errors, "climate.cells.match is not the best match of the cell's own sources");
  const s = c['climate.src'];
  const badBasis = new Tally();
  for (let i = 0; i < s.nrow; i++) if ((s.cols.basis[i] === 'assets') !== (s.cols.asset_group[i] !== null)) badBasis.hit(i);
  badBasis.report(out.errors, 'climate.src: an assets source needs an asset_group, a country source none');
  const l = c['climate.lifecycle'];
  const badLife = new Tally();
  for (let i = 0; i < l.nrow; i++) {
    const proxy = l.cols.basis[i] === 'proxy';
    if (proxy !== (l.cols.proxy_of[i] !== null) || (proxy && Number.isFinite(l.cols.g_min[i])) || (!proxy && !(l.cols.g_min[i] <= l.cols.g_max[i]))) badLife.hit(i);
  }
  badLife.report(out.errors, 'climate.lifecycle: proxy rows need proxy_of and no gCO2e range; ipcc rows g_min <= g_max');
  const it = c['climate.intensity'];
  const badInt = new Tally();
  for (let i = 0; i < it.nrow; i++) {
    const a = it.cols.activity[i];
    const v = it.cols.value[i];
    if (!(a > 0) || Math.abs(v - it.cols.emissions[i] / a) > 1e-12 * Math.max(Math.abs(v), 1e-300)) badInt.hit(i);
  }
  badInt.report(out.errors, 'climate.intensity: value is not emissions / activity');
}

/** Cross-table rules of sector-fdi (FDI_SPEC); the R mirror is sector_check_fdi(). */
function checkFdi(c: SectorFdiChunk, out: ChunkCheck) {
  const p = c['fdi.projects'];
  const valueState = new Tally();
  const nonPositive = new Tally();
  const badFlag = new Tally();
  const sameGeo = new Tally();
  for (let i = 0; i < p.nrow; i++) {
    const v = p.cols.value[i];
    if ((v !== v) !== (p.cols.value_status[i] === 'nq')) valueState.hit(i);
    if (v === v && v <= 0) nonPositive.hit(i);
    if (p.cols.index_use[i] === 1 && !(p.cols.manufacturing[i] === 1 && p.cols.active[i] === 1)) badFlag.hit(i);
    if (p.cols.home[i] === p.cols.dest[i]) sameGeo.hit(i);
  }
  valueState.report(out.errors, 'fdi.projects: value is NaN exactly when value_status is "nq" is violated');
  nonPositive.report(out.errors, 'fdi.projects.value has zero or negative values (an undisclosed value is NaN, never 0)');
  badFlag.report(out.errors, 'fdi.projects: index_use without manufacturing and active');
  sameGeo.report(out.errors, 'fdi.projects: home equals dest (CM-FDI projects are cross-border)');

  const s = c['fdi.sources'];
  const badProject = new Tally();
  const badUrl = new Tally();
  for (let i = 0; i < s.nrow; i++) {
    if (s.cols.project[i] < 0 || s.cols.project[i] >= p.nrow) badProject.hit(i);
    if (splitPipe(s.cols.url[i]).some((u) => !FDI_URL_RE.test(u))) badUrl.hit(i);
  }
  badProject.report(out.errors, `fdi.sources.project is not a row of fdi.projects (0..${p.nrow - 1})`);
  badUrl.report(out.errors, 'fdi.sources.url holds a link that is not a clean http(s) URL');

  const agg = (path: 'fdi.flows' | 'fdi.years') => {
    const t = c[path];
    const bad = new Tally();
    for (let i = 0; i < t.nrow; i++) {
      const n = t.cols.n[i];
      const nv = t.cols.n_value[i];
      const v = t.cols.value[i];
      if (n < 1 || nv < 0 || nv > n || (v !== v) !== (nv === 0)) bad.hit(i);
    }
    bad.report(out.errors, `${path}: needs n >= 1, 0 <= n_value <= n, and value NaN exactly when n_value is 0`);
  };
  agg('fdi.flows');
  agg('fdi.years');
  const g = c['fdi.groups'];
  const badGroup = new Tally();
  const sums = new Map<string, number>();
  for (let i = 0; i < g.nrow; i++) {
    const n = g.cols.n[i];
    const nv = g.cols.n_value[i];
    const v = g.cols.value[i];
    if (nv < 0 || nv > n || (v !== v) !== (n > 0 && nv === 0)) badGroup.hit(i);
    const k = `${g.cols.item[i]}|${g.cols.dest[i]}|${g.cols.basis[i]}`;
    sums.set(k, (sums.get(k) ?? 0) + g.cols.share_n[i]);
  }
  badGroup.report(out.errors, 'fdi.groups: needs 0 <= n_value <= n, and value NaN exactly when projects exist but none discloses a value');
  const off = [...sums.values()].filter((x) => Math.abs(x - 1) > 1e-9).length;
  if (off) out.errors.push(`fdi.groups.share_n does not sum to 1 for ${off} (item, dest, basis) splits`);
  let nAll = 0;
  for (let i = 0; i < c['fdi.flows'].nrow; i++) if (c['fdi.flows'].cols.basis[i] === 'all') nAll += c['fdi.flows'].cols.n[i];
  if (c['fdi.flows'].nrow > 0 && nAll !== p.nrow) out.errors.push(`fdi.flows (basis all) counts ${nAll} projects, fdi.projects has ${p.nrow}`);
}

/**
 * Every way a decoded chunk falls short of the contract. `objects` is decoded.flat, decoded.objects
 * or the OsbDecoded itself (then the header's kind and name are checked too). `ref` (from the decoded
 * sector-meta) adds range checks of the i32 indices of other chunks; sector-meta checks its own.
 */
export function checkChunk(name: string, objects: OsbDecoded | Record<string, unknown>, ref?: SectorRefSizes): ChunkCheck {
  const out: ChunkCheck = { errors: [], warnings: [] };
  if (!isSectorChunkName(name)) {
    out.errors.push(`${name} is not a sector chunk name`);
    return out;
  }
  let flat: Record<string, unknown> = objects as Record<string, unknown>;
  if (isDecoded(objects)) {
    if (objects.header.kind !== 'chunk') out.errors.push(`the OSB header kind is ${String(objects.header.kind)}, expected chunk`);
    if (objects.header.name !== name) out.errors.push(`the OSB header name is ${String(objects.header.name)}, expected ${name}`);
    flat = objects.flat;
  }
  const spec = chunkSpec(name);

  let sizes: Partial<SectorRefSizes> = ref ?? {};
  if (name === 'sector-meta') {
    const size = (p: RefTable) => {
      const t = pick(flat, p);
      return isObj(t) && typeof t.nrow === 'number' ? t.nrow : undefined;
    };
    sizes = { 'dims.geo': size('dims.geo'), 'dims.items': size('dims.items'), 'dict.vars': size('dict.vars') };
  }

  const okNodes = new Set<string>();
  for (const [path, node] of Object.entries(spec)) {
    const v = pick(flat, path);
    if (v === undefined || v === null) {
      if (!node.optional) out.errors.push(`${path} is missing`);
      continue;
    }
    if (node.kind === 'strv') {
      if (!Array.isArray(v) || !v.every((s) => typeof s === 'string')) out.errors.push(`${path} must be a character vector without NA`);
      else okNodes.add(path);
    } else if (checkTable(path, node, v, sizes, out)) okNodes.add(path);
  }
  if (out.errors.length) return out;

  const c = flat as Record<string, unknown>;
  const get = (p: string) => pick(c, p);
  if (name === 'sector-meta') checkMeta(Object.fromEntries(Object.keys(spec).map((p) => [p, get(p)])) as SectorMetaChunk, out);
  else if (name === 'sector-facts') {
    checkObsIndex('facts', get('facts.obs'), get('facts.index'), out);
    const notes = get('facts.notes');
    if (notes) checkFactNotes(get('facts.obs'), notes, out);
  }
  else if (name === 'sector-series') checkObsIndex('series', get('series.obs'), get('series.index'), out);
  else if (name.startsWith('sector-trade-')) checkTrade(Object.fromEntries(Object.keys(spec).map((p) => [p, get(p)])) as SectorTradeChunk, out);
  else if (name === 'sector-fdi') checkFdi(Object.fromEntries(Object.keys(spec).map((p) => [p, get(p)])) as SectorFdiChunk, out);
  else if (name === 'sector-dev') checkDev(Object.fromEntries(Object.keys(spec).map((p) => [p, get(p)])) as SectorDevChunk, out);
  else if (name === 'sector-climate') checkClimate(Object.fromEntries(Object.keys(spec).map((p) => [p, get(p)])) as SectorClimateChunk, out);
  return out;
}

/**
 * The typed objects of a decoded chunk, keyed by dotted path (meta['dims.geo'].cols.code), or a
 * SectorContractError listing every problem. Warnings (open vocabularies, expected counts) do not
 * throw; they go to `onWarnings` when given.
 */
export function validateChunk<N extends SectorChunkName>(
  name: N,
  objects: OsbDecoded | Record<string, unknown>,
  ref?: SectorRefSizes,
  onWarnings?: (warnings: string[]) => void,
): ChunkFor<N> {
  const res = checkChunk(name, objects, ref);
  if (res.errors.length) throw new SectorContractError(name, res.errors);
  if (res.warnings.length && onWarnings) onWarnings(res.warnings);
  const flat = isDecoded(objects) ? objects.flat : objects;
  const out: Record<string, unknown> = {};
  for (const path of Object.keys(chunkSpec(name))) {
    const v = pick(flat, path);
    if (v !== undefined && v !== null) out[path] = v;
  }
  return out as ChunkFor<N>;
}
