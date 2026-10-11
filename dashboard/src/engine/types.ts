// Types of the bundle, the parameters and the results (spec_react.md 6.1). The bundle mirrors the
// R bundle (R/dashboard/dash_bundle.R) as the chunks of R/dashboard/dash_web.R carry it.

import type { Column, NDArray, Table } from './codec.ts';

export type { Column, NDArray, Table };
export type Str = (string | null)[];
export type NumRecord = Record<string, number>;

// ---- Manifest -------------------------------------------------------------------------------------

export interface DialGrid {
  d: number[];
  m: number[];
  stage_weights: Record<string, number[]>;
}

export interface ChunkInfo {
  name: string;
  file: string;
  bytes: number;
  bytes_gz: number;
  md5: string;
  initial: boolean;
}

export interface Manifest {
  schema: string;
  schema_version: number;
  bundle_id: string;
  created: string;
  git_commit: string | null;
  osi_branch: string | null;
  flags: Record<string, boolean | null>;
  notes: string[];
  validation: Record<string, unknown>[];
  sources: Record<string, unknown>[];
  sources_params?: unknown;
  baseline_inputs: Record<string, unknown>;
  baseline_params: Record<string, unknown>;
  share_rank_digits?: number;
  dial_grid: DialGrid;
  chunks: ChunkInfo[];
  budget?: Record<string, unknown>;
  encoding?: Record<string, unknown>;
  paper?: { title: string | null; url: string | null; pdf: string | null };
  [k: string]: unknown;
}

// ---- Baseline inputs (bundle$manifest$baseline_inputs, strings unboxed) ---------------------------

export interface Portfolio {
  enable: boolean;
  top_k: number;
  min_cap: number;
  max_cap: number;
  transform: string;
  max_iters: number;
}

export interface BaselineInputs {
  es_weights: NumRecord;
  eo_weights: NumRecord;
  tau: number | null;
  allow_partial_categories: boolean;
  policy_weights: NumRecord;
  policy_iea_var: string;
  policy_cat_var: string;
  si_weights: NumRecord;
  stage_weights: NumRecord;
  excluded_sectors: string[];
  allied_excluded_techs: string[] | null;
  /** the endowment screen of the allied picks at the baseline (absent = on) */
  require_resource_base?: boolean;
  fossil_techs: string[];
  fossil_threshold: number;
  fs_weights: NumRecord;
  fs_penalty: number;
  po_weights: NumRecord;
  po_penalty: number;
  psi_weights: NumRecord;
  psi_view: 'coalition' | 'world';
  psi_tau: number | null;
  top_k: number;
  node_weights: { eo: number; policy: number; resilience: number };
  node_policy: boolean;
  node_policy_weight_max: number;
  edge_weights: { friendshore: number; opportunity: number };
  demand_weights: { need: number; size: number };
  lambda: number;
  max_share: number;
  min_share: number;
  min_producers: number;
  min_suppliers_per_consumer: number;
  epsilon_supplier_share: number;
  w_node: number;
  w_edge: number;
  w_dev: number;
  allow_self: boolean;
  portfolio: Portfolio;
  diversity_breakpoint_step: number;
  supply_chains: string[];
  milp_time_limit: number;
  policy_missing: string;
  missing_components: string;
  demand_mode: string;
  size_transform: string;
  [k: string]: unknown;
}

// ---- Parameters (unclass(dash_params()); strings unboxed) -----------------------------------------

export const DIAL_NAMES = ['hawkishness', 'bullishness', 'populism', 'greenness'] as const;
export type DialName = (typeof DIAL_NAMES)[number];
export type Dials = Record<DialName, number>;

export type Fossil = 'auto' | 'never' | 'always';
export type PsiView = 'coalition' | 'world';

export interface AdvancedNetwork {
  lambda: number | null;
  max_share: number;
  min_producers: number;
  portfolio_enable: boolean;
}

export interface Advanced {
  es_weights: NumRecord;
  eo_weights: NumRecord;
  tau: number | null;
  fossil: Fossil;
  /** the endowment screen: an Upstream stage without recorded reserves or production is never an allied pick */
  require_resource_base: boolean;
  psi_view: PsiView;
  network: AdvancedNetwork;
}

/** What the URL carries: dials and (possibly partial) advanced settings. */
export interface ScenarioSpec {
  dials: Partial<Dials>;
  advanced?: Partial<Omit<Advanced, 'network'>> & { network?: Partial<AdvancedNetwork> };
}

export interface NetworkParams {
  node_weights: { eo: number; policy: number; resilience: number };
  node_policy: boolean;
  edge_weights: { friendshore: number; opportunity: number };
  demand_weights: { need: number; size: number };
  diversity_lambda: number;
  self_multiplier: number;
  max_share: number;
  min_share: number;
  min_producers: number;
  min_suppliers_per_consumer: number;
  epsilon_supplier_share: number;
  w_node: number;
  w_edge: number;
  w_dev: number;
  allow_self: boolean;
  portfolio: Portfolio;
  diversity_breakpoint_step: number;
  supply_chains: string[];
  exclude_techs: string[];
  milp_time_limit: number;
  policy_missing: string;
  missing_components: string;
  demand_mode: string;
  size_transform: string;
}

export interface Params {
  dials: Dials;
  m: { H: number; B: number; P: number; G: number };
  advanced: Advanced;
  pillars: { es_weights: NumRecord; eo_weights: NumRecord; tau: number | null };
  policy: { weights: NumRecord };
  si: {
    weights: NumRecord;
    stage_weights: NumRecord;
    excluded_sectors: string[];
    fossil_excluded: boolean;
    fossil_techs: string[];
    require_resource_base: boolean;
  };
  partner: {
    fs_weights: NumRecord;
    fs_penalty: number;
    po_weights: NumRecord;
    po_penalty: number;
    psi_weights: NumRecord;
    psi_view: PsiView;
    psi_tau: number | null;
    top_k: number;
    exclude_techs: string[];
  };
  network: NetworkParams;
}

// ---- Bundle ---------------------------------------------------------------------------------------

export interface PillarInputs {
  cells: Table; // Country, tech, supply_chain, iso3c, sovereign
  categories: string[];
  score: NDArray<Float64Array>;
  present: NDArray<Uint8Array>;
  imputed: NDArray<Uint8Array>;
  missing_rule: NDArray<Str>;
  cw_imp: NDArray<Float64Array>;
  cw_tot: NDArray<Float64Array>;
  n_var: Int32Array;
  categories_in_data: string[];
  allow_partial_categories: boolean;
  var?: Table; // vars chunk
}

export interface Dims {
  countries: Table;
  nodes: Table;
  techs: string[];
  stages: string[];
  fossil_techs: string[];
  es_categories: string[];
  eo_categories: string[];
  cells: Table; // cell_id, tech, supply_chain, sector
  U: string[];
  U_names: string[];
  U_sovereign: Uint8Array;
}

export interface PartnerInputs {
  U: string[];
  names: string[];
  sovereign: Uint8Array;
  cells: Table;
  fs: {
    imp_trade: NDArray<Float64Array>;
    econ_opp_raw?: NDArray<Float64Array>;
    es_need?: NDArray<Float64Array>;
    eo_partner?: NDArray<Float64Array>;
    outbound?: NDArray<Float64Array>;
    geo?: NDArray<Float64Array>;
    mask: NDArray<Uint8Array>;
    cpi: Float64Array;
    gh: Float64Array;
  };
  po: {
    trade: NDArray<Float64Array>;
    econ_opp_index: NDArray<Float64Array>;
    partner_es_need: NDArray<Float64Array>;
    mask: NDArray<Uint8Array>;
    cpi: Float64Array;
    gh: Float64Array;
  };
  dp: Table;
  top5_share: { fs: NDArray<Float64Array>; po: NDArray<Float64Array> };
  psi: { coalition_reporters: string[]; tau: number | null };
  geo_dyad?: Record<string, unknown>;
}

export interface NetworkInputs {
  members: Table; // iso3c, tier, network_node, node_tier (checked, scripts/30 order)
  node_raw_member: Table;
  member_weights: Table;
  node_size: Table;
  caps: Table;
  country_size?: Table;
  node_trade?: Table | null;
  node_labels?: Record<string, string> | null;
  country_info: Table;
  country_map: Table;
  stored?: Record<string, unknown> | null;
  greedy_baseline?: Record<string, unknown> | null;
  observed_hhi?: Table | null;
}

export interface Bundle {
  manifest: Manifest;
  baselineInputs: BaselineInputs;
  baselineParams: Params;
  dims: Dims;
  pillars: { es: PillarInputs; eo: PillarInputs };
  /** required: the components a country needs somewhere to be scored (absent = every weighted one);
   * components: the pipeline's component order, for the policy_basis labels (absent = variables) */
  policy: { cells: Table; variables: string[]; values: NDArray<Float64Array>; required?: string[] | null; components?: string[] | null };
  /** resource_base: the endowment screen (Country, tech, supply_chain, resource_base, resource_evidence;
   * absent = no screen) */
  si: { tech_ghg: Table; trl: Table | null; allies: Table; resource_base?: Table | null };
  /** baseline.* of every loaded chunk (core, vars, partner, network) */
  baseline: Record<string, unknown>;
  partner?: PartnerInputs;
  network?: NetworkInputs;
  /** chunks loaded so far */
  loaded: Set<string>;
}

// ---- Results --------------------------------------------------------------------------------------

export interface PillarsResult {
  es: Table;
  eo: Table;
  es_category_weight: NDArray<Float64Array>;
  eo_category_weight: NDArray<Float64Array>;
  es_contrib?: Table;
  eo_contrib?: Table;
}

export interface PolicyResult {
  index: Table;
  eligibility: Table;
  weights: NumRecord;
}

export interface SiResult {
  cells: Table;
  excluded: Table;
  weights: NumRecord;
  stage_weights: NumRecord;
  degenerate: Table;
}

export interface Top5Result {
  fs: Float64Array; // [U * U]
  po: Float64Array;
  fs_exists: Uint8Array;
  po_exists: Uint8Array;
}

export interface PsiResult {
  dyad: Table;
  partner: Table;
  reporters: string[];
  view: PsiView;
}

export interface PartnerCells {
  fs_index: Float64Array; // [U * U * C]
  po_index: Float64Array;
}
