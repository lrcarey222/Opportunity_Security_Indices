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
  dial_grid: DialGrid;
  chunks: ChunkInfo[];
  budget?: Record<string, unknown>;
  encoding?: Record<string, unknown>;
  paper?: { title: string | null; url: string | null; pdf: string | null };
  [k: string]: unknown;
}

// ---- Baseline inputs (bundle$manifest$baseline_inputs, strings unboxed) ---------------------------
// The original allied network design is retired (2026-10-17): no network inputs, parameters,
// advanced settings or chunk (the Allied Industrial Base analysis replaces it).

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
  [k: string]: unknown;
}

// ---- Parameters (unclass(dash_params()); strings unboxed) -----------------------------------------

export const DIAL_NAMES = ['hawkishness', 'bullishness', 'populism', 'greenness'] as const;
export type DialName = (typeof DIAL_NAMES)[number];
export type Dials = Record<DialName, number>;

export type Fossil = 'auto' | 'never' | 'always';
export type PsiView = 'coalition' | 'world';

export interface Advanced {
  es_weights: NumRecord;
  eo_weights: NumRecord;
  tau: number | null;
  fossil: Fossil;
  /** the endowment screen: an Upstream stage without recorded reserves or production is never an allied pick */
  require_resource_base: boolean;
  psi_view: PsiView;
}

/** What the URL carries: dials and (possibly partial) advanced settings. */
export interface ScenarioSpec {
  dials: Partial<Dials>;
  advanced?: Partial<Advanced>;
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
    /** which measure each cell's outbound component holds: 'imf_dip' (the IMF position `outbound`) or
     * 'cm_fdi' (the CM-FDI investment tie); absent = IMF everywhere (a build before CM-FDI) */
    outbound_source?: Str;
    /** CM-FDI investment ties [U, U, K] of the K 'cm_fdi' cells, in cell order */
    outbound_cmfdi?: NDArray<Float64Array>;
    /** chinese_parent_exposure [U (partner), C]: the share of the partner's foreign-invested
     * manufacturing owned by China-headquartered parents, a risk scored 1 - share; NA = undefined */
    cpe?: NDArray<Float64Array>;
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
  /** baseline.* of every loaded chunk (core, vars, partner) */
  baseline: Record<string, unknown>;
  partner?: PartnerInputs;
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
