// Draw the Fence: the data contract written by scripts/89_export_yards_fences_data.R into
// public/data/yards-fences/ and read by this module. Plain JSON (no OSB): the bundle is small.
//
//   manifest.json                 always revalidated: vintage, inputs, validation, file list
//   reference.<h8>.json           metric map, curated components and stacks, sources
//   evidence-<ISO3>.<h8>.json     one country's evidence records, supply and network views

export const YF_SCHEMA = 'osi-yards-fences';
export const YF_SCHEMA_VERSION = 1;

export type Stage = 'Upstream' | 'Midstream' | 'Downstream';
export type ScopeType = 'stage' | 'subcomponent' | 'mineral';

export type YFConcept =
  | 'supplier_concentration'
  | 'coercive_share'
  | 'allied_alternatives'
  | 'foreign_dependency'
  | 'production_depth'
  | 'resource_optionality'
  | 'domestic_position'
  | 'trade_competitiveness'
  | 'production_momentum'
  | 'investment'
  | 'future_demand'
  | 'cost_competitiveness'
  | 'technology_readiness'
  | 'technology_momentum';

export type YFUnit = 'share' | 'hhi' | 'usd' | 'usd_bn' | 'rca' | 'index01' | 'pct_change' | 'pct_pa' | 'trl' | 'trl_points';

export interface YFManifest {
  schema: typeof YF_SCHEMA;
  schema_version: number;
  bundle_id: string;
  created: string;
  git_commit: string | null;
  generator: string;
  inputs: {
    sector_bundle_id: string;
    sector_created: string;
    strategic_bundle_id: string;
    strategic_created: string;
    network_build: string | null;
    pipeline_marker: string | null;
  };
  countries: { iso3: string; name: string; file: string }[];
  reference: string;
  validation: { check: string; ok: boolean; detail?: string }[];
  notes: string[];
}

/** One measured fact about one scope, in the shape the brief specified (YFEvidenceRecord). */
export interface YFEvidenceRecord {
  country: string;
  technology: string;
  stage: Stage;
  scope: ScopeType;
  /** 'batteries-midstream' | 'batteries-midstream::Battery Cells' | 'rare-earths' */
  scopeId: string;
  scopeLabel: string;
  metricId: string;
  /** measure key from the metric map (exports, sub_exports, mfg_capacity, refining, ...) or null */
  measure: string | null;
  concept: YFConcept;
  label: string;
  rawValue?: number | string | null;
  unit: YFUnit;
  /** OSI normalised component value (0-1); set only on normalised metrics */
  normalizedValue?: number | null;
  normalized: boolean;
  percentile?: number | null;
  rank?: number | null;
  nEconomies?: number | null;
  /** who the value is about, for shares: "China", "United States" */
  subject?: string | null;
  year?: number | string | null;
  source?: string | null;
  sourceId?: string | null;
  explanation?: string | null;
  caveats?: string[];
  /** engine input this record may feed (only raw measures) */
  engineInput?: string | null;
  evidenceType: 'data';
}

export interface YFSubcomponent {
  id: string;
  label: string;
  hs6: string[];
  /** some codes are shared with other technologies (X-attributed trade) */
  shared: boolean;
}

export interface YFCell {
  id: string;
  technology: string;
  stage: Stage;
  label: string;
  blurb?: string | null;
  hasTrade: boolean;
  subcomponents: YFSubcomponent[];
  caveats: string[];
}

export interface YFSupplier {
  geo: string;
  name: string;
  tier: 'core' | 'partner' | 'none';
  share: number;
}

export interface YFSupplyView {
  scopeId: string;
  measure: string;
  measureLabel: string;
  year: number;
  source: string;
  top: YFSupplier[];
  hhi: number;
  effN: number;
  china: number;
  core: number;
  partner: number;
  rest: number;
  /** the home country's import sources (stage scope only; from the reconciled bilateral matrix) */
  homeImports?: { year: number; total: number; hhi: number; sources: YFSupplier[] } | null;
}

export interface YFNetworkNode {
  node: string;
  label: string;
  share: number;
  tier: 'core' | 'partner';
  /** country-level Partnership Strength (PSI) with the home country, where defined */
  psi?: number | null;
  psiRank?: number | null;
}

export interface YFNetworkView {
  cell: string;
  method: string;
  lambda: number | null;
  built: string | null;
  designedHhi: number;
  effectiveProducers: number;
  maxShare: number;
  observedHhi: number | null;
  observedTop: { node: string; share: number } | null;
  observedYear: number | null;
  nodes: YFNetworkNode[];
  homeShare: number | null;
  barred: string[];
}

export interface YFPartner {
  iso3: string;
  name: string;
  tier: 'core' | 'partner' | 'none';
  psi: number;
  friendshore: number | null;
  opportunity: number | null;
  rank: number | null;
}

export interface YFCountryBundle {
  schema: string;
  schema_version: number;
  bundle_id: string;
  country: { iso3: string; name: string; tier: 'core' | 'partner' | 'none' };
  cells: YFCell[];
  records: YFEvidenceRecord[];
  supply: YFSupplyView[];
  network: YFNetworkView[];
  partners: YFPartner[];
}

export interface YFComponentAssessment {
  value: string;
}

export interface YFComponent {
  id: string;
  technology: string;
  stage: Stage;
  label: string;
  description: string;
  layer: 'control' | 'hardware' | 'material' | 'fuel' | 'structure';
  /** the evidence scope that best describes this component, or null when none does */
  evidence: { scope: ScopeType; scopeId: string } | null;
  secondary: { scope: ScopeType; scopeId: string }[];
  /** curated indicative answers keyed by engine input id; absent = Unknown */
  assessments: Record<string, string>;
  /** a curated override of the coercion share where trade codes cannot see the component */
  shareOverride: { value: number; subject: string } | null;
  criticalFunction: string | null;
  paperPlacement: 'small' | 'medium' | 'large' | null;
  notes: string;
  source: string;
  confidence: 'low' | 'moderate' | 'high';
}

export interface YFStack {
  id: string;
  label: string;
  technology: string;
  stage: Stage;
  description: string;
  layers: string[];
}

export interface YFMetricDef {
  id: string;
  concept: YFConcept;
  label: string;
  unit: YFUnit;
  engineInput: string | null;
  normalized: boolean;
  explanation: string;
}

export interface YFReference {
  schema: string;
  schema_version: number;
  bundle_id: string;
  metrics: YFMetricDef[];
  measures: Record<string, { var: string; year: number; scope: ScopeType; label: string }>;
  enginePriority: Record<ScopeType, string[]>;
  concepts: Record<YFConcept, { label: string; family: 'security' | 'opportunity' | 'technology' }>;
  components: YFComponent[];
  stacks: YFStack[];
  sources: { id: string; short: string; citation: string | null; url: string | null; licence: string | null; edition: string | null }[];
  hhiRefs: { id: string; value: number; label: string; authority: string; document: string; year: number }[];
}
