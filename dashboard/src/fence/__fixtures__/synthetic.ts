// A small synthetic Draw the Fence bundle for tests. Every number is made up; nothing here is data.

import type { YFCountryBundle, YFEvidenceRecord, YFManifest, YFReference } from '../data/contract.ts';

const rec = (scope: YFEvidenceRecord['scope'], scopeId: string, metricId: string, measure: string | null, rawValue: number, extra: Partial<YFEvidenceRecord> = {}): YFEvidenceRecord => ({
  country: 'USA',
  technology: 'Testtech',
  stage: 'Midstream',
  scope,
  scopeId,
  scopeLabel: `${scopeId} (synthetic)`,
  metricId,
  measure,
  concept: 'supplier_concentration',
  label: `${metricId} (synthetic)`,
  rawValue,
  unit: 'share',
  normalized: false,
  year: 2024,
  source: 'Synthetic source',
  explanation: 'Synthetic.',
  evidenceType: 'data',
  ...extra,
});

const conc = (scope: YFEvidenceRecord['scope'], id: string, measure: string, top: number, home: number, coalition: number): YFEvidenceRecord[] => [
  rec(scope, id, 'top_share', measure, top, { subject: 'Otherland', engineInput: 'topSupplierShare' }),
  rec(scope, id, 'hhi', measure, top * top + 0.05, { unit: 'hhi', engineInput: 'hhi', percentile: 80 }),
  rec(scope, id, 'non_coalition_top', measure, top, { subject: 'Otherland', concept: 'coercive_share', engineInput: 'nonAlliedTopShare' }),
  rec(scope, id, 'coalition_share', measure, coalition, { concept: 'allied_alternatives', engineInput: 'alliedShare' }),
  rec(scope, id, 'home_share', measure, home, { subject: 'United States', concept: 'domestic_position', engineInput: 'homeShare', rank: 4, nEconomies: 50, percentile: 94 }),
];

export const SYN_REFERENCE: YFReference = {
  schema: 'osi-yards-fences-reference',
  schema_version: 1,
  bundle_id: 'synthetic',
  metrics: [],
  measures: {
    exports: { var: 'trade.exports', year: 2024, scope: 'stage', label: 'world exports of the HS6 basket' },
    mfg_capacity: { var: 'manufacturing.share_now', year: 2024, scope: 'stage', label: 'global manufacturing capacity' },
    sub_exports: { var: 'trade.sub_exports', year: 2024, scope: 'subcomponent', label: 'world exports of this sub-component' },
    refining: { var: 'production.refining', year: 2025, scope: 'mineral', label: 'world refined production' },
  },
  // a one-item list written as a bare string, as an unguarded JSON export would: the adapter copes
  enginePriority: { stage: ['mfg_capacity', 'exports'], subcomponent: 'sub_exports' as unknown as string[], mineral: ['refining'] },
  concepts: {
    supplier_concentration: { label: 'Supplier concentration', family: 'security' },
    coercive_share: { label: 'Potentially coercive share', family: 'security' },
    allied_alternatives: { label: 'Trusted alternatives', family: 'security' },
    foreign_dependency: { label: 'Foreign dependency', family: 'security' },
    production_depth: { label: 'Domestic production depth', family: 'security' },
    resource_optionality: { label: 'Resource optionality', family: 'security' },
    domestic_position: { label: 'Present competitive position', family: 'opportunity' },
    trade_competitiveness: { label: 'Trade competitiveness', family: 'opportunity' },
    production_momentum: { label: 'Production momentum', family: 'opportunity' },
    investment: { label: 'Investment', family: 'opportunity' },
    future_demand: { label: 'Future demand', family: 'opportunity' },
    cost_competitiveness: { label: 'Cost competitiveness', family: 'opportunity' },
    technology_readiness: { label: 'Technology readiness', family: 'technology' },
    technology_momentum: { label: 'Technology momentum', family: 'technology' },
  },
  components: [
    {
      id: 'syn-control', technology: 'Testtech', stage: 'Midstream', label: 'Synthetic control software', description: 'Synthetic.', layer: 'control',
      evidence: null, secondary: [], assessments: { digitallyActive: 'yes', remoteControl: 'yes', separability: 'separable', consequence: 'severe', timeHorizon: 'days' },
      shareOverride: null, criticalFunction: 'Synthetic function', paperPlacement: 'small', notes: 'Synthetic.', source: 'Synthetic', confidence: 'moderate',
    },
    {
      id: 'syn-cells', technology: 'Testtech', stage: 'Midstream', label: 'Synthetic cells', description: 'Synthetic.', layer: 'hardware',
      evidence: { scope: 'subcomponent', scopeId: 'testtech-midstream::Cells' }, secondary: [{ scope: 'stage', scopeId: 'testtech-midstream' }],
      assessments: { digitallyActive: 'no', consequence: 'moderate', timeHorizon: 'years', substitutability: 'moderate', qualificationTime: 'm3to12', reconstitutionTime: 'y1to3', surgeCapacity: 'partial', processKnowledge: 'yes', workforceSuppliers: 'yes', adjacentInnovation: 'yes', complementaryCapabilities: 'partly' },
      shareOverride: null, criticalFunction: null, paperPlacement: 'medium', notes: 'Synthetic.', source: 'Synthetic', confidence: 'moderate',
    },
    {
      id: 'syn-magnet', technology: 'Testtech', stage: 'Midstream', label: 'Synthetic magnets', description: 'Synthetic.', layer: 'material',
      evidence: { scope: 'mineral', scopeId: 'synthium' }, secondary: [],
      assessments: { digitallyActive: 'no', consequence: 'severe', timeHorizon: 'months', substitutability: 'hard', qualificationTime: 'y1to3', reconstitutionTime: 'gt3y', surgeCapacity: 'none' },
      shareOverride: null, criticalFunction: null, paperPlacement: 'small', notes: 'Synthetic.', source: 'Synthetic', confidence: 'low',
    },
    {
      id: 'syn-box', technology: 'Testtech', stage: 'Midstream', label: 'Synthetic enclosure', description: 'Synthetic.', layer: 'structure',
      evidence: null, secondary: [], assessments: { digitallyActive: 'no', consequence: 'low' },
      shareOverride: { value: 0.9, subject: 'Curatoria' }, criticalFunction: null, paperPlacement: 'large', notes: 'Synthetic.', source: 'Synthetic', confidence: 'high',
    },
  ],
  stacks: [{ id: 'syn-stack', label: 'Synthetic system', technology: 'Testtech', stage: 'Midstream', description: 'Synthetic.', layers: ['syn-control', 'syn-cells', 'syn-magnet'] }],
  sources: [{ id: 'syn', short: 'Synthetic source', citation: null, url: null, licence: 'synthetic', edition: null }],
  hhiRefs: [],
};

export const SYN_BUNDLE: YFCountryBundle = {
  schema: 'osi-yards-fences-evidence',
  schema_version: 1,
  bundle_id: 'synthetic',
  country: { iso3: 'USA', name: 'United States', tier: 'core' },
  cells: [{ id: 'testtech-midstream', technology: 'Testtech', stage: 'Midstream', label: 'Testtech — Midstream', hasTrade: true, subcomponents: [{ id: 'testtech-midstream::Cells', label: 'Cells', hs6: ['999999'], shared: false }], caveats: [] }],
  records: [
    ...conc('stage', 'testtech-midstream', 'exports', 0.3, 0.08, 0.6),
    // the preferred measure, on which the home country is not listed (no value, no engine input)
    ...conc('stage', 'testtech-midstream', 'mfg_capacity', 0.7, 0, 0.2).map((r) => (r.metricId === 'home_share' ? { ...r, rawValue: null, engineInput: null } : r)),
    rec('stage', 'testtech-midstream', 'trl', null, 8.5, { unit: 'trl', concept: 'technology_readiness', engineInput: 'trl' }),
    ...conc('subcomponent', 'testtech-midstream::Cells', 'sub_exports', 0.6, 0.06, 0.38),
    ...conc('mineral', 'synthium', 'refining', 0.85, 0.05, 0.12),
  ],
  supply: [
    { scopeId: 'testtech-midstream', measure: 'exports', measureLabel: 'world exports of the HS6 basket', year: 2024, source: 'Synthetic source', top: [{ geo: 'OTH', name: 'Otherland', tier: 'none', share: 0.3 }], hhi: 0.14, effN: 7, china: 0.3, core: 0.4, partner: 0.2, rest: 0.1, homeImports: null },
  ],
  network: [
    { cell: 'testtech-midstream', method: 'milp', lambda: 0.78, built: '2026-10-12', designedHhi: 0.15, effectiveProducers: 6.7, maxShare: 0.3, observedHhi: 0.12, observedTop: { node: 'EUU', share: 0.2 }, observedYear: 2024, nodes: [{ node: 'USA', label: 'United States', share: 0.3, tier: 'core', psi: null, psiRank: null }, { node: 'JPN', label: 'Japan', share: 0.2, tier: 'core', psi: 0.9, psiRank: 5 }], homeShare: 0.3, barred: [] },
  ],
  partners: [{ iso3: 'GBR', name: 'United Kingdom', tier: 'core', psi: 0.97, friendshore: 0.97, opportunity: 0.97, rank: 1 }],
};

export const SYN_MANIFEST: YFManifest = {
  schema: 'osi-yards-fences',
  schema_version: 1,
  bundle_id: 'synthetic',
  created: '2026-10-14T00:00:00Z',
  git_commit: null,
  generator: 'synthetic fixture',
  inputs: { sector_bundle_id: 'synthetic', sector_created: '2026-10-14T00:00:00Z', strategic_bundle_id: 'synthetic', strategic_created: '2026-10-14T00:00:00Z', network_build: '2026-10-12', pipeline_marker: null },
  countries: [{ iso3: 'USA', name: 'United States', file: 'evidence-USA.synthetic.json' }],
  reference: 'reference.synthetic.json',
  validation: [{ check: 'synthetic', ok: true }],
  notes: ['Synthetic fixture.'],
};
