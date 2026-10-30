// A small schema-2 posture bundle in the exporter's shape (scripts/91; design section 3): three countries, two
// technologies, the six stages, and a store that serves it without the network. Aland (AAA) carries every
// choice of the framework: Secure by each rationale (chokepoint, systemic resilience, operational control),
// Preserve (one with the review flag of design D2), Catch Up (credible pathway), Partner (misaligned policy),
// Deploy · Accelerate (deployment gap), Deploy · Watch at a producing stage and at Deployment, and a node not
// assessed; its detail document covers the supply-chain and deployment need families (with basis and
// scenario), the prize components, the Competitive Position families, the pathway evidence, the compact trace
// and the policy evidence, in the field spellings detail.ts accepts (flat and nested). `economies` renames the
// three (AAA, BBB, CCC) so a page's route test can serve a posture for a real economy of its own fixture.

import { PostureStore } from '../store.ts';

export const TECHS = ['Batteries', 'Solar'];
export const STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'];
const DEP = 4;

interface Row {
  c: number; t: number; s: number; p: 'S' | 'C' | 'D' | 'N'; v: string | null; m: string | null; dm: string | null;
  need: number | null; prize: number | null; cp: number | null; share?: number | null; sec?: string | null; pw?: string | null;
  bd?: string | null; conf?: string | null; fl?: string | null; ev?: string | null; al?: string | null; alc?: string | null; alf?: string | null;
  p1?: string | null; gap?: boolean | null; mom?: string | null; mq?: string | null;
}

export function fixtureRows(): Row[] {
  const rows: Row[] = [];
  for (let c = 0; c < 3; c++)
    for (let t = 0; t < 2; t++)
      for (let s = 0; s < 6; s++) {
        rows.push(
          s === DEP
            ? { c, t, s, p: 'D', v: 'deploy_watch', m: null, dm: 'watch', need: 0.3, prize: null, cp: 0.3, al: 'aligned', alc: 'medium', ev: 'high', conf: 'high', mq: 'ok' }
            : { c, t, s, p: 'D', v: 'deploy_watch', m: null, dm: 'watch', need: 0.2, prize: 0.2, cp: 0.3, al: 'aligned', alc: 'medium', ev: 'high', conf: 'high', pw: 'no', mq: 'ok' },
        );
      }
  const set = (c: number, t: number, s: number, r: Partial<Row>) => Object.assign(rows.find((x) => x.c === c && x.t === t && x.s === s) as Row, r);
  // Aland: every choice
  set(0, 0, 0, { p: 'C', v: 'compete_partner', m: 'partner', dm: null, need: 0.8, prize: 0.3, cp: 0.2, share: 0.004, pw: 'no', al: 'misaligned', alc: 'high', alf: 'partner_autarkic', p1: 'BBB', gap: false, mom: 'up' });
  set(0, 0, 1, { p: 'S', v: 'secure_input', dm: null, sec: 'coercive_chokepoint', need: 0.95, prize: 0.4, cp: 0.1, share: 0.002, pw: null, al: 'under_prioritized' });
  set(0, 0, 2, { p: 'C', v: 'compete_catch_up', m: 'catch_up', dm: null, need: 0.5, prize: 0.8, cp: 0.4, share: 0.03, pw: 'yes', mom: 'flat' });
  set(0, 0, 3, { p: 'C', v: 'compete_preserve', m: 'preserve', dm: null, need: 0.4, prize: 0.9, cp: 0.95, share: 0.12, pw: null, ev: 'low', bd: 'need' });
  set(0, 0, 4, { v: 'deploy_accelerate', dm: 'accelerate', need: 0.85, cp: 0.5, al: 'deployment_gap', alc: 'low' });
  set(0, 0, 5, { p: 'N', v: null, dm: null, need: null, prize: 0.3, cp: null, pw: null, al: null, alc: null, ev: null, conf: 'low' });
  set(0, 1, 1, { p: 'S', v: 'secure_resilience', dm: null, sec: 'systemic_resilience', need: 0.9, prize: 0.5, cp: 0.2, pw: null });
  set(0, 1, 2, { p: 'S', v: 'secure_control', dm: null, sec: 'operational_control', need: 0.7, prize: 0.65, cp: 0.3, pw: null, al: 'mixed', alf: 'sources_disagree', conf: 'moderate' });
  set(0, 1, 3, { p: 'C', v: 'compete_preserve', m: 'preserve', dm: null, need: 0.8, prize: 0.3, cp: 0.9, share: 0.08, pw: null, fl: 'review_strong_position_low_prize', conf: 'moderate' });
  set(0, 1, 4, { need: 0.4, al: 'over_prioritized' });
  set(0, 1, 5, { need: 0.1, prize: 0.15, cp: 0.05, ev: 'none', al: 'insufficient_data', alc: null });
  // Bland: Batteries Components Catch Up, Batteries Deployment Accelerate; Cland: Batteries Components Partner with a gap, Solar Refining not assessed
  set(1, 0, 2, { p: 'C', v: 'compete_catch_up', m: 'catch_up', dm: null, need: 0.9, prize: 0.9, cp: 0.6, pw: 'yes' });
  set(1, 0, 4, { v: 'deploy_accelerate', dm: 'accelerate', need: 0.9 });
  set(2, 0, 2, { p: 'C', v: 'compete_partner', m: 'partner', dm: null, need: 0.85, prize: 0.1, cp: 0.2, gap: true, pw: 'no' });
  set(2, 1, 1, { p: 'N', v: null, dm: null, prize: null, pw: null, al: null });
  return rows;
}

/** The node phrases of the strategy sentence (meta.phrases); the other nodes fall back to "<technology> <stage>". */
export const PHRASES: Record<string, string> = {
  'Batteries|Extraction': 'lithium and nickel mining',
  'Batteries|Refining': 'battery materials refining',
  'Batteries|Components': 'battery cell manufacturing',
  'Batteries|Assembly': 'battery pack assembly',
  'Batteries|Deployment': 'battery storage',
  'Solar|Refining': 'polysilicon refining',
  'Solar|Components': 'solar inverters',
  'Solar|Assembly': 'solar module assembly',
  'Solar|Deployment': 'solar power',
};

export function fixtureCore(momentum = true): Record<string, unknown> {
  const rows = fixtureRows();
  const col = <K extends keyof Row>(k: K) => rows.map((r) => (r[k] === undefined ? null : r[k]));
  return {
    schema: 'osi-posture', schema_version: 2, created: '2026-10-29T00:00:00Z', git_commit: 'abc1234',
    meta: {
      techs: TECHS, stages: STAGES, benchmark_period: '2022-2024',
      momentum_period: momentum ? 'Sep 2025 - Aug 2026' : null, momentum_previous_period: momentum ? 'Sep 2024 - Aug 2025' : null,
      momentum_through: momentum ? 'Aug 2026' : null, momentum_band: 0.1,
      thresholds: { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.67, borderline_band: 0.05 },
      scenario_labels: { current_policy: 'Current policy', national_commitments: 'National commitments', climate_aligned: 'Climate-aligned' },
      family_scenarios: { demand: 'current_policy', policy_gap: 'national_commitments', system: null },
      phrases: PHRASES,
      core_members: 2, partner_members: 1,
    },
    countries: { iso3: ['AAA', 'BBB', 'CCC'], name: ['Aland', 'Bland', 'Cland'], tier: ['core', 'core', 'partner'] },
    cells: {
      c: col('c'), t: col('t'), s: col('s'), p: col('p'), v: col('v'), m: col('m'), dm: col('dm'), sec: col('sec'), need: col('need'), prize: col('prize'), cp: col('cp'),
      share: col('share'), pw: col('pw'), bd: col('bd'), conf: col('conf'), fl: col('fl'), ev: col('ev'), al: col('al'), alc: col('alc'), alf: col('alf'),
      p1: col('p1'), gap: col('gap'), mom: col('mom'), mq: col('mq'),
    },
    division: {
      t: [0, 0, 0, 1], s: [0, 1, 2, 3], primary: ['BBB', 'BBB', 'BBB', null], primary_share: [0.4, 0.3, 0.2, null], backup_1: ['CCC', 'AAA', null, null],
      backup_1_share: [0.1, 0.1, null, null], backup_2: [null, null, null, null], backup_2_share: [null, null, null, null], redundancy: [3, 2, 1, 0],
      allied_share: [0.6, 0.4, 0.2, 0.01], coercive_share: [0.2, 0.5, 0.7, 0.9], leader: ['BBB', 'XXX', 'XXX', 'XXX'], leader_share: [0.4, 0.5, 0.7, 0.9],
      gap: ['none', 'insufficient_redundancy', 'single_point_of_failure', 'no_trusted_supplier'],
      gaps: [null, 'insufficient_redundancy;weak_evidence', 'single_point_of_failure;insufficient_redundancy', 'no_trusted_supplier;weak_evidence'],
      source: ['aib_position_share', 'aib_position_share', 'world_share', 'world_share'], aib_nodes: [1, 2, null, null], partner_demand: [1, 0, 2, 1],
      secure_demand: [0, 1, 0, 0], partner_gaps: [0, 0, 1, 1], member_partner_demand: [1, 0, 2, 1],
    },
    aib_header: { core_members: 2, members: 3, priority_nodes: 4, covered_share: 0.25, unresolved_gaps: 3 },
    design: {
      'secure_input|mixed': { fence: 'Screen coercive suppliers of refined battery materials.', build: 'Allied refining offtake and a strategic stock.', gate: 'Qualify trusted refiners.' },
      'compete_partner|allied': { build: 'Long-term offtake with trusted miners.' },
    },
  };
}

/** The fixture core with one cell's columns changed (economy, technology, stage indices; e.g. { cp: 0.875 }), as the columnar document. */
export function withCell(core: Record<string, unknown>, c: number, t: number, s: number, patch: Record<string, unknown>): Record<string, unknown> {
  const cells = core.cells as Record<string, unknown[]>;
  const i = cells.c.findIndex((_, k) => cells.c[k] === c && cells.t[k] === t && cells.s[k] === s);
  return { ...core, cells: Object.fromEntries(Object.entries(cells).map(([k, v]) => [k, k in patch ? v.map((x, j) => (j === i ? patch[k] : x)) : v])) };
}

/** The fixture core without one cell (economy, technology, stage indices), as the columnar document. */
export function withoutCell(core: Record<string, unknown>, c: number, t: number, s: number): Record<string, unknown> {
  const cells = core.cells as Record<string, unknown[]>;
  const i = cells.c.findIndex((_, k) => cells.c[k] === c && cells.t[k] === t && cells.s[k] === s);
  return { ...core, cells: Object.fromEntries(Object.entries(cells).map(([k, v]) => [k, v.filter((_, j) => j !== i)])) };
}

const trace = (...steps: [string, string, number | string | null, number | string | null, string][]) =>
  steps.map(([step, test, value, threshold, result]) => ({ step, test, value, threshold, result }));

/** Aland's detail document: all twelve nodes, in the spellings detail.ts accepts (flat and nested). */
export function fixtureDetail(): Record<string, unknown> {
  const trade = { benchmark_period: '2022-2024', benchmark_share: 0.05, benchmark_rca: 1.4, benchmark_exports: 2.5e9, momentum_raw: 0.3, momentum_quality: 'ok' };
  return {
    schema: 'osi-posture', schema_version: 2, created: '2026-10-29T00:00:00Z', iso3: 'AAA', country: 'Aland',
    cells: [
      {
        tech: 'Batteries', stage: 'Extraction', posture: 'compete', variant: 'compete_partner', mode: 'partner', delivery: 'allied', confidence: 'high',
        need_type: 'supply_chain', need: 0.8, import_dependence: 0.9, import_hhi: 0.7, china_bloc: 0.6, trusted_alternatives: 0.5,
        top_supplier: 'XXX', top_supplier_share: 0.55, china_share: 0.4, opposite_bloc_share: 0.45,
        industrial_opportunity: 0.3, io_node_prize: 0.35, io_maturity: 9, io_basis: 'global_context',
        cp: 0.2, home_share: 0.004, cp_export_position: 0.2, cp_resource_endowment: 0.1,
        pathway_credible: 'no', pathway_basis: [],
        partners: [{ iso3: 'BBB', capability: 0.2, cp: 0.8, relationship: 0.9, diversification: 0.05 }, { iso3: 'CCC', capability: 0.06, cp: 0.5, relationship: 0.7 }],
        partner_gap: false, partner_aib_status: 'covered', partner_reason: 'Bland holds 20% of world supply.', partner_evidence: 'moderate', partner_source: 'aib_position_share',
        trace: trace(['1', 'Secure override', 'no route met', null, 'no'], ['3', 'Need', 0.8, 0.67, 'high'], ['3', 'Prize', 0.3, 0.6, 'low'], ['3', 'Competitive Position', 0.2, 0.75, 'weak'], ['3', 'Choice', null, null, 'partner']),
        policy: { priority: 0.85, support: 0.2, protection: 0.9, protection_recorded: true, momentum: 'rising', alignment: 'misaligned', alignment_confidence: 'high', flag: 'partner_autarkic', sources: ['NIPO', 'IEA PAMS'], n_sources: 2 },
        trade: { ...trade, momentum_raw: 0.3 },
      },
      {
        tech: 'Batteries', stage: 'Refining', posture: 'secure', variant: 'secure_input', security_rationale: 'coercive_chokepoint', security_routes: 'coercive_chokepoint',
        delivery: 'mixed', mitigation: 'Diversify refining through allied offtake and hold a strategic stock.', confidence: 'high',
        need: 0.95, sn_import_dependence: 0.95, sn_supplier_concentration: 0.9, sn_risky_supplier_exposure: 0.92,
        coercive_top: 'XXX', coercive_top_share: 0.7, coercive_import_share: 0.8, trusted_share: 0.2, substitutability: 'hard', reconstitution_time: 'y1to3', stockpile: 'unknown',
        prize: { value: 0.4, node_prize: 0.45, basis: 'global_context' }, cp: 0.1,
        alignment: 'under_prioritized', alignment_confidence: 'medium', policy_priority: 0.2,
        trace: trace(['1', 'Secure override', 'coercive chokepoint', null, 'secure']),
        trade: { benchmark_period: '2022-2024', benchmark_share: 0.02, momentum_quality: 'incomplete_coverage' },
      },
      {
        tech: 'Batteries', stage: 'Components', posture: 'compete', variant: 'compete_catch_up', mode: 'catch_up', delivery: 'domestic',
        need: 0.5, import_dependence: 0.6, import_hhi: 0.4, china_bloc: 0.5,
        industrial_opportunity: 0.8, io_node_prize: 0.85, io_investment_momentum: 0.7, io_home_market: 0.75, io_basis: 'national',
        cp: 0.4, cp_export_position: 0.35, cp_manufacturing_capacity: 0.5, cp_investment_position: 0.6,
        pathway_credible: 'yes', pathway_basis: 'feasibility;pipeline',
        trace: trace(['3', 'Prize', 0.8, 0.6, 'high'], ['3', 'Competitive Position', 0.4, 0.75, 'weak'], ['3', 'Credible pathway', 'feasibility; pipeline', null, 'yes'], ['3', 'Choice', null, null, 'catch_up']),
        // support, protection and momentum are within-country percentile ranks; no protection measure is recorded here (its rank is a tie among zeros)
        policy: { priority: 0.6, support: 0.6, protection: 0.3, protection_recorded: false, momentum: 0.12, alignment: 'aligned', alignment_confidence: 'medium', sources: 'NIPO' },
        trade: { ...trade, momentum_raw: 0.02 },
      },
      {
        tech: 'Batteries', stage: 'Assembly', posture: 'compete', variant: 'compete_preserve', mode: 'preserve', delivery: 'domestic', borderline: 'need',
        need: 0.4, import_dependence: 0.3, import_hhi: 0.2, china_bloc: 0.2,
        industrial_opportunity: 0.9, io_node_prize: 0.9, io_basis: 'global_context',
        cp: 0.95, home_share: 0.12, cp_export_position: 0.9, cp_production_position: 0.95, cp_manufacturing_capacity: 0.9,
        policy: { priority: 0.7, support: 0.8, protection: 0.2, alignment: 'aligned', alignment_confidence: 'high', sources: ['NIPO', 'IEA PAMS'] },
        trade,
      },
      {
        tech: 'Batteries', stage: 'Deployment', posture: 'deploy', variant: 'deploy_accelerate', deploy_mode: 'accelerate', delivery: 'diversified_global',
        need_type: 'deployment', need: 0.85,
        dn_demand: { value: 0.9, basis: 'national', scenario: 'current_policy' },
        dn_policy_gap: 0.8, dn_policy_gap_basis: 'national', dn_policy_gap_scenario: 'national_commitments',
        dn_system: { value: null, basis: 'not_assessed' },
        // as R writes it (scripts/95): a ratio to current policies, not a 0-1 score
        dn_climate_aligned: { value: 1.04, basis: 'global_context', note: 'World 2035, Net Zero vs Current Policies', unit: 'ratio_vs_current_policies' },
        deployment_trend: 'rising',
        cp: 0.5, cp_deployment_scale: 0.5,
        trace: trace(['2', 'Deployment need', 0.85, 0.67, 'high'], ['2', 'Choice', null, null, 'accelerate']),
        policy: { priority: 0.2, alignment: 'deployment_gap', alignment_confidence: 'low', sources: ['IEA PAMS'], n_sources: 1 },
      },
      { tech: 'Batteries', stage: 'Recovery', posture: 'not_assessed', not_assessed_reason: 'Supply-chain need is not measured for this node.', industrial_opportunity: 0.3 },
      {
        tech: 'Solar', stage: 'Extraction', posture: 'deploy', variant: 'deploy_watch', deploy_mode: 'watch',
        // trusted_alternatives is a need score: low means many trusted alternatives (R supply_chain_need.R)
        need: 0.2, import_dependence: 0.1, import_hhi: 0.2, china_bloc: 0.1, trusted_alternatives: 0.2,
        industrial_opportunity: 0.2, cp: 0.3, pathway_credible: 'no',
        // the engine's reasons as R writes them (decision_engine.R sp_watch_reasons): a ";"-separated string
        watch_reasons: 'low_need;low_prize;ample_diversified_supply',
        policy: { priority: 0.4, alignment: 'aligned', alignment_confidence: 'medium' },
      },
      {
        tech: 'Solar', stage: 'Refining', posture: 'secure', variant: 'secure_resilience', security_rationale: 'systemic_resilience', delivery: 'allied',
        need: 0.9, import_dependence: 0.9, import_hhi: 0.85, china_bloc: 0.95, reconstitution_time: 'gt3y', systemic_essentiality: 'high',
        industrial_opportunity: 0.5, cp: 0.2,
        policy: { priority: 0.7, support: 0.7, alignment: 'aligned', alignment_confidence: 'medium' },
      },
      {
        tech: 'Solar', stage: 'Components', posture: 'secure', variant: 'secure_control', security_rationale: 'operational_control', delivery: 'domestic',
        mitigation: 'Trusted inverter firmware and remote-access controls.', operational_control: 'digitally active; remote access',
        need: 0.7, import_dependence: 0.7, import_hhi: 0.6, china_bloc: 0.8, industrial_opportunity: 0.65, cp: 0.3,
        policy: { priority: 0.5, support: 0.6, protection: 0.4, alignment: 'mixed', alignment_confidence: 'low', flag: 'sources_disagree' },
      },
      {
        tech: 'Solar', stage: 'Assembly', posture: 'compete', variant: 'compete_preserve', mode: 'preserve', delivery: 'domestic', consistency_flag: 'review_strong_position_low_prize',
        need: 0.8, import_dependence: 0.8, import_hhi: 0.7, china_bloc: 0.7, industrial_opportunity: 0.3, io_node_prize: 0.3, io_outlook: 'oversupplied',
        // the structural (benchmark) share only where momentum moved the export position (R posture_export.R drops an identical one)
        cp: 0.9, home_share: 0.08, cp_export_position: 0.85, cp_export_position_structural: 0.7,
        policy: { priority: 0.6, support: 0.6, alignment: 'aligned', alignment_confidence: 'medium' },
      },
      {
        tech: 'Solar', stage: 'Deployment', posture: 'deploy', variant: 'deploy_watch', deploy_mode: 'watch', need_type: 'deployment', need: 0.4,
        dn_demand: 0.4, dn_demand_basis: 'observed_trend', dn_demand_scenario: 'current_policy', dn_policy_gap: 0.3, dn_policy_gap_basis: 'national', dn_policy_gap_scenario: 'national_commitments',
        dn_system: 0.5, dn_system_basis: 'national', deployment_trend: 0.08, watch_reasons: 'low_deployment_need',
        policy: { priority: 0.9, protection: 0.85, alignment: 'over_prioritized', alignment_confidence: 'low' },
      },
      {
        tech: 'Solar', stage: 'Recovery', posture: 'deploy', variant: 'deploy_watch', deploy_mode: 'watch', need: 0.1, industrial_opportunity: 0.15, cp: 0.05, alignment: 'insufficient_data',
        // the reasons as an array; option value marks an emerging technology
        watch_reasons: ['low_need', 'low_prize', 'option_value'],
      },
    ],
  };
}

/** A new economy for a fixture one: its code and, optionally, its name. */
export type FixtureEconomies = Partial<Record<'AAA' | 'BBB' | 'CCC', { iso3: string; name?: string }>>;

/** The fixture's documents with AAA, BBB and CCC renamed (codes everywhere they appear; names where given). */
export function renameEconomies<T>(doc: T, economies: FixtureEconomies): T {
  const pairs = Object.entries(economies) as [string, { iso3: string; name?: string }][];
  const code = (v: unknown): unknown => (typeof v === 'string' ? (economies[v as 'AAA']?.iso3 ?? v) : v);
  const NAME: Record<string, string> = { AAA: 'Aland', BBB: 'Bland', CCC: 'Cland' };
  const nameOf = (v: unknown): unknown => {
    const hit = pairs.find(([k]) => NAME[k] === v);
    return hit?.[1].name ?? v;
  };
  const walk = (v: unknown, key?: string): unknown => {
    if (Array.isArray(v)) return v.map((x) => walk(x, key));
    if (v && typeof v === 'object') return Object.fromEntries(Object.entries(v).map(([k, x]) => [k, walk(x, k)]));
    if (key === 'name' || key === 'country') return nameOf(v);
    return code(v);
  };
  return walk(doc) as T;
}

export function fixtureStore(opts: { momentum?: boolean; core?: Record<string, unknown>; economies?: FixtureEconomies } = {}): PostureStore {
  const ren = <T,>(d: T): T => (opts.economies ? renameEconomies(d, opts.economies) : d);
  const aaa = opts.economies?.AAA?.iso3 ?? 'AAA';
  const files: Record<string, unknown> = {
    'manifest.json': { schema: 'osi-posture', schema_version: 2, created: 'x', core: 'posture-core.1.json', details: { [aaa]: 'posture-detail-AAA.1.json' }, counts: {} },
    'posture-core.1.json': ren(opts.core ?? fixtureCore(opts.momentum ?? true)),
    'posture-detail-AAA.1.json': ren(fixtureDetail()),
  };
  return new PostureStore('/x/', async (url: string) => {
    const f = url.replace('/x/', '');
    return f in files ? new Response(JSON.stringify(files[f]), { status: 200 }) : new Response('', { status: 404 });
  });
}
