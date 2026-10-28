// A small posture bundle in the exporter's shape (scripts/91): three countries, two technologies, the six
// stages, and a store that serves it without the network.

import { PostureStore } from '../store.ts';

export const TECHS = ['Batteries', 'Solar'];
export const STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Deployment', 'Recovery'];

interface Row {
  c: number; t: number; s: number; p: 'S' | 'C' | 'D'; v: string; m: string | null; q: string | null;
  need: number | null; opp: number | null; cp: number | null; share?: number | null; al?: string | null; sec?: string | null;
  p1?: string | null; gap?: boolean | null; mom?: string | null; bd?: string | null; fl?: string | null; ev?: string | null;
}

export function fixtureRows(): Row[] {
  const rows: Row[] = [];
  for (let c = 0; c < 3; c++)
    for (let t = 0; t < 2; t++)
      for (let s = 0; s < 6; s++) rows.push({ c, t, s, p: 'D', v: 'deploy', m: null, q: 'watch', need: 0.2, opp: 0.2, cp: 0.3, al: 'aligned', ev: 'high' });
  const set = (c: number, t: number, s: number, r: Partial<Row>) => Object.assign(rows.find((x) => x.c === c && x.t === t && x.s === s) as Row, r);
  // AAA: Batteries Refining Secure (chokepoint), Batteries Components Partner, Solar Assembly Preserve
  set(0, 0, 1, { p: 'S', v: 'secure_input', m: null, q: 'partner', need: 0.95, opp: 0.4, sec: 'coercive_chokepoint', al: 'under_prioritized' });
  set(0, 0, 2, { p: 'C', v: 'compete_partner', m: 'partner', q: 'partner', need: 0.8, opp: 0.3, cp: 0.9, p1: 'BBB', gap: false, mom: 'up', fl: 'partner_despite_capability' });
  set(0, 1, 3, { p: 'C', v: 'compete_preserve', m: 'preserve', q: 'preserve', need: 0.4, opp: 0.9, cp: 0.95, ev: 'low', bd: 'need' });
  // BBB: Batteries Components Catch up; CCC: Batteries Components Partner with a gap
  set(1, 0, 2, { p: 'C', v: 'compete_catch_up', m: 'catch_up', q: 'catch_up', need: 0.9, opp: 0.9, cp: 0.6 });
  set(2, 0, 2, { p: 'C', v: 'compete_partner', m: 'partner', q: 'partner', need: 0.85, opp: 0.1, cp: 0.2, gap: true });
  // CCC: Solar Refining with no measured opportunity
  set(2, 1, 1, { q: 'insufficient_data', opp: null });
  return rows;
}

export function fixtureCore(momentum = true): Record<string, unknown> {
  const rows = fixtureRows();
  const col = <K extends keyof Row>(k: K) => rows.map((r) => (r[k] === undefined ? null : r[k]));
  return {
    schema: 'osi-posture', schema_version: 1, created: '2026-10-28T00:00:00Z',
    meta: {
      techs: TECHS, stages: STAGES, benchmark_period: '2022-2024',
      momentum_period: momentum ? 'Sep 2025 - Aug 2026' : null, momentum_previous_period: momentum ? 'Sep 2024 - Aug 2025' : null,
      momentum_through: momentum ? 'Aug 2026' : null, momentum_band: 0.1, need_threshold: 0.67, opportunity_threshold: 0.67, borderline_band: 0.05,
      core_members: 2, partner_members: 1,
    },
    countries: { iso3: ['AAA', 'BBB', 'CCC'], name: ['Aland', 'Bland', 'Cland'], tier: ['core', 'core', 'partner'] },
    cells: {
      c: col('c'), t: col('t'), s: col('s'), p: col('p'), v: col('v'), m: col('m'), q: col('q'), need: col('need'), opp: col('opp'), cp: col('cp'),
      share: col('share'), al: col('al'), alf: rows.map(() => null), conf: rows.map(() => 'high'), ev: col('ev'), sec: col('sec'), bd: col('bd'),
      fl: col('fl'), p1: col('p1'), gap: col('gap'), mom: col('mom'), mq: rows.map(() => 'ok'),
    },
    division: {
      t: [0, 0, 1], s: [1, 2, 3], primary: ['BBB', 'BBB', null], primary_share: [0.3, 0.2, null], backup_1: ['AAA', null, null], backup_1_share: [0.1, null, null],
      backup_2: [null, null, null], backup_2_share: [null, null, null], redundancy: [2, 1, 0], allied_share: [0.4, 0.2, 0.01], coercive_share: [0.5, 0.7, 0.9],
      leader: ['XXX', 'XXX', 'XXX'], leader_share: [0.5, 0.7, 0.9], gap: ['insufficient_redundancy', 'single_point_of_failure', 'no_trusted_supplier'],
      gaps: ['insufficient_redundancy;weak_evidence', 'single_point_of_failure;insufficient_redundancy', 'no_trusted_supplier;weak_evidence'],
      source: ['aib_position_share', 'world_share', 'world_share'], aib_nodes: [2, null, null], partner_demand: [0, 2, 1], secure_demand: [1, 0, 0],
      partner_gaps: [0, 1, 1], member_partner_demand: [0, 2, 1],
    },
    aib_header: { core_members: 2, members: 3, priority_nodes: 3, covered_share: 0.333, unresolved_gaps: 3 },
  };
}

export function fixtureDetail(): Record<string, unknown> {
  return {
    schema: 'osi-posture', schema_version: 1, created: '2026-10-28T00:00:00Z', iso3: 'AAA', country: 'Aland',
    cells: [
      { tech: 'Batteries', stage: 'Refining', posture: 'secure', variant: 'secure_input', security_rationale: 'coercive_chokepoint', delivery: 'allied', need: 0.95, opportunity: 0.4,
        trade: { benchmark_period: '2022-2024', benchmark_share: 0.02, momentum_quality: 'incomplete_coverage' } },
      { tech: 'Batteries', stage: 'Components', posture: 'compete', variant: 'compete_partner', mode: 'partner', quadrant: 'partner', need: 0.8, opportunity: 0.3, cp: 0.9,
        partners: [{ iso3: 'BBB', capability: 0.2, cp: 0.6, relationship: 0.9, diversification: 0.05 }], partner_gap: false, partner_reason: 'BBB holds 20% of world supply.',
        partner_evidence: 'low', partner_source: 'world_share', consistency_flag: 'partner_despite_capability',
        trade: { benchmark_period: '2022-2024', benchmark_share: 0.05, benchmark_rca: 1.4, momentum_period: 'Sep 2025 - Aug 2026', momentum_previous_period: 'Sep 2024 - Aug 2025', momentum_raw: 0.3 } },
      { tech: 'Solar', stage: 'Extraction', posture: 'deploy', variant: 'deploy', quadrant: 'watch', need: 0.2, opportunity: 0.2 },
    ],
  };
}

export function fixtureStore(opts: { momentum?: boolean; core?: Record<string, unknown> } = {}): PostureStore {
  const files: Record<string, unknown> = {
    'manifest.json': { schema: 'osi-posture', schema_version: 1, created: 'x', core: 'posture-core.1.json', details: { AAA: 'posture-detail-AAA.1.json' }, counts: {} },
    'posture-core.1.json': opts.core ?? fixtureCore(opts.momentum ?? true),
    'posture-detail-AAA.1.json': fixtureDetail(),
  };
  return new PostureStore('/x/', async (url: string) => {
    const f = url.replace('/x/', '');
    return f in files ? new Response(JSON.stringify(files[f]), { status: 200 }) : new Response('', { status: 404 });
  });
}
