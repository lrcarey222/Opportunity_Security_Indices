// Tests of the threat-amelioration derivation (qc/threat.ts): only exported fields are read, everything else is
// null with a note; a zero-gain host never counts as an intervention; diversified / shifted / unchanged against
// the bundle's 0.40 concentration line; strategic and operational compacts; and the real AIB bundle.

import { describe, expect, it } from 'vitest';
import type { AibInvestmentHost } from '../../aib/contract.ts';
import { concentrationLine, dependenceOutcome, deriveCompactThreat, deriveNodeThreat, effectiveHosts, TIME_TO_CAPACITY_NOTE } from './threat.ts';
import { HAVE_REAL, loadReal, mkAibDoc, mkNode } from './__tests__/fixtures.ts';

const host = (node: string, iso: string, rank: number, gain: number): AibInvestmentHost =>
  ({ node, host: iso, rank, gain, before: 0.8, after: 0.8 + gain, newCapacityUsed: gain, servedUsd: 1, hostIncrement: gain, type: 'scaling', rankBasis: 'scaling host first' }) as AibInvestmentHost;

describe('dependenceOutcome against the concentration line', () => {
  const before = { largest_supplier: 'CHN', largest_share: 0.5 };
  it('diversified: the largest share falls and ends below the line', () => {
    expect(dependenceOutcome(before, { largest_supplier: 'AUS', largest_share: 0.3 }, 0.4)).toBe('diversified');
  });
  it('shifted: a different single supplier ends at or above the line', () => {
    expect(dependenceOutcome(before, { largest_supplier: 'AUS', largest_share: 0.45 }, 0.4)).toBe('shifted');
    expect(dependenceOutcome(before, { largest_supplier: 'AUS', largest_share: 0.4 }, 0.4)).toBe('shifted');
  });
  it('unchanged: the same supplier stays above the line; unknown when a share is not exported', () => {
    expect(dependenceOutcome(before, { largest_supplier: 'CHN', largest_share: 0.45 }, 0.4)).toBe('unchanged');
    expect(dependenceOutcome(before, { largest_supplier: null, largest_share: null }, 0.4)).toBe('unknown');
  });
  it('reads the line from the bundle meta (manage_largest_share)', () => {
    expect(concentrationLine(mkAibDoc([]))).toBe(0.4);
    const notes: string[] = [];
    expect(concentrationLine({ meta: {} } as never, notes)).toBe(0.4);
    expect(notes[0]).toMatch(/missing/);
  });
});

describe('deriveNodeThreat', () => {
  const node = mkNode({ id: 'n1', exposure: 0.4, static: 0.6, existing: 0.7, optimized: 0.85, dividend: 0.15, residual: 0.15, demandUsd: 2e9, lostUsd: 8e8, largest: 'AUS', largestShare: 0.3, hhi: 0.18, n1Reoptimized: 0.7, diversion: 0.05, hosts: ['PNG', 'DEU'] });
  const doc = mkAibDoc([node], { investmentFlows: { hosts: [host('n1', 'PNG', 1, 0), host('n1', 'DEU', 2, 0.06)] } });
  const t = deriveNodeThreat(doc, 'n1', 'decoupling')!;
  it('before / after from exported fields only', () => {
    expect(t.before).toMatchObject({ hhi: 0.35, largest_supplier: 'CHN', largest_share: 0.5, top3_share: 0.8, coverage: 0.6, risky_share: 0.4 });
    expect(t.after).toMatchObject({ hhi: 0.18, largest_supplier: 'AUS', largest_share: 0.3, coverage: 0.85, n1_coverage: 0.7 });
    expect(t.remaining_gap).toBe(0.15);
    expect(t.remaining_gap_usd).toBeCloseTo(0.15 * 2e9);
    expect(t.lost_supply_replaced_share).toBeCloseTo((0.85 - 0.6) / 0.4);
    expect(t.dependence_outcome).toBe('diversified');
  });
  it('never invents a field the bundle does not export', () => {
    expect(t.after.top3_share).toBeNull();
    expect(t.after.risky_share).toBeNull();
    expect(t.before.n_credible_suppliers).toBeNull();
    expect(t.after.n_credible_suppliers).toBeNull();
    expect(t.time_to_capacity_years).toBeNull();
    expect(t.notes).toContain(TIME_TO_CAPACITY_NOTE);
    expect(t.notes.join(' ')).toMatch(/top3_share/);
  });
  it('a zero-gain host is not counted as an intervention', () => {
    expect(effectiveHosts(doc.investmentFlows?.hosts, 'n1').zeroGain.map((h) => h.host)).toEqual(['PNG']);
    expect(t.intervention).not.toMatch(/PNG/);
    expect(t.intervention).toMatch(/DEU/);
    expect(t.new_capacity_share).toBe(0.06);
    expect(t.notes.join(' ')).toMatch(/zero simulated gain.*PNG/);
  });
  it('shifted when another single supplier ends above the line', () => {
    const d2 = mkAibDoc([mkNode({ id: 'n2', largest: 'IDN', largestShare: 0.55 })]);
    expect(deriveNodeThreat(d2, 'n2', 'decoupling')!.dependence_outcome).toBe('shifted');
  });
  it('baseline: no shock, no intervention; managed without a diversify record: unknown; absent node: null', () => {
    expect(deriveNodeThreat(doc, 'n1', 'baseline')!).toMatchObject({ intervention: 'none', dependence_outcome: 'unchanged' });
    expect(deriveNodeThreat(doc, 'n1', 'managed')!.dependence_outcome).toBe('unknown');
    expect(deriveNodeThreat(doc, 'nope', 'decoupling')).toBeNull();
  });
});

describe('deriveCompactThreat', () => {
  const sc = {
    id: 'SC-TEST', name: 'Test compact', purpose: 'p', core: [], steps: [],
    scenarios: {
      decoupling: {
        coverage: { static: 0.6, existing: 0.7, coordinated: 0.8, postInvestment: 0.9 }, lostUsd: 4e9, exposure: 0.4, demandUsd: 1e10,
        criticalExternal: ['VNM'], nonAlliedReliedOn: ['CHN'], buildHosts: ['DEU'], buildHostTies: 0, missions: [], counts: {},
      },
    },
  };
  const doc = mkAibDoc([], { strategic: { compacts: [sc] } });
  it('a strategic compact: coverage path, gap and the post-investment caveat; no concentration invented', () => {
    const t = deriveCompactThreat(doc, 'SC-TEST', 'decoupling')!;
    expect(t.allied_coverage).toBe(0.8);
    expect(t.remaining_gap).toBeCloseTo(0.2);
    expect(t.remaining_gap_usd).toBeCloseTo(0.2 * 1e10);
    expect(t.new_capacity_share).toBeCloseTo(0.1);
    expect(t.before.hhi).toBeNull();
    expect(t.dependence_outcome).toBe('unknown');
    expect(t.notes.join(' ')).toMatch(/arithmetic, not a finding/);
    expect(t.notes.join(' ')).toMatch(/CHN/);
  });
  it('a missing scenario record is blank with a note; an unknown compact is null', () => {
    expect(deriveCompactThreat(doc, 'SC-TEST', 'managed')!.notes.join(' ')).toMatch(/no managed scenario record/);
    expect(deriveCompactThreat(doc, 'SC-NONE', 'decoupling')).toBeNull();
  });
});

describe.skipIf(!HAVE_REAL)('threat derivation on the shipped AIB bundle', () => {
  it('derives every node under Decoupling with no invented field and no zero-gain host as intervention', () => {
    const { decoded } = loadReal();
    const a = decoded.aib!;
    for (const n of a.doc.nodes) {
      const t = deriveNodeThreat(a.doc, n.id, 'decoupling', { managed: a.managed })!;
      expect(t.time_to_capacity_years).toBeNull();
      expect(t.after.top3_share).toBeNull();
      for (const z of effectiveHosts(a.doc.investmentFlows?.hosts, n.id).zeroGain) expect(t.intervention).not.toContain(`${z.host},`);
    }
    for (const c of a.doc.strategic?.compacts ?? []) for (const s of ['decoupling', 'managed'] as const) expect(deriveCompactThreat(a.doc, c.id, s)).not.toBeNull();
  });
});
