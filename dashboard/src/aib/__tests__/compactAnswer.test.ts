// The first screen of a Strategic Sectoral Compact (derive/compactAnswer.ts, 2026-11-25): the bottleneck (the
// BUILD steps, largest lost supply first; else the most exposed steps), who contributes (the core countries
// grouped by role and step, then the named hosts) and the remaining gap (1 - coverage after coordinated trade
// and after simulated new capacity), with the steps the scenario could not test as an evidence limit. Missing
// evidence reads as such, never as zero. On the real export (when present): every compact and scenario
// gives three lines without a placeholder number.

import { describe, expect, it } from 'vitest';
import type { AibStrategicCompactScenario } from '../contract.ts';
import { compactAnswer, gapWords, type AnswerStep } from '../derive/compactAnswer.ts';
import { nodeScenario } from '../derive/scenarios.ts';
import { compactNodeIds } from '../derive/strategic.ts';
import { haveFixture, readDoc, REAL_DIR } from './fixture.ts';

const NAMES: Record<string, string> = { AAA: 'Avalon', BBB: 'Borduria', CCC: 'Carpania', DDD: 'Dorvalia' };
const name = (x: string) => NAMES[x] ?? x;
const core = [
  { iso3: 'AAA', role: 'Resource supplier', step: 'Minerals' },
  { iso3: 'BBB', role: 'Resource supplier', step: 'Minerals' },
  { iso3: 'CCC', role: 'Demand anchor', step: 'Cells & packs' },
];
const steps: AnswerStep[] = [
  { id: 'a', label: 'Lithium minerals', status: null, lostUsd: 1e9 },
  { id: 'b', label: 'Cobalt ores', status: null, lostUsd: null },
  { id: 'c', label: 'Cells & packs', status: null, lostUsd: 5e9 },
  { id: 'd', label: 'Separators', status: null, lostUsd: 2e9 },
];

function scn(over: Partial<AibStrategicCompactScenario> = {}): AibStrategicCompactScenario {
  return {
    counts: { build: 2, coordinate: 0, manage: 0, low: 1, insufficient: 1, notStressed: 0, notTested: 0, noShock: 0 },
    needs: { build: 2, coordinate: 0, manage: 0 },
    missions: [],
    statement: '',
    policyMix: [],
    menu: [],
    demandUsd: 20e9,
    lostUsd: 8e9,
    exposure: 0.4,
    coverage: { static: 0.6, existing: 0.62, coordinated: 0.65, postInvestment: 0.8 },
    activeWorkstreams: [],
    criticalPartners: null,
    criticalExternal: null,
    coalition: null,
    buildHosts: ['DDD'],
    classMix: null,
    headline: '',
    nodes: [
      ['a', 'BUILD', 0.5, null, null],
      ['b', 'INSUFFICIENT DATA', null, null, null],
      ['c', 'BUILD', 0.4, 'DDD', 1],
      ['d', 'LOW INTERVENTION', 0.9, null, null],
    ],
    ...over,
  };
}

describe('compactAnswer', () => {
  it('three lines from the compact’s own fields', () => {
    const a = compactAnswer({ core }, scn(), steps, 'Decoupling', name, false);
    expect(a.bottleneck).toBe('Cells & packs and lithium minerals need new capacity (Build). 40% of the compact’s allied demand ($8.0 bn) is lost under Decoupling.');
    expect(a.who).toBe('Avalon and Borduria (resource suppliers, minerals); Carpania (demand anchor, cells & packs). New capacity is best placed in Dorvalia under Decoupling.');
    expect(a.gap).toBe('35% of the compact’s allied demand is not met with coordinated trade, 20% after simulated new capacity. Evidence limit: 1 step has too little data to test (Cobalt ores).');
  });

  it('Managed Interdependence: supply has to shift; untested steps are an evidence limit', () => {
    const sc = scn({ nodes: [['a', 'NOT TESTED', null, null, null], ['b', 'NOT TESTED', null, null, null], ['c', 'BUILD', 0.4, null, 2], ['d', 'NOT STRESSED', 1, null, null]], buildHosts: [] });
    const a = compactAnswer({ core }, sc, steps, 'Managed Interdependence', name, true);
    expect(a.bottleneck).toBe('Cells & packs needs new capacity (Build). 40% of the compact’s allied demand ($8.0 bn) has to shift under Managed Interdependence.');
    expect(a.who).not.toMatch(/best placed/);
    expect(a.gap).toMatch(/Evidence limit: 2 concentrated steps are not tested under Managed Interdependence\.$/);
  });

  it('no step to build: the most exposed steps with their status; no loss: says so', () => {
    const sc = scn({ nodes: [['a', 'COORDINATE', 0.9, null, null], ['b', 'LOW INTERVENTION', 1, null, null], ['c', 'COORDINATE', 0.95, null, null], ['d', 'LOW INTERVENTION', 1, null, null]] });
    expect(compactAnswer({ core }, sc, steps, 'Decoupling', name, false).bottleneck).toMatch(/^No step needs new capacity under Decoupling; the most supply is lost in cells & packs \(.+\) and separators \(.+\)\./);
    const none = compactAnswer({ core }, scn({ exposure: 0, lostUsd: 0 }), steps.map((x) => ({ ...x, lostUsd: 0 })), 'Decoupling', name, false);
    expect(none.gap).toBe('No allied demand is lost under Decoupling, so no gap remains. Evidence limit: 1 step has too little data to test (Cobalt ores).');
  });

  it('missing evidence is never zero', () => {
    const a = compactAnswer({ core: [] }, null, steps, 'Decoupling', name, false);
    expect(a.bottleneck).toBe('No data for this compact under Decoupling.');
    expect(a.who).toBe('No core countries are named for this compact.');
    expect(a.gap).toBe('Remaining gap: not assessed under Decoupling.');
    const noInv = compactAnswer({ core }, scn({ coverage: { static: 0.6, existing: 0.6, coordinated: 0.6, postInvestment: null } }), steps, 'Partial', name, false);
    expect(noInv.gap).toMatch(/^40% of the compact’s allied demand is not met with coordinated trade; new capacity is not simulated under Partial\./);
    expect(gapWords(0.003)).toBe('Less than 1%');
    expect(gapWords(0)).toBe('None');
    const tiny = compactAnswer({ core }, scn({ coverage: { static: 0.9, existing: 0.99, coordinated: 0.997, postInvestment: 1 } }), steps, 'Decoupling', name, false);
    expect(tiny.gap).toMatch(/^Less than 1% of the compact’s allied demand is not met with coordinated trade, none after simulated new capacity\./);
  });
});

describe.skipIf(!haveFixture(REAL_DIR))('compactAnswer on the real export (public/data/aib)', () => {
  const real = readDoc(REAL_DIR);
  it.skipIf(!real.strategic)('every compact under Managed Interdependence and Decoupling: three lines, no dash for a number', () => {
    const nodes = new Map(real.nodes.map((n) => [n.id, n]));
    for (const c of real.strategic!.compacts) {
      for (const sid of ['managed', 'decoupling'] as const) {
        const st = compactNodeIds(c).map((id) => {
          const n = nodes.get(id);
          const r = n ? nodeScenario(n, sid) : null;
          return { id, label: n?.label ?? id, status: null, lostUsd: r?.ran ? (r.lostUsd ?? null) : null };
        });
        const a = compactAnswer(c, c.scenarios[sid], st, sid, (x) => real.countries[x] ?? x, sid === 'managed');
        for (const line of [a.bottleneck, a.who, a.gap]) {
          expect(line.length, `${c.id} ${sid}`).toBeGreaterThan(20);
          expect(line, `${c.id} ${sid}`).not.toMatch(/–|NaN|undefined/);
        }
      }
    }
  });
});
