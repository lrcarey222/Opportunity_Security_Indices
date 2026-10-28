// The posture bundle contract and the headline derivations (src/posture/contract.ts, derive.ts).

import { describe, expect, it } from 'vitest';
import { checkPostureDetail, decodeCore, PostureContractError } from '../contract.ts';
import { aibHeader, aibTakeaway, countPostures, countryCells, countryTakeaway, freshnessLine, sectorTakeaway } from '../derive.ts';
import { MODE_MARK, POSTURE_COLOR, POSTURE_LABEL } from '../grammar.ts';
import { postureMomentum } from '../momentum.ts';
import { fixtureCore, fixtureDetail, fixtureStore } from './fixture.ts';

describe('the posture contract', () => {
  it('decodes the columnar core into six-stage cells', () => {
    const core = decodeCore(fixtureCore());
    expect(core.cells).toHaveLength(36);
    expect(new Set(core.cells.map((c) => c.stage)).size).toBe(6);
    const s = core.cells.find((c) => c.iso3 === 'AAA' && c.tech === 'Batteries' && c.stage === 'Refining');
    expect(s?.posture).toBe('S');
    expect(s?.rationale).toBe('coercive_chokepoint');
    expect(core.division[1].gap).toBe('single_point_of_failure');
    expect(core.division[0].backups.map((b) => b.iso3)).toEqual(['AAA']);
  });
  it('refuses three-stage data, an unknown posture and a newer schema', () => {
    const three = fixtureCore();
    (three.meta as { stages: string[] }).stages = ['Upstream', 'Midstream', 'Downstream'];
    expect(() => decodeCore(three)).toThrow(PostureContractError);
    const bad = fixtureCore();
    (bad.cells as { p: string[] }).p[0] = 'X';
    expect(() => decodeCore(bad)).toThrow(/unknown posture/);
    const newer = { ...fixtureCore(), schema_version: 9 };
    expect(() => decodeCore(newer)).toThrow(/newer app/);
    const mode = fixtureCore();
    (mode.cells as { m: (string | null)[] }).m[0] = 'watch';
    expect(() => decodeCore(mode)).toThrow(/mode/);
  });
  it('checks the detail document', () => {
    expect(checkPostureDetail(fixtureDetail()).cells).toHaveLength(3);
    const bad = fixtureDetail();
    (bad.cells as { stage: string }[])[0].stage = 'Midstream';
    expect(() => checkPostureDetail(bad)).toThrow(PostureContractError);
  });
  it('the store fetches, checks and caches', async () => {
    const st = fixtureStore();
    const a = await st.core();
    expect(await st.core()).toBe(a);
    expect(st.peek('core')).toBe(a);
    expect((await st.detail('AAA')).country).toBe('Aland');
    await expect(st.detail('ZZZ')).rejects.toThrow(/No posture detail/);
  });
});

describe('the headline derivations', () => {
  const core = decodeCore(fixtureCore());
  it('counts postures and modes, Watch only among Deploy', () => {
    const n = countPostures(countryCells(core, 'AAA'));
    expect([n.S, n.C, n.D]).toEqual([1, 2, 9]);
    expect([n.partner, n.preserve, n.catch_up]).toEqual([1, 1, 0]);
    expect(n.watch).toBe(9);
  });
  it('the country takeaway names the Secure rationale and the Compete mix', () => {
    const t = countryTakeaway('Aland', countryCells(core, 'AAA'));
    expect(t).toMatch(/^Aland should secure 1 node \(Batteries refining; coercive chokepoint\)/);
    expect(t).toMatch(/compete in 2 nodes \(1 partner, 1 preserve/);
    expect(t).toMatch(/deploy openly in the other 9\.$/);
    const none = countryTakeaway('Bland', countryCells(core, 'BBB').filter((c) => c.posture === 'D'));
    expect(none).toMatch(/deploy openly in all/);
  });
  it('the sector takeaway counts each quadrant in the node', () => {
    const cells = core.cells.filter((c) => c.tech === 'Batteries' && c.stage === 'Components');
    const t = sectorTakeaway('Batteries', 'Components', cells, { AAA: 'Aland', BBB: 'Bland', CCC: 'Cland' });
    expect(t).toContain('1 have the need and opportunity to catch up');
    expect(t).toContain('2 need it without the opportunity, and should partner');
  });
  it('the AIB takeaway leads with the worst gap among nodes members rely on', () => {
    expect(aibTakeaway(core.division, null)).toMatch(/3 of the 3 nodes .* have a gap; the most pressing is Solar assembly \(no trusted supplier\)/);
    expect(aibTakeaway(core.division, 'Geothermal')).toMatch(/^No country's posture asks/);
    expect(aibHeader(core)).toEqual({ coreMembers: 2, coveredShare: 0.333, unresolvedGaps: 3, priorityNodes: 3 });
  });
  it('the freshness line states the benchmark and the signal, or why there is none', () => {
    expect(freshnessLine(core.meta)).toEqual({ benchmark: 'Structural trade benchmark: 2022–24', signal: 'Current signal through: Aug 2026' });
    const none = decodeCore(fixtureCore(false));
    expect(freshnessLine(none.meta).signal).toBe('Current momentum unavailable — incomplete matched-period reporting');
  });
  it('momentum direction follows the exporter rule', () => {
    expect([postureMomentum(0.2), postureMomentum(0.05), postureMomentum(-0.3), postureMomentum(null)]).toEqual(['up', 'flat', 'down', null]);
  });
  it('one grammar: three postures, three modes', () => {
    expect(Object.keys(POSTURE_COLOR)).toEqual(['S', 'C', 'D']);
    expect(Object.values(POSTURE_LABEL)).toEqual(['Secure', 'Compete', 'Deploy']);
    expect(Object.keys(MODE_MARK).sort()).toEqual(['catch_up', 'partner', 'preserve']);
  });
});
