import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo, fixtureItem } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import { regionOf, stateCounts, stateOf, stateShort, stateWords } from './states.ts';

const fx = buildSectorFixture();
const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
const v = (id: string) => model.varById.get(id)!;
const geo = (c: string) => model.geos[fixtureGeo(c)];

describe('value states (3.5)', () => {
  it('not covered: no row and outside the roster', () => {
    const reserves = v('resources.mineral_reserves');
    const li = fixtureItem('mineral', 'lithium');
    const jpn = geo('JPN');
    const s = stateOf(facts.value(reserves.idx, li, 2025, jpn.idx), jpn, reserves, model.rosterOf(reserves), 2025);
    expect(s.state).toBe('not_covered');
    expect(stateWords(s, reserves, model)).toMatch(/^Not in USGS MCS \(5 economies\)/);
  });

  it('shipped states: zero, nq, nsr, mirror, est, na_growth', () => {
    const reserves = v('resources.mineral_reserves');
    const li = fixtureItem('mineral', 'lithium');
    const z = stateOf(facts.value(reserves.idx, li, 2025, fixtureGeo('RUS')), geo('RUS'), reserves, null, 2025);
    expect(z.state).toBe('zero');
    const nq = stateOf(facts.value(reserves.idx, li, 2025, fixtureGeo('IDN')), geo('IDN'), reserves, null, 2025);
    expect(nq.state).toBe('nq');
    expect(stateShort(nq, reserves)).toBe('withheld');
    const fuels = v('production.fuels');
    const nsr = stateOf(facts.value(fuels.idx, fixtureItem('fuel', 'oil'), 2025, fixtureGeo('VNM')), geo('VNM'), fuels, null, 2025);
    expect(stateWords(nsr, fuels, model)).toMatch(/Not reported separately by EI Statistical Review/);
    const pc = v('consumption.pc_change');
    const ng = stateOf(facts.value(pc.idx, fixtureItem('fuel', 'oil'), 2025, fixtureGeo('VNM')), geo('VNM'), pc, null, 2025);
    expect(stateShort(ng, pc)).toBe('new');
  });

  it('old: obs_year older than stale_years', () => {
    const w = v('prices.wages');
    const mfg = fixtureItem('activity', 'manufacturing');
    const vnm = stateOf(facts.value(w.idx, mfg, 2025, fixtureGeo('VNM')), geo('VNM'), w, null, 2025);
    expect(vnm.old).toBe(true);
    expect(vnm.obsYear).toBe(2016);
    const usa = stateOf(facts.value(w.idx, mfg, 2025, fixtureGeo('USA')), geo('USA'), w, null, 2025);
    expect(usa.old).toBe(false);
  });

  it('regional: a region value is never a country value', () => {
    const ev = v('trade.exports');
    const reg = stateOf(null, geo('R:iea:Europe'), ev, null, 2024);
    expect(reg.state).toBe('regional');
    expect(regionOf(model, geo('DEU'))?.code).toBe('R:iea:Europe');
  });

  it('counts for the result line', () => {
    const reserves = v('resources.mineral_reserves');
    const c = stateCounts(facts.rows(reserves.idx, fixtureItem('mineral', 'lithium'), 2025), model, reserves);
    expect(c).toMatchObject({ withData: 3, zero: 1, noFigure: 1 });
    expect(c.notCovered).toBe(model.countries.length - 5);
  });
});
