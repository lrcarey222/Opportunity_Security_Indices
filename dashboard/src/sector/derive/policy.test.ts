import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { countsByCountry, countsByInstrument, countsByYear, instrumentOf, policyRecords, policyTexts, safeUrl, scopePolicies, statusClass, tagBit } from './policy.ts';

const fx = buildSectorFixture();
const chunk = validateChunk('sector-policy', fx.chunks['sector-policy']);
const recs = policyRecords(chunk);
const tags = chunk['policy.tags'];

describe('policy masks (4.3 S10)', () => {
  it('decodes tech and stage masks', () => {
    const eu = recs.find((r) => r.geo === fixtureGeo('EUU') && r.tags.length === 2)!;
    expect(eu.tags).toEqual(['Batteries', 'Critical minerals']);
    expect(eu.stages).toEqual(['extraction', 'assembly']);
  });

  it('scopes by technology tag and stage; the cross-cutting toggle adds economy-wide records', () => {
    const bit = tagBit(tags, 'Batteries');
    expect(bit).toBe(0);
    const mid = scopePolicies(recs, { bit, stage: 'assembly', cross: false });
    expect(mid.every((r) => r.tags.includes('Batteries') && r.stages.includes('assembly'))).toBe(true);
    expect(mid).toHaveLength(8);
    // the six stage bits decode on their own: the materials-processing grant is Refining and Assembly
    const ref = scopePolicies(recs, { bit, stage: 'refining', cross: false });
    expect(ref.map((r) => r.stages)).toEqual([['refining', 'assembly']]);
    expect(scopePolicies(recs, { bit, stage: 'recovery', cross: false })).toEqual([]);
    const any = scopePolicies(recs, { bit, stage: null, cross: false });
    expect(any.length).toBeGreaterThan(mid.length);
    const withCross = scopePolicies(recs, { bit, stage: 'assembly', cross: true });
    expect(withCross.length).toBe(mid.length + 2);
    expect(scopePolicies(recs, { bit, stage: null, cross: false, geo: fixtureGeo('USA') })).toHaveLength(3);
  });

  it('counts by country, year (announced after the data year) and instrument', () => {
    const scoped = scopePolicies(recs, { bit: 0, stage: null, cross: false });
    const byC = countsByCountry(scoped);
    expect(byC[0]).toMatchObject({ geo: fixtureGeo('USA'), n: 3, inForce: 2, ended: 1 });
    const byY = countsByYear(scoped, 2000, 2030, 2026);
    expect(byY.find((y) => y.year === 2027)).toEqual({ year: 2027, n: 1, announced: true });
    expect(byY.find((y) => y.year === 2022)?.announced).toBe(false);
    const byI = countsByInstrument(scoped, 2000, 2030);
    expect(byI['Financial incentive'].reduce((a, x) => a + x.n, 0)).toBe(4);
    expect(instrumentOf('RD&D')).toBe('R&D');
    expect(statusClass('Ended')).toBe('ended');
  });

  it('titles and links; only http(s) links are rendered', () => {
    const t = policyTexts(validateChunk('sector-policy-text', fx.chunks['sector-policy-text']));
    expect(t.get(1000)?.title).toBe('Advanced manufacturing production credit');
    expect(safeUrl('https://www.iea.org/policies/1')).toBe('https://www.iea.org/policies/1');
    expect(safeUrl('javascript:alert(1)')).toBeNull();
    expect(safeUrl(null)).toBeNull();
  });
});
