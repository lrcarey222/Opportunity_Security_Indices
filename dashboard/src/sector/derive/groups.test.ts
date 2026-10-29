import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo, fixtureItem } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import { blocNote, countryOptions, euRows, membersRows, partOf } from './groups.ts';

const fx = buildSectorFixture();
const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
const v = (id: string) => model.varById.get(id)!;

describe('EU-27 as one bloc (3.7)', () => {
  it('members mode drops EU-level rows (no double counting)', () => {
    const rows = facts.rows(v('consumption.per_capita').idx, fixtureItem('fuel', 'oil'), 2025);
    expect(rows.some((r) => r.geo === fixtureGeo('EUU'))).toBe(true);
    expect(membersRows(rows, model).some((r) => r.geo === fixtureGeo('EUU'))).toBe(false);
  });

  it("the source's own EU row is preferred and members are dropped", () => {
    const rows = facts.rows(v('consumption.per_capita').idx, fixtureItem('fuel', 'oil'), 2025);
    const r = euRows(rows, model, v('consumption.per_capita'), 'bloc');
    expect(r.mode).toBe('source_eu');
    expect(r.rows.find((x) => x.geo === fixtureGeo('EUU'))?.value).toBe(50.2);
    expect(r.rows.some((x) => x.geo === fixtureGeo('DEU') || x.geo === fixtureGeo('FRA'))).toBe(false);
  });

  it('additive measures sum the members', () => {
    const it = fixtureItem('cell', 'batteries-assembly');
    const rows = facts.rows(v('investment.annual').idx, it, 2025);
    const r = euRows(rows, model, v('investment.annual'), 'bloc');
    expect(r.mode).toBe('summed');
    const eu = r.rows.find((x) => x.geo === fixtureGeo('EUU'))!;
    expect(eu.value).toBeCloseTo(4.5, 12); // DEU only (FRA, CYP have no investment rows)
    expect(eu.status).toBe('est');
    expect(r.rows.some((x) => x.geo === fixtureGeo('DEU'))).toBe(false);
  });

  it('intensive measures are not defined for the bloc', () => {
    const rows = facts.rows(v('trade.rca').idx, fixtureItem('cell', 'batteries-assembly'), 2024);
    const r = euRows(rows, model, v('trade.rca'), 'bloc');
    expect(r.mode).toBe('undefined');
    expect(r.rows.some((x) => x.geo === fixtureGeo('DEU'))).toBe(true);
  });

  it('trade measures need the dyads', () => {
    const rows = facts.rows(v('trade.exports').idx, fixtureItem('cell', 'batteries-assembly'), 2024);
    expect(euRows(rows, model, v('trade.exports'), 'bloc').mode).toBe('needs_trade');
  });

  it('the bloc note lists the EU members outside the coalition', () => {
    expect(blocNote(model)).toMatch(/CYP is an EU member outside the coalition tiers/);
  });

  it('highlight options: China first among other economies; members fold into the EU in bloc mode', () => {
    const o = countryOptions(model, 'members');
    const other = o.filter((x) => x.group === 'Other economies');
    expect(other[0].iso3c).toBe('CHN');
    const b = countryOptions(model, 'bloc');
    expect(b[0].iso3c).toBe('EUU');
    expect(b.some((x) => x.iso3c === 'DEU')).toBe(false);
    expect(partOf(model.geoByCode.get('WLD'))).toBe('unattributed');
  });
});
