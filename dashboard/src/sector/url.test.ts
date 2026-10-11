import { describe, expect, it } from 'vitest';
import { navSectorSearch, parseSectorSearch, patchReplaces, SECTOR_DEFAULTS, SECTOR_KEY_ORDER, sectorHref, writeSectorSearch, type SectorUrlState } from './url.ts';

const ctx = {
  countries: new Set(['USA', 'CHN', 'DEU']),
  vars: new Set(['trade.exports', 'investment.annual']),
  items: new Set(['lithium', 'batteries-midstream']),
  techs: new Set(['Batteries', 'Solar']),
  stages: new Set(['Upstream', 'Midstream', 'Downstream']),
};

describe('sector url (2.7)', () => {
  it('defaults are omitted', () => {
    expect(writeSectorSearch(SECTOR_DEFAULTS)).toBe('');
    expect(parseSectorSearch('').state).toEqual(SECTOR_DEFAULTS);
  });

  it('round-trips every key in the fixed order', () => {
    const s: SectorUrlState = {
      c: 'USA', eu: 'bloc', lens: 'hhi', fam: 'exports', m: 'trade.exports', y: 2024, tdir: 'imports', xc: true,
      tech: 'Batteries', stage: 'Midstream', item: 'lithium', as: 'share', scale: 'log', geo: 'coalition',
    };
    const q = writeSectorSearch(s);
    expect(q).toBe('c=USA&eu=bloc&lens=hhi&fam=exports&m=trade.exports&y=2024&tdir=imports&xc=1&tech=Batteries&stage=Midstream&item=lithium&as=share&scale=log&geo=coalition');
    expect(q.split('&').map((p) => p.split('=')[0])).toEqual([...SECTOR_KEY_ORDER]);
    const back = parseSectorSearch(`?${q}`, ctx);
    expect(back.problems).toEqual([]);
    expect(back.state).toEqual(s);
  });

  it('reports invalid values and drops them', () => {
    const r = parseSectorSearch('c=XXX&lens=red&y=19&m=Bad&as=gdp&scale=cubic&eu=all&item=Not_Slug&geo=asia', ctx);
    expect(r.state).toEqual(SECTOR_DEFAULTS);
    expect(r.problems).toHaveLength(9);
    expect(r.problems.join(' ')).toMatch(/not an economy in the sector data/);
    expect(r.problems.join(' ')).toMatch(/% of GDP is not in this build/);
  });

  it('ignores Strategic and unknown keys silently', () => {
    const r = parseSectorSearch('h=80&g=95&tau=off&metric=si&c=CHN&zzz=1', ctx);
    expect(r.problems).toEqual([]);
    expect(r.state.c).toBe('CHN');
  });

  it('EUU needs eu=bloc', () => {
    expect(parseSectorSearch('c=EUU&eu=bloc').state.c).toBe('EUU');
    const r = parseSectorSearch('c=EUU');
    expect(r.state.c).toBeNull();
    expect(r.problems[0]).toMatch(/EUU needs eu=bloc/);
  });

  it('repeated keys keep the first value', () => {
    const r = parseSectorSearch('c=USA&c=CHN', ctx);
    expect(r.state.c).toBe('USA');
    expect(r.problems).toHaveLength(1);
  });

  it('lowercase ISO3 is accepted', () => {
    expect(parseSectorSearch('c=usa', ctx).state.c).toBe('USA');
  });

  it('navigation keeps c and eu only', () => {
    expect(navSectorSearch({ ...SECTOR_DEFAULTS, c: 'USA', eu: 'bloc', m: 'trade.exports', lens: 'hhi' })).toBe('?c=USA&eu=bloc');
    expect(navSectorSearch({ ...SECTOR_DEFAULTS })).toBe('');
    expect(navSectorSearch({ ...SECTOR_DEFAULTS, c: 'EUU', eu: 'members' })).toBe('');
    expect(sectorHref('/sector/batteries/midstream', { c: 'USA' }, { m: 'trade.exports' }, 'leaders')).toBe('/sector/batteries/midstream?c=USA&m=trade.exports#leaders');
  });

  it('year, show-as, scale and item replace history; the rest push', () => {
    expect(patchReplaces({ y: 2020 })).toBe(true);
    expect(patchReplaces({ y: 2020, as: 'share' })).toBe(true);
    expect(patchReplaces({ m: 'trade.exports' })).toBe(false);
    expect(patchReplaces({ c: 'USA' })).toBe(false);
  });

  it('syntax-only parsing before meta loads', () => {
    const r = parseSectorSearch('c=JPN&m=trade.rca&item=rare-earths&tech=Batteries');
    expect(r.problems).toEqual([]);
    expect(r.state).toMatchObject({ c: 'JPN', m: 'trade.rca', item: 'rare-earths', tech: 'Batteries' });
  });
});
