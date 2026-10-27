import { describe, expect, it } from 'vitest';
import { countryHref, parseCountrySearch, serializeCountrySearch } from '../url.ts';
import { countryIsoOfPath, countryNavItems, countryNavKeyOf } from '../nav.ts';

describe('Country URL state (A3)', () => {
  it('omits defaults and writes keys in the fixed order', () => {
    expect(serializeCountrySearch({})).toBe('');
    expect(serializeCountrySearch({ all: true, partner: 'USA', cell: 'oil-upstream' })).toBe('?cell=oil-upstream&partner=USA&all=1');
    expect(serializeCountrySearch({ cell: null, partner: null, all: false })).toBe('');
  });

  it('round-trips', () => {
    const cases = ['', '?cell=batteries-midstream', '?partner=JPN', '?all=1', '?ptab=development', '?cell=electric-grid-midstream&partner=DEU&all=1&ptab=investment'];
    for (const s of cases) expect(serializeCountrySearch(parseCountrySearch(s).state)).toBe(s);
  });

  it('drops invalid values and lists them; ignores unknown keys', () => {
    const p = parseCountrySearch('?cell=oil&partner=usa1&all=yes&h=80&tech=Oil');
    expect(p.state).toEqual({ cell: null, partner: null, all: false, ptab: 'trade' });
    expect(p.problems).toHaveLength(3);
    expect(p.problems[0]).toMatch(/^cell=oil/);
  });

  it('normalises case and checks against the sheet', () => {
    expect(parseCountrySearch('?cell=Oil-Upstream&partner=jpn').state).toEqual({ cell: 'oil-upstream', partner: 'JPN', all: false, ptab: 'trade' });
    const ctx = { cells: new Set(['oil-upstream']), partners: new Set(['JPN']) };
    const p = parseCountrySearch('?cell=gas-upstream&partner=DEU', ctx);
    expect(p.state.cell).toBeNull();
    expect(p.state.partner).toBeNull();
    expect(p.problems.join(' ')).toMatch(/not a sector of this country.*not among the partners/);
  });

  it('keeps the first of a repeated key', () => {
    const p = parseCountrySearch('?cell=oil-upstream&cell=gas-upstream');
    expect(p.state.cell).toBe('oil-upstream');
    expect(p.problems[0]).toMatch(/repeated/);
  });

  it('builds profile links', () => {
    expect(countryHref('jpn')).toBe('/countries/JPN');
    expect(countryHref('USA', { cell: 'wind-upstream' }, 'why')).toBe('/countries/USA?cell=wind-upstream#why');
  });
});

describe('Country nav (A2.1)', () => {
  it('reads the profile ISO3 and the view of a path', () => {
    expect(countryIsoOfPath('/countries/JPN')).toBe('JPN');
    expect(countryIsoOfPath('/countries/jpn/memo')).toBe('JPN');
    expect(countryIsoOfPath('/countries/about')).toBeNull();
    expect(countryIsoOfPath('/countries')).toBeNull();
    expect(countryNavKeyOf('/countries')).toBe('countries');
    expect(countryNavKeyOf('/countries/about')).toBe('about');
    expect(countryNavKeyOf('/countries/DEU/memo')).toBe('profile');
    expect(countryNavKeyOf('/country/DEU')).toBeNull();
  });

  it('points PROFILE at the country on screen', () => {
    const items = countryNavItems('/countries/DEU');
    expect(items.map((i) => i.label)).toEqual(['All countries', 'Country profile']);
    expect(items[1]).toMatchObject({ to: '/countries/DEU', active: true });
  });
});
