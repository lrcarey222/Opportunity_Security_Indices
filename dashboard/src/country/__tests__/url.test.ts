import { describe, expect, it } from 'vitest';
import { countryHref, legacyCellOf, parseCountrySearch, serializeCountrySearch } from '../url.ts';
import { countryIsoOfPath, countryNavItems, countryNavKeyOf } from '../nav.ts';

describe('Country URL state (A3)', () => {
  it('omits defaults and writes keys in the fixed order', () => {
    expect(serializeCountrySearch({})).toBe('');
    expect(serializeCountrySearch({ all: true, partner: 'USA', cell: 'oil-extraction' })).toBe('?cell=oil-extraction&partner=USA&all=1');
    expect(serializeCountrySearch({ cell: null, partner: null, all: false })).toBe('');
  });

  it('round-trips', () => {
    const cases = ['', '?cell=batteries-refining', '?partner=JPN', '?all=1', '?ptab=development', '?cell=electric-grid-components&partner=DEU&all=1&ptab=investment'];
    for (const s of cases) expect(serializeCountrySearch(parseCountrySearch(s).state)).toBe(s);
  });

  it('drops invalid values and lists them; ignores unknown keys', () => {
    const p = parseCountrySearch('?cell=oil&partner=usa1&all=yes&h=80&tech=Oil');
    expect(p.state).toEqual({ cell: null, partner: null, all: false, ptab: 'trade' });
    expect(p.problems).toHaveLength(3);
    expect(p.problems[0]).toMatch(/^cell=oil/);
  });

  it('normalises case and checks against the sheet', () => {
    expect(parseCountrySearch('?cell=Oil-Extraction&partner=jpn').state).toEqual({ cell: 'oil-extraction', partner: 'JPN', all: false, ptab: 'trade' });
    const ctx = { cells: new Set(['oil-extraction']), partners: new Set(['JPN']) };
    const p = parseCountrySearch('?cell=gas-extraction&partner=DEU', ctx);
    expect(p.state.cell).toBeNull();
    expect(p.state.partner).toBeNull();
    expect(p.problems.join(' ')).toMatch(/not a sector of this country.*not among the partners/);
  });

  it('keeps the first of a repeated key', () => {
    const p = parseCountrySearch('?cell=oil-extraction&cell=gas-extraction');
    expect(p.state.cell).toBe('oil-extraction');
    expect(p.problems[0]).toMatch(/repeated/);
  });

  it('an old three-stage cell (design U1): Downstream is read as Deployment with a notice; Upstream and Midstream are dropped, naming the stages that replaced them', () => {
    expect(legacyCellOf('oil-downstream')).toEqual({ to: 'oil-deployment', note: 'Oil — Downstream is now Oil — Deployment' });
    expect(legacyCellOf('batteries-midstream')).toMatchObject({ to: null });
    expect(legacyCellOf('batteries-midstream')!.note).toMatch(/^Batteries — Midstream is no longer a stage: it is now mostly Components and Assembly/);
    expect(legacyCellOf('wind-upstream')!.note).toMatch(/^Wind — Upstream is no longer a stage: it is now Extraction, Refining and Recovery/);
    // not a legacy key of a Country technology: the ordinary notice
    for (const k of ['fusion-upstream', 'oil-refining', 'oil', 'upstream']) expect(legacyCellOf(k)).toBeNull();

    const down = parseCountrySearch('?cell=Solar-Downstream&partner=JPN');
    expect(down.state).toEqual({ cell: 'solar-deployment', partner: 'JPN', all: false, ptab: 'trade' });
    expect(down.problems).toEqual(['cell=Solar-Downstream (Solar — Downstream is now Solar — Deployment; showing it)']);
    // the URL is written back with the six-stage key
    expect(serializeCountrySearch(down.state)).toBe('?cell=solar-deployment&partner=JPN');

    const mid = parseCountrySearch('?cell=batteries-midstream');
    expect(mid.state.cell).toBeNull();
    expect(mid.problems[0]).toMatch(/^cell=batteries-midstream \(Batteries — Midstream is no longer a stage: .*; the sector was dropped\)$/);
    const up = parseCountrySearch('?cell=oil-upstream');
    expect(up.state.cell).toBeNull();
    expect(up.problems[0]).toMatch(/Extraction, Refining and Recovery; the sector was dropped\)$/);
    // a mapped key the sheet does not have is dropped too
    const ctx = { cells: new Set(['oil-extraction']) };
    const gone = parseCountrySearch('?cell=oil-downstream', ctx);
    expect(gone.state.cell).toBeNull();
    expect(gone.problems[0]).toMatch(/Oil — Deployment, which is not a sector of this country\)$/);
    expect(parseCountrySearch('?cell=fusion-upstream').problems).toEqual(['cell=fusion-upstream (not a sector)']);
  });

  it('builds profile links', () => {
    expect(countryHref('jpn')).toBe('/countries/JPN');
    expect(countryHref('USA', { cell: 'wind-components' }, 'why')).toBe('/countries/USA?cell=wind-components#why');
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
