// Module-tab and Home-button targets (spec_sector.md 2.5, 2.6; spec_country.md A2.1, A2.2): pure
// functions over a location and the remembered last locations.

import { afterEach, describe, expect, it } from 'vitest';
import {
  countryProfileHref, countryTabHref, lastLocations, moduleResumeHref, rememberLocation, resetLastLocations, sectorCarry, sectorCountry,
  sectorProfileHref, sectorTabHref, strategicCarry, strategicCountry, strategicTabHref,
} from './lastLocation.ts';
import {
  ABOUT_NAV, aboutNavKeyOf, AIB_NAV, aibNavKeyOf, COUNTRY_NAV, countryIsoOfPath, countryNavKeyOf, isSectorProfilePath, MODULES, placeOf, sectorCellOfPath, sectorNavKeyOf,
  STRATEGIC_NAV, strategicNavOf, tabOf,
} from './modules.ts';
import { isLegacyStrategicSearch } from './HomeOrLegacy.tsx';
import { pageTitle } from './title.ts';
import { RESERVED_SEGMENTS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, techSlugOf } from '../sector/contract.ts';
import { isShellLegacyStageSlug, isShellStageSlug, SHELL_RESERVED_SEGMENTS, SHELL_STAGE_SLUGS, SHELL_TECH_SLUGS } from './sectorSlugs.ts';
import { SUPPLY_STAGE_SLUGS } from '../lib/stages.ts';
import { legacySectorRedirect } from './legacySector.tsx';

afterEach(() => resetLastLocations());

const none = { strategic: null, sector: null, sectorCell: null, country: null, countryIso: null };

describe('places and nav keys', () => {
  it('classifies paths', () => {
    expect(placeOf('/')).toBe('home');
    expect(placeOf('/sector')).toBe('sector');
    expect(placeOf('/sector/batteries/refining/USA')).toBe('sector');
    for (const p of ['/overview', '/country/USA', '/space', '/strategic', '/partners/CAN', '/methods', '/data']) expect(placeOf(p)).toBe('strategic');
    expect(placeOf('/sectors')).toBeNull();
    expect(placeOf('/nothing')).toBeNull();
    // Country Insights lives at /countries; the Strategic "Country scores" view keeps /country
    for (const p of ['/countries', '/countries/about', '/countries/JPN', '/countries/usa/memo']) expect(placeOf(p)).toBe('country');
    expect(placeOf('/country')).toBe('strategic');
    // IA refactor: About owns /about and /glossary; the AIB module owns /aib and /compacts
    expect(placeOf('/glossary')).toBe('about');
    expect(placeOf('/about')).toBe('about');
    for (const p of ['/aib', '/aib/partnerships', '/aib/roles', '/compacts', '/compacts/AIB-C01']) expect(placeOf(p)).toBe('aib');
  });
  it('legacy Strategic views show the tab that owns their content', () => {
    for (const p of ['/overview', '/country/USA', '/space', '/strategic/JPN']) expect(tabOf(p)).toBe('country');
    for (const p of ['/partners/CAN']) expect(tabOf(p)).toBe('aib');
    for (const p of ['/methods', '/data', '/glossary', '/about']) expect(tabOf(p)).toBe('about');
    expect(tabOf('/')).toBeNull();
    expect(tabOf('/nothing')).toBeNull();
    expect(aibNavKeyOf('/aib')).toBe('overview');
    expect(aibNavKeyOf('/aib/partnerships')).toBe('partnerships');
    expect(aibNavKeyOf('/compacts/AIB-C01')).toBe('compacts');
    expect(aibNavKeyOf('/compacts/about')).toBe('about');
    expect(aibNavKeyOf('/partners/CAN')).toBe('partnerships');
    expect(aboutNavKeyOf('/methods')).toBe('methods');
    expect(aboutNavKeyOf('/glossary')).toBe('glossary');
  });
  it('four tabs: Sectors, Countries, Allied Industrial Base, About (secondary); the legacy Strategic labels spell their views out', () => {
    expect(MODULES.map((m) => [m.id, m.label, m.home, !!m.secondary])).toEqual([
      ['sector', 'Sectors', '/sector', false],
      ['country', 'Countries', '/countries', false],
      ['aib', 'Allied Industrial Base', '/aib', false],
      ['about', 'About', '/about', true],
    ]);
    expect(AIB_NAV.map((n) => n.label)).toEqual(['Overview', 'Partnerships', 'Sectoral Compacts', 'Country roles', 'Managed Interdependence']);
    expect(ABOUT_NAV.map((n) => n.to)).toEqual(['/about', '/methods', '/data', '/glossary']);
    expect(STRATEGIC_NAV.map((n) => n.label)).toEqual(['Overview', 'Country scores', 'Security vs opportunity', 'Strategic Index', 'Partners', 'Methods', 'Data']);
    expect(STRATEGIC_NAV.map((n) => n.to)).toEqual(['/overview', '/country', '/space', '/strategic', '/partners', '/methods', '/data']);
    expect(COUNTRY_NAV).toEqual([
      { key: 'countries', to: '/countries', label: 'All countries' },
      { key: 'profile', to: '/countries/USA', label: 'Country profile' },
    ]);
  });
  it('Country paths: the ISO3 of a profile or memo, and the nav entry', () => {
    expect(countryIsoOfPath('/countries/JPN')).toBe('JPN');
    expect(countryIsoOfPath('/countries/jpn/memo')).toBe('JPN');
    expect(countryIsoOfPath('/countries/JPN/other')).toBeNull();
    expect(countryIsoOfPath('/countries/about')).toBeNull();
    expect(countryIsoOfPath('/countries/JAPAN')).toBeNull();
    expect(countryIsoOfPath('/countries')).toBeNull();
    expect(countryIsoOfPath('/country/JPN')).toBeNull();
    expect(countryNavKeyOf('/countries')).toBe('countries');
    expect(countryNavKeyOf('/countries/about')).toBe('about');
    expect(countryNavKeyOf('/countries/DEU')).toBe('profile');
    expect(countryNavKeyOf('/countries/DEU/memo')).toBe('profile');
    expect(countryNavKeyOf('/countries/1234')).toBeNull();
    expect(countryNavKeyOf('/overview')).toBeNull();
  });
  it('maps Sector paths to their nav entry', () => {
    expect(sectorNavKeyOf('/sector')).toBe('sectors');
    expect(sectorNavKeyOf('/sector/batteries')).toBe('sectors');
    expect(sectorNavKeyOf('/sector/batteries/refining')).toBe('profile');
    expect(sectorNavKeyOf('/sector/batteries/refining/USA')).toBe('profile');
    expect(sectorNavKeyOf('/sector/minerals/lithium')).toBe('minerals');
    expect(sectorNavKeyOf('/sector/explore/trade')).toBe('explore');
    expect(sectorNavKeyOf('/sector/concentration')).toBe('concentration');
    expect(sectorNavKeyOf('/sector/sources')).toBe('sources');
    expect(sectorNavKeyOf('/sector/data')).toBe('sources');
    expect(sectorNavKeyOf('/sector/country/JPN')).toBeNull();
    expect(strategicNavOf('/country/USA')).toBe('/country');
    expect(sectorCellOfPath('/sector/batteries/refining/USA')).toEqual({ tech: 'batteries', stage: 'refining' });
    for (const s of SUPPLY_STAGE_SLUGS) expect(sectorCellOfPath(`/sector/wind/${s}`)).toEqual({ tech: 'wind', stage: s });
    expect(sectorCellOfPath('/sector/minerals/lithium')).toBeNull();
    expect(sectorCellOfPath('/sector/baterries/refining')).toBeNull();
    // an old three-stage path is not a cell (the shell redirects it, design U1)
    for (const s of ['upstream', 'midstream', 'downstream']) expect(sectorCellOfPath(`/sector/batteries/${s}`)).toBeNull();
    expect(isSectorProfilePath('/sector/batteries/midstream')).toBe(false);
    // the "Skip to sections" link shows on profiles only
    expect(isSectorProfilePath('/sector/batteries/refining')).toBe(true);
    expect(isSectorProfilePath('/sector/batteries/refining/')).toBe(true);
    expect(isSectorProfilePath('/sector/batteries/refining/USA')).toBe(false);
    expect(isSectorProfilePath('/sector/minerals/lithium')).toBe(false);
    expect(isSectorProfilePath('/sector/batteries')).toBe(false);
  });
  it('legacy root links and titles', () => {
    expect(isLegacyStrategicSearch('?h=80&tech=Solar')).toBe(true);
    expect(isLegacyStrategicSearch('?show=change')).toBe(true);
    expect(isLegacyStrategicSearch('')).toBe(false);
    expect(isLegacyStrategicSearch('?c=USA&eu=bloc')).toBe(false);
    expect(pageTitle('')).toBe('Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Overview')).toBe('Overview | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Batteries — Refining', 'Sector Insights')).toBe('Batteries — Refining | Sectors | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Batteries — Refining', 'sector')).toBe('Batteries — Refining | Sectors | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Compacts', 'aib')).toBe('Compacts | Allied Industrial Base | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Countries', 'country')).toBe('Countries | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Overview', 'strategic')).toBe('Overview | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('Japan', 'country')).toBe('Japan | Countries | Allied Industrial Base Explorer | NEIS Center');
    expect(pageTitle('', 'Country Insights')).toBe('Countries | Allied Industrial Base Explorer | NEIS Center');
  });
  it("the shell's slug copies equal the contract's", () => {
    expect(SHELL_TECH_SLUGS).toEqual([...TECH_SLUGS]);
    expect(SHELL_STAGE_SLUGS).toEqual([...STAGE_SLUGS]);
    // the six stages in chain order (lib/stages.ts, which the Sector contract reads too); no legacy slug
    expect(SHELL_STAGE_SLUGS).toEqual(['extraction', 'refining', 'components', 'assembly', 'deployment', 'recovery']);
    for (const s of ['upstream', 'midstream', 'downstream']) {
      expect(isShellStageSlug(s), s).toBe(false);
      expect(isShellLegacyStageSlug(s), s).toBe(true);
    }
    expect(SHELL_RESERVED_SEGMENTS).toEqual([...RESERVED_SEGMENTS]);
    for (const t of SHELL_TECH_SLUGS) expect(SHELL_RESERVED_SEGMENTS).not.toContain(t);
  });
  it('the tab rule slugs the Strategic tech labels exactly as the contract does', () => {
    for (const t of TECH_SLUGS) {
      expect(techSlugOf(TECH_LABELS[t])).toBe(t);
      expect(TECH_LABELS[t].toLowerCase().replace(/\s+/g, '-')).toBe(t);
    }
  });
});

describe('carried keys', () => {
  it('Strategic: the scenario and show / tech / stage, in order', () => {
    expect(strategicCarry('?h=80&g=95&show=change&tech=Solar&stage=Refining&metric=si&partner=CAN')).toBe('?h=80&g=95&show=change&tech=Solar&stage=Refining');
    expect(strategicCarry('')).toBe('');
    expect(strategicCarry('?metric=si')).toBe('');
  });
  it('Sector: c and eu only', () => {
    expect(sectorCarry('?c=USA&eu=bloc&lens=hhi&m=trade.exports')).toBe('?c=USA&eu=bloc');
    expect(sectorCarry('?lens=hhi')).toBe('');
  });
  it('the country of a Strategic location', () => {
    expect(strategicCountry('/country/USA')).toBe('USA');
    expect(strategicCountry('/strategic/jpn')).toBe('JPN');
    expect(strategicCountry('/partners/CAN/')).toBe('CAN');
    expect(strategicCountry('/country')).toBeNull();
    expect(strategicCountry('/overview')).toBeNull();
  });
});

describe('tab targets (2.6)', () => {
  it('Strategic -> SECTOR with tech and stage: the profile, with the country as c', () => {
    expect(sectorTabHref('/overview', '?h=80&tech=Batteries&stage=Refining', none)).toBe('/sector/batteries/refining');
    expect(sectorTabHref('/country/USA', '?tech=Electric%20Vehicles&stage=Deployment', none)).toBe('/sector/electric-vehicles/deployment?c=USA');
    expect(sectorTabHref('/strategic/JPN', '?tech=Green+Hydrogen&stage=Components', none)).toBe('/sector/green-hydrogen/components?c=JPN');
  });
  it('Strategic -> SECTOR with an old three-stage stage: Downstream opens Deployment, Upstream and Midstream the chain', () => {
    expect(sectorTabHref('/country/USA', '?tech=Electric%20Vehicles&stage=Downstream', none)).toBe('/sector/electric-vehicles/deployment?c=USA');
    expect(sectorTabHref('/overview', '?tech=Batteries&stage=Midstream', none)).toBe('/sector/batteries');
    expect(sectorTabHref('/strategic/JPN', '?tech=Solar&stage=Upstream', none)).toBe('/sector/solar?c=JPN');
  });
  it('Strategic -> SECTOR otherwise: the last Sector location (c added), else /sector', () => {
    expect(sectorTabHref('/overview', '?h=80', none)).toBe('/sector');
    expect(sectorTabHref('/country/USA', '', none)).toBe('/sector?c=USA');
    const last = { ...none, sector: '/sector/explore/trade?c=DEU&m=trade.exports' };
    expect(sectorTabHref('/overview', '?tech=Batteries', last)).toBe('/sector/explore/trade?c=DEU&m=trade.exports');
    expect(sectorTabHref('/partners/CAN', '', last)).toBe('/sector/explore/trade?c=CAN&m=trade.exports');
    // an unknown technology is not carried
    expect(sectorTabHref('/overview', '?tech=Tech%20A&stage=Refining', none)).toBe('/sector');
  });
  it('inside a module the tab is its landing; Home and other places resume', () => {
    expect(sectorTabHref('/sector/batteries/refining', '?c=USA&eu=bloc&lens=hhi', none)).toBe('/sector?c=USA&eu=bloc');
    expect(strategicTabHref('/country/USA', '?h=80&metric=si', none)).toBe('/overview?h=80');
    expect(strategicTabHref('/sector', '', { ...none, strategic: '/partners/CAN?h=80&partner=USA' })).toBe('/partners/CAN?h=80&partner=USA');
    expect(strategicTabHref('/', '', none)).toBe('/overview');
    expect(sectorTabHref('/', '', { ...none, sector: '/sector/minerals/lithium' })).toBe('/sector/minerals/lithium');
  });
  it('remembers the last location per module and the last cell', () => {
    rememberLocation('/overview', '?h=80');
    rememberLocation('/', '');
    rememberLocation('/nothing', '?x=1');
    rememberLocation('/glossary', '?module=country');
    rememberLocation('/sector/solar/refining/CHN', '?c=CHN');
    rememberLocation('/sector/explore/trade', '');
    // an old three-stage link, about to be redirected, is not remembered
    rememberLocation('/sector/wind/midstream', '?c=DEU');
    expect(lastLocations()).toEqual({ strategic: '/overview?h=80', sector: '/sector/explore/trade', sectorCell: '/sector/solar/refining', country: null, countryIso: null });
    expect(moduleResumeHref('strategic')).toBe('/overview?h=80');
    expect(moduleResumeHref('sector')).toBe('/sector/explore/trade');
    expect(sectorProfileHref()).toBe('/sector/solar/refining');
    resetLastLocations();
    expect(moduleResumeHref('sector')).toBe('/sector');
    expect(moduleResumeHref('strategic')).toBe('/overview');
    // the default cell: Batteries — Refining (design E6)
    expect(sectorProfileHref()).toBe('/sector/batteries/refining');
  });
});

describe('Country Insights tab targets (A2.2)', () => {
  it('Strategic -> COUNTRY: the country of /country, /strategic or /partners; else the last Country location; else /countries', () => {
    expect(countryTabHref('/country/USA', '?h=80', none)).toBe('/countries/USA');
    expect(countryTabHref('/strategic/jpn', '', none)).toBe('/countries/JPN');
    expect(countryTabHref('/partners/DEU', '?h=80', none)).toBe('/countries/DEU');
    expect(countryTabHref('/overview', '?h=80', none)).toBe('/countries');
    expect(countryTabHref('/overview', '', { ...none, country: '/countries/KOR?x=1', countryIso: 'KOR' })).toBe('/countries/KOR?x=1');
  });
  it('Sector -> COUNTRY: the highlighted country (path or c, never the EU aggregate)', () => {
    expect(countryTabHref('/sector/batteries/refining/CHN', '', none)).toBe('/countries/CHN');
    expect(countryTabHref('/sector/explore/trade', '?c=USA&m=trade.exports', none)).toBe('/countries/USA');
    expect(countryTabHref('/sector', '?c=EUU', none)).toBe('/countries');
    expect(countryTabHref('/sector/minerals/lithium', '', { ...none, country: '/countries/about' })).toBe('/countries/about');
    expect(sectorCountry('/sector/batteries/refining/fra', '')).toBe('FRA');
    expect(sectorCountry('/sector/minerals/lithium', '?c=xx')).toBeNull();
  });
  it('inside Country the tab is its landing; Home resumes; PROFILE follows the country on screen, else the last one, else USA', () => {
    expect(countryTabHref('/countries/JPN', '', none)).toBe('/countries');
    expect(countryTabHref('/', '', none)).toBe('/countries');
    expect(countryProfileHref('/countries/DEU/memo', none)).toBe('/countries/DEU');
    expect(countryProfileHref('/countries', { ...none, countryIso: 'JPN' })).toBe('/countries/JPN');
    expect(countryProfileHref('/countries/about', none)).toBe('/countries/USA');
  });
  it('Country -> SECTOR adds the profile country as the highlight; Country -> STRATEGIC resumes unchanged', () => {
    expect(sectorTabHref('/countries/JPN', '', none)).toBe('/sector?c=JPN');
    expect(sectorTabHref('/countries/JPN/memo', '', { ...none, sector: '/sector/solar/assembly?c=USA' })).toBe('/sector/solar/assembly?c=JPN');
    expect(sectorTabHref('/countries', '', { ...none, sector: '/sector/explore/trade' })).toBe('/sector/explore/trade');
    expect(sectorTabHref('/countries/about', '', none)).toBe('/sector');
    expect(strategicTabHref('/countries/JPN', '', { ...none, strategic: '/partners/CAN?h=80' })).toBe('/partners/CAN?h=80');
    expect(strategicTabHref('/countries/JPN', '', none)).toBe('/overview');
  });
  it('remembers the last Country location and profile', () => {
    rememberLocation('/countries/JPN', '');
    rememberLocation('/countries/about', '');
    expect(lastLocations().country).toBe('/countries/about');
    expect(lastLocations().countryIso).toBe('JPN');
    expect(moduleResumeHref('country')).toBe('/countries/about');
    expect(countryProfileHref('/countries')).toBe('/countries/JPN');
    rememberLocation('/countries/deu/memo', '');
    expect(lastLocations().countryIso).toBe('DEU');
    resetLastLocations();
    expect(moduleResumeHref('country')).toBe('/countries');
  });
});

describe('old three-stage Sector links (design U1)', () => {
  it('Downstream opens the Deployment page, the query and hash kept, with a notice', () => {
    expect(legacySectorRedirect('/sector/solar/downstream', '?c=USA', '#trade')).toEqual({
      to: { pathname: '/sector/solar/deployment', search: '?c=USA', hash: '#trade' },
      notice: 'Solar — Downstream corresponds to Solar — Deployment.',
    });
    expect(legacySectorRedirect('/sector/electric-vehicles/downstream/IND', '', '#aid-investment')?.to).toEqual({
      pathname: '/sector/electric-vehicles/deployment/IND', search: '', hash: '#aid-investment',
    });
  });
  it('Upstream and Midstream open the chain page with the query kept, the economy as c and the hash dropped; the notice names the new stages', () => {
    const mid = legacySectorRedirect('/sector/batteries/midstream', '?eu=bloc', '#trade');
    expect(mid?.to).toEqual({ pathname: '/sector/batteries', search: '?eu=bloc', hash: '' });
    expect(mid?.notice).toBe(
      'Batteries — Midstream corresponds mostly to Components and Assembly (some refined materials are under Refining). Showing the Batteries supply chain: pick a stage there.',
    );
    const up = legacySectorRedirect('/sector/green-hydrogen/upstream/jpn', '?lens=hhi', '');
    expect(up?.to).toEqual({ pathname: '/sector/green-hydrogen', search: '?c=JPN&lens=hhi', hash: '' });
    expect(up?.notice).toContain('Green Hydrogen — Upstream corresponds to Extraction, Refining and Recovery.');
    // a highlight already in the link wins over the economy segment
    expect(legacySectorRedirect('/sector/wind/midstream/CHN', '?c=DEU', '')?.to.search).toBe('?c=DEU');
  });
  it('only legacy Sector paths', () => {
    for (const p of ['/sector/batteries/refining', '/sector/batteries', '/sector/minerals/lithium', '/overview', '/network/batteries-midstream']) {
      expect(legacySectorRedirect(p), p).toBeNull();
    }
  });
});
