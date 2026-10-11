// Module-tab and Home-button targets (spec_sector.md 2.5, 2.6): pure functions over a location and
// the remembered last locations.

import { afterEach, describe, expect, it } from 'vitest';
import {
  lastLocations, moduleResumeHref, rememberLocation, resetLastLocations, sectorCarry, sectorProfileHref, sectorTabHref, strategicCarry,
  strategicCountry, strategicTabHref,
} from './lastLocation.ts';
import { isSectorProfilePath, placeOf, sectorCellOfPath, sectorNavKeyOf, strategicNavOf } from './modules.ts';
import { isLegacyStrategicSearch } from './HomeOrLegacy.tsx';
import { pageTitle } from './title.ts';
import { RESERVED_SEGMENTS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, techSlugOf } from '../sector/contract.ts';
import { SHELL_RESERVED_SEGMENTS, SHELL_STAGE_SLUGS, SHELL_TECH_SLUGS } from './sectorSlugs.ts';

afterEach(() => resetLastLocations());

const none = { strategic: null, sector: null, sectorCell: null };

describe('places and nav keys', () => {
  it('classifies paths', () => {
    expect(placeOf('/')).toBe('home');
    expect(placeOf('/sector')).toBe('sector');
    expect(placeOf('/sector/batteries/midstream/USA')).toBe('sector');
    for (const p of ['/overview', '/country/USA', '/space', '/strategic', '/partners/CAN', '/network/batteries-midstream', '/methods', '/data']) expect(placeOf(p)).toBe('strategic');
    expect(placeOf('/sectors')).toBeNull();
    expect(placeOf('/nothing')).toBeNull();
  });
  it('maps Sector paths to their nav entry', () => {
    expect(sectorNavKeyOf('/sector')).toBe('sectors');
    expect(sectorNavKeyOf('/sector/batteries')).toBe('sectors');
    expect(sectorNavKeyOf('/sector/batteries/midstream')).toBe('profile');
    expect(sectorNavKeyOf('/sector/batteries/midstream/USA')).toBe('profile');
    expect(sectorNavKeyOf('/sector/minerals/lithium')).toBe('minerals');
    expect(sectorNavKeyOf('/sector/explore/trade')).toBe('explore');
    expect(sectorNavKeyOf('/sector/concentration')).toBe('concentration');
    expect(sectorNavKeyOf('/sector/sources')).toBe('sources');
    expect(sectorNavKeyOf('/sector/data')).toBe('sources');
    expect(sectorNavKeyOf('/sector/country/JPN')).toBeNull();
    expect(strategicNavOf('/country/USA')).toBe('/country');
    expect(sectorCellOfPath('/sector/batteries/midstream/USA')).toEqual({ tech: 'batteries', stage: 'midstream' });
    expect(sectorCellOfPath('/sector/minerals/lithium')).toBeNull();
    expect(sectorCellOfPath('/sector/baterries/midstream')).toBeNull();
    // the "Skip to sections" link shows on profiles only
    expect(isSectorProfilePath('/sector/batteries/midstream')).toBe(true);
    expect(isSectorProfilePath('/sector/batteries/midstream/')).toBe(true);
    expect(isSectorProfilePath('/sector/batteries/midstream/USA')).toBe(false);
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
    expect(pageTitle('Batteries — Midstream', 'Sector Insights')).toBe('Batteries — Midstream | Sector Insights | Allied Industrial Base Explorer | NEIS Center');
  });
  it("the shell's slug copies equal the contract's", () => {
    expect(SHELL_TECH_SLUGS).toEqual([...TECH_SLUGS]);
    expect(SHELL_STAGE_SLUGS).toEqual([...STAGE_SLUGS]);
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
    expect(strategicCarry('?h=80&g=95&show=change&tech=Solar&stage=Upstream&metric=si&partner=CAN')).toBe('?h=80&g=95&show=change&tech=Solar&stage=Upstream');
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
    expect(sectorTabHref('/overview', '?h=80&tech=Batteries&stage=Midstream', none)).toBe('/sector/batteries/midstream');
    expect(sectorTabHref('/country/USA', '?tech=Electric%20Vehicles&stage=Downstream', none)).toBe('/sector/electric-vehicles/downstream?c=USA');
    expect(sectorTabHref('/strategic/JPN', '?tech=Green+Hydrogen&stage=Midstream', none)).toBe('/sector/green-hydrogen/midstream?c=JPN');
  });
  it('Strategic -> SECTOR otherwise: the last Sector location (c added), else /sector', () => {
    expect(sectorTabHref('/overview', '?h=80', none)).toBe('/sector');
    expect(sectorTabHref('/country/USA', '', none)).toBe('/sector?c=USA');
    const last = { ...none, sector: '/sector/explore/trade?c=DEU&m=trade.exports' };
    expect(sectorTabHref('/overview', '?tech=Batteries', last)).toBe('/sector/explore/trade?c=DEU&m=trade.exports');
    expect(sectorTabHref('/partners/CAN', '', last)).toBe('/sector/explore/trade?c=CAN&m=trade.exports');
    // an unknown technology is not carried
    expect(sectorTabHref('/overview', '?tech=Tech%20A&stage=Midstream', none)).toBe('/sector');
  });
  it('inside a module the tab is its landing; Home and other places resume', () => {
    expect(sectorTabHref('/sector/batteries/midstream', '?c=USA&eu=bloc&lens=hhi', none)).toBe('/sector?c=USA&eu=bloc');
    expect(strategicTabHref('/country/USA', '?h=80&metric=si', none)).toBe('/overview?h=80');
    expect(strategicTabHref('/sector', '', { ...none, strategic: '/partners/CAN?h=80&partner=USA' })).toBe('/partners/CAN?h=80&partner=USA');
    expect(strategicTabHref('/', '', none)).toBe('/overview');
    expect(sectorTabHref('/', '', { ...none, sector: '/sector/minerals/lithium' })).toBe('/sector/minerals/lithium');
  });
  it('remembers the last location per module and the last cell', () => {
    rememberLocation('/overview', '?h=80');
    rememberLocation('/', '');
    rememberLocation('/nothing', '?x=1');
    rememberLocation('/sector/solar/upstream/CHN', '?c=CHN');
    rememberLocation('/sector/explore/trade', '');
    expect(lastLocations()).toEqual({ strategic: '/overview?h=80', sector: '/sector/explore/trade', sectorCell: '/sector/solar/upstream' });
    expect(moduleResumeHref('strategic')).toBe('/overview?h=80');
    expect(moduleResumeHref('sector')).toBe('/sector/explore/trade');
    expect(sectorProfileHref()).toBe('/sector/solar/upstream');
    resetLastLocations();
    expect(moduleResumeHref('sector')).toBe('/sector');
    expect(moduleResumeHref('strategic')).toBe('/overview');
    expect(sectorProfileHref()).toBe('/sector/batteries/midstream');
  });
});
