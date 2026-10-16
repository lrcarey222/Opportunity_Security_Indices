// The four modules and their view lists (spec_sector.md 2.2, 5.2; spec_country.md A2.1; Sectoral
// Compacts). Dependency-free (the Sector slugs are the shell's copies), so the entry imports neither
// the engine, the Sector contract, the Country contract nor the compacts contract; src/state/nav.ts
// re-exports STRATEGIC_NAV (alias NAV_ITEMS) and MODULES for the Strategic views.

import { isShellReservedSegment, isShellStageSlug, isShellTechSlug } from './sectorSlugs.ts';

export type ModuleId = 'sector' | 'strategic' | 'country' | 'aib';
/** Where a path belongs: a module, Home, or nothing (an unknown path, about to redirect). */
export type Place = ModuleId | 'home' | null;

export interface ModuleInfo {
  id: ModuleId;
  label: string;
  /** the Home card's tag */
  tag: string;
  /** the module's landing path */
  home: string;
}

/** Sector, Strategic, Country: the order the user numbered them; Sectoral Compacts fourth. */
export const MODULES: readonly ModuleInfo[] = [
  { id: 'sector', label: 'Sector Insights', tag: 'DESCRIPTIVE DATA', home: '/sector' },
  { id: 'strategic', label: 'Strategic Insights', tag: 'NORMATIVE PRIORITISATION', home: '/overview' },
  { id: 'country', label: 'Country Insights', tag: 'COUNTRY BRIEFING', home: '/countries' },
  { id: 'aib', label: 'Sectoral Compacts', tag: 'COORDINATION DESIGN', home: '/compacts' },
];

export const moduleInfo = (id: ModuleId): ModuleInfo => MODULES.find((m) => m.id === id) as ModuleInfo;

/**
 * Strategic Insights views (spec_react 8.1), in nav order. Labels spell out what each view is
 * (spec_country.md C3.5): "Country scores" never reads like the Country Insights module; paths unchanged.
 */
export const STRATEGIC_NAV: { to: string; label: string }[] = [
  { to: '/overview', label: 'Overview' },
  { to: '/country', label: 'Country scores' },
  { to: '/space', label: 'Security vs opportunity' },
  { to: '/strategic', label: 'Strategic Index' },
  { to: '/partners', label: 'Partners' },
  { to: '/network', label: 'Allied network' },
  { to: '/methods', label: 'Methods' },
  { to: '/data', label: 'Data' },
];

/** The first path segments of the Strategic views. */
export const STRATEGIC_SEGMENTS = STRATEGIC_NAV.map((n) => n.to.slice(1));

export type SectorNavKey = 'sectors' | 'profile' | 'minerals' | 'explore' | 'concentration' | 'sources';

/** Sector Insights views (2.2). PROFILE's target is the last visited cell (see lastLocation.ts). */
export const SECTOR_NAV: { key: SectorNavKey; to: string; label: string }[] = [
  { key: 'sectors', to: '/sector', label: 'Sectors' },
  { key: 'profile', to: '/sector/batteries/midstream', label: 'Profile' },
  { key: 'minerals', to: '/sector/minerals', label: 'Minerals' },
  { key: 'explore', to: '/sector/explore', label: 'Explore' },
  { key: 'concentration', to: '/sector/concentration', label: 'Concentration' },
  { key: 'sources', to: '/sector/sources', label: 'Sources & data' },
];

export type CountryNavKey = 'countries' | 'profile' | 'about';

/** The default profile of the Country nav before any country was visited. */
export const COUNTRY_DEFAULT_ISO = 'USA';

/** Country Insights views (spec_country.md A2.1). PROFILE's target is the last visited country. */
export const COUNTRY_NAV: { key: CountryNavKey; to: string; label: string }[] = [
  { key: 'countries', to: '/countries', label: 'Countries' },
  { key: 'profile', to: `/countries/${COUNTRY_DEFAULT_ISO}`, label: 'Country profile' },
  { key: 'about', to: '/countries/about', label: 'About & data' },
];

/** The first path segment of Country Insights (the Strategic "Country scores" view is /country). */
export const COUNTRY_SEGMENT = 'countries';

/** Second segments under /countries that are views, not countries. */
const COUNTRY_RESERVED = new Set(['about']);

export type AibNavKey = 'compacts' | 'compact' | 'about';

/**
 * Sectoral Compacts views. The Compact link needs a compact id, which only the data know: the module
 * registers it (src/aib/nav.ts) once its data load; this static fallback lists the other two.
 */
export const AIB_NAV: { key: AibNavKey; to: string; label: string }[] = [
  { key: 'compacts', to: '/compacts', label: 'Compacts' },
  { key: 'about', to: '/compacts/about', label: 'About & data' },
];

/** The first path segment of Sectoral Compacts. */
export const AIB_SEGMENT = 'compacts';

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** Which module (or Home) a pathname belongs to. */
export function placeOf(pathname: string): Place {
  const s = segments(pathname);
  if (s.length === 0) return 'home';
  if (s[0] === 'sector') return 'sector';
  if (s[0] === COUNTRY_SEGMENT) return 'country';
  if (s[0] === AIB_SEGMENT) return 'aib';
  if (STRATEGIC_SEGMENTS.includes(s[0])) return 'strategic';
  return null;
}

/** The ISO3 of a Country Insights profile or memo path (/countries/:iso3[/memo]), upper-cased, or null. */
export function countryIsoOfPath(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== COUNTRY_SEGMENT || s.length < 2 || s.length > 3) return null;
  if (COUNTRY_RESERVED.has(s[1].toLowerCase())) return null;
  if (s.length === 3 && s[2] !== 'memo') return null;
  return /^[A-Za-z]{3}$/.test(s[1]) ? s[1].toUpperCase() : null;
}

/** The Country view a pathname shows, for the active state of the tier-2 links. */
export function countryNavKeyOf(pathname: string): CountryNavKey | null {
  const s = segments(pathname);
  if (s[0] !== COUNTRY_SEGMENT) return null;
  if (s.length === 1) return 'countries';
  if (s[1].toLowerCase() === 'about') return 'about';
  return countryIsoOfPath(pathname) ? 'profile' : null;
}

/** The Sectoral Compacts view a pathname shows, for the active state of the tier-2 links. */
export function aibNavKeyOf(pathname: string): AibNavKey | null {
  const s = segments(pathname);
  if (s[0] !== AIB_SEGMENT) return null;
  if (s.length === 1) return 'compacts';
  return s[1].toLowerCase() === 'about' ? 'about' : 'compact';
}

/** The (tech, stage) of a profile or country-in-sector path (/sector/:tech/:stage[/:iso3]), or null. */
export function sectorCellOfPath(pathname: string): { tech: string; stage: string } | null {
  const s = segments(pathname);
  if (s[0] !== 'sector' || s.length < 3 || s.length > 4) return null;
  if (isShellReservedSegment(s[1]) || !isShellTechSlug(s[1]) || !isShellStageSlug(s[2])) return null;
  return { tech: s[1], stage: s[2] };
}

/** A Sector profile (/sector/:tech/:stage, which has the "Sections" rail, id "sections"). */
export function isSectorProfilePath(pathname: string): boolean {
  return segments(pathname).length === 3 && sectorCellOfPath(pathname) !== null;
}

/** The Sector view a pathname shows, for the active state of the tier-2 links. */
export function sectorNavKeyOf(pathname: string): SectorNavKey | null {
  const s = segments(pathname);
  if (s[0] !== 'sector') return null;
  if (s.length === 1) return 'sectors';
  switch (s[1]) {
    case 'minerals':
      return 'minerals';
    case 'explore':
      return 'explore';
    case 'concentration':
      return 'concentration';
    case 'sources':
    case 'data':
      return 'sources';
    case 'country':
      return null;
    default:
      // /sector/:tech is the technology chain (browsing sectors); deeper is the profile
      return s.length === 2 ? 'sectors' : 'profile';
  }
}

/** The Strategic view a pathname shows (its first segment), for the active state of the links. */
export function strategicNavOf(pathname: string): string | null {
  const s = segments(pathname);
  return s.length && STRATEGIC_SEGMENTS.includes(s[0]) ? `/${s[0]}` : null;
}
