// The top navigation and the module view lists (IA refactor, 2026-10-16; before it spec_sector.md 2.2,
// 5.2 and spec_country.md A2.1). Four tabs: Sectors, Countries, Allied Industrial Base and, visually
// secondary, About; Home is the eyebrow link. "Strategic Insights" is no longer a module: its views keep
// their paths (STRATEGIC_NAV, the legacy list) and the header files each under the tab that now owns
// its content (tabOf): Overview, Country scores, Security vs opportunity and Strategic Index under
// Countries, Partners under Allied Industrial Base, Methods and Data under About. The Allied network
// view (the original allied network design) is retired (2026-10-17): /network redirects to /aib.
// Dependency-free (the Sector slugs are the shell's copies), so the entry imports neither the engine
// nor any module contract; src/state/nav.ts re-exports STRATEGIC_NAV (alias NAV_ITEMS) and MODULES.

import { isShellReservedSegment, isShellStageSlug, isShellTechSlug } from './sectorSlugs.ts';

export type ModuleId = 'sector' | 'country' | 'aib' | 'about';
/** Where a path belongs: a module, the legacy Strategic views, Home, or nothing (an unknown path, about to redirect). */
export type Place = ModuleId | 'strategic' | 'home' | null;

export interface ModuleInfo {
  id: ModuleId;
  label: string;
  /** a short tag for cards that introduce the module */
  tag: string;
  /** the module's landing path */
  home: string;
  /** shown smaller, after a hairline (About) */
  secondary?: boolean;
}

/** The top tabs, in order. */
export const MODULES: readonly ModuleInfo[] = [
  { id: 'sector', label: 'Sectors', tag: 'WHAT MATTERS ABOUT A SECTOR', home: '/sector' },
  { id: 'country', label: 'Countries', tag: "A COUNTRY'S STRATEGIC POSITION", home: '/countries' },
  { id: 'aib', label: 'Allied Industrial Base', tag: 'WHERE COORDINATION ADDS VALUE', home: '/aib' },
  { id: 'about', label: 'About', tag: 'METHODS, DATA AND GLOSSARY', home: '/about', secondary: true },
];

export const moduleInfo = (id: ModuleId): ModuleInfo => MODULES.find((m) => m.id === id) as ModuleInfo;

/**
 * The legacy Strategic views (spec_react 8.1), in their old nav order. Not shown as a nav any more;
 * kept for the paths (STRATEGIC_SEGMENTS) and the scenario carry. Paths unchanged.
 */
export const STRATEGIC_NAV: { to: string; label: string }[] = [
  { to: '/overview', label: 'Overview' },
  { to: '/country', label: 'Country scores' },
  { to: '/space', label: 'Security vs opportunity' },
  { to: '/strategic', label: 'Strategic Index' },
  { to: '/partners', label: 'Partners' },
  { to: '/methods', label: 'Methods' },
  { to: '/data', label: 'Data' },
];

/** The first path segments of the Strategic views. */
export const STRATEGIC_SEGMENTS = STRATEGIC_NAV.map((n) => n.to.slice(1));

/** Which tab owns each legacy Strategic view. */
const STRATEGIC_TAB: Readonly<Record<string, ModuleId>> = {
  overview: 'country',
  country: 'country',
  space: 'country',
  strategic: 'country',
  partners: 'aib',
  methods: 'about',
  data: 'about',
};

export type SectorNavKey = 'sectors' | 'profile' | 'minerals' | 'explore' | 'concentration' | 'sources';

/**
 * Sector views. PROFILE's target is the last visited cell (see lastLocation.ts). Concentration and
 * Sources & data keep their paths but leave the nav: the Sector profile links to them (Security &
 * concentration; Evidence) and About lists the sources.
 */
export const SECTOR_NAV: { key: SectorNavKey; to: string; label: string }[] = [
  { key: 'sectors', to: '/sector', label: 'All sectors' },
  { key: 'profile', to: '/sector/batteries/midstream', label: 'Sector profile' },
  { key: 'minerals', to: '/sector/minerals', label: 'Minerals' },
  { key: 'explore', to: '/sector/explore', label: 'Advanced explorer' },
];

export type CountryNavKey = 'countries' | 'profile' | 'about';

/** The default profile of the Country nav before any country was visited. */
export const COUNTRY_DEFAULT_ISO = 'USA';

/** Country views. PROFILE's target is the last visited country. /countries/about stays (About lists it). */
export const COUNTRY_NAV: { key: CountryNavKey; to: string; label: string }[] = [
  { key: 'countries', to: '/countries', label: 'All countries' },
  { key: 'profile', to: `/countries/${COUNTRY_DEFAULT_ISO}`, label: 'Country profile' },
];

/** The first path segment of the Country module (the legacy "Country scores" view is /country). */
export const COUNTRY_SEGMENT = 'countries';

/** Second segments under /countries that are views, not countries. */
const COUNTRY_RESERVED = new Set(['about']);

export type AibNavKey = 'overview' | 'partnerships' | 'compacts' | 'roles' | 'about';

/**
 * Allied Industrial Base views: the overview, then the three questions. Sectoral Compacts keep their
 * paths (/compacts, /compacts/:id); /compacts/about stays (About lists it). The module registers the
 * same list (src/aib/nav.ts) once its data load; this is the fallback before it does.
 */
export const AIB_NAV: { key: AibNavKey; to: string; label: string }[] = [
  { key: 'overview', to: '/aib', label: 'Overview' },
  { key: 'partnerships', to: '/aib/partnerships', label: 'Partnerships' },
  { key: 'compacts', to: '/compacts', label: 'Sectoral Compacts' },
  { key: 'roles', to: '/aib/roles', label: 'Country roles' },
];

/** The first path segments of the Allied Industrial Base module. */
export const AIB_SEGMENT = 'aib';
export const COMPACTS_SEGMENT = 'compacts';

export type AboutNavKey = 'about' | 'methods' | 'data' | 'glossary';

/** About: Methods and Data are the Strategic pages (paths kept), the Glossary the shell page. */
export const ABOUT_NAV: { key: AboutNavKey; to: string; label: string }[] = [
  { key: 'about', to: '/about', label: 'Overview' },
  { key: 'methods', to: '/methods', label: 'Methods' },
  { key: 'data', to: '/data', label: 'Data & sources' },
  { key: 'glossary', to: '/glossary', label: 'Glossary' },
];

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** Which module (or Home, or the legacy Strategic views) a pathname belongs to. */
export function placeOf(pathname: string): Place {
  const s = segments(pathname);
  if (s.length === 0) return 'home';
  if (s[0] === 'sector') return 'sector';
  if (s[0] === COUNTRY_SEGMENT) return 'country';
  if (s[0] === AIB_SEGMENT || s[0] === COMPACTS_SEGMENT) return 'aib';
  if (s[0] === 'about' || s[0] === 'glossary') return 'about';
  if (STRATEGIC_SEGMENTS.includes(s[0])) return 'strategic';
  return null;
}

/** The tab a pathname shows as current (null on Home and unknown paths). */
export function tabOf(pathname: string): ModuleId | null {
  const p = placeOf(pathname);
  if (p === 'strategic') return STRATEGIC_TAB[segments(pathname)[0]] ?? null;
  return p === 'home' ? null : p;
}

/** The ISO3 of a Country profile or memo path (/countries/:iso3[/memo]), upper-cased, or null. */
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

/** The Allied Industrial Base view a pathname shows, for the active state of the tier-2 links. */
export function aibNavKeyOf(pathname: string): AibNavKey | null {
  const s = segments(pathname);
  if (s[0] === AIB_SEGMENT) {
    if (s.length === 1) return 'overview';
    const v = s[1].toLowerCase();
    return v === 'partnerships' ? 'partnerships' : v === 'roles' ? 'roles' : null;
  }
  if (s[0] === COMPACTS_SEGMENT) return s.length > 1 && s[1].toLowerCase() === 'about' ? 'about' : 'compacts';
  // the legacy Partners view lives with the partnerships
  if (s[0] === 'partners') return 'partnerships';
  return null;
}

/** The About page a pathname shows, for the active state of the tier-2 links. */
export function aboutNavKeyOf(pathname: string): AboutNavKey | null {
  const s = segments(pathname);
  switch (s[0]) {
    case 'about':
      return s.length === 1 ? 'about' : null;
    case 'methods':
      return 'methods';
    case 'data':
      return 'data';
    case 'glossary':
      return 'glossary';
    default:
      return null;
  }
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

/** The Strategic view a pathname shows (its first segment), or null. */
export function strategicNavOf(pathname: string): string | null {
  const s = segments(pathname);
  return s.length && STRATEGIC_SEGMENTS.includes(s[0]) ? `/${s[0]}` : null;
}
