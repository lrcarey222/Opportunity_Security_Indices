// The modules and their view lists (spec_sector.md 2.2, 5.2). Dependency-free (the Sector slugs
// are the shell's copies), so the entry imports neither the engine nor the Sector contract;
// src/state/nav.ts re-exports STRATEGIC_NAV (alias NAV_ITEMS) and MODULES for the Strategic views.

import { isShellReservedSegment, isShellStageSlug, isShellTechSlug } from './sectorSlugs.ts';

export type ModuleId = 'sector' | 'strategic' | 'yards';
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

/** Sector first, as the user named it. */
export const MODULES: readonly ModuleInfo[] = [
  { id: 'sector', label: 'Sector Insights', tag: 'DESCRIPTIVE DATA', home: '/sector' },
  { id: 'strategic', label: 'Strategic Insights', tag: 'NORMATIVE PRIORITISATION', home: '/overview' },
  { id: 'yards', label: 'Draw the Fence', tag: 'DECISION TOOL', home: '/yards-fences' },
];

export const moduleInfo = (id: ModuleId): ModuleInfo => MODULES.find((m) => m.id === id) as ModuleInfo;

/** Strategic Insights views (spec_react 8.1), in nav order. */
export const STRATEGIC_NAV: { to: string; label: string }[] = [
  { to: '/overview', label: 'Overview' },
  { to: '/country', label: 'Country' },
  { to: '/space', label: 'Space' },
  { to: '/strategic', label: 'Strategic Index' },
  { to: '/partners', label: 'Partners' },
  { to: '/network', label: 'Network' },
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

/** Draw the Fence (Yards & Fences) views, in nav order. The assessment state travels in `a`. */
export const YARDS_SEGMENT = 'yards-fences';
export const YARDS_NAV: { key: string; to: string; label: string }[] = [
  { key: 'home', to: '/yards-fences', label: 'Overview' },
  { key: 'assess', to: '/yards-fences/assess', label: 'Assess' },
  { key: 'result', to: '/yards-fences/result', label: 'Result' },
  { key: 'scenario', to: '/yards-fences/scenario', label: 'Test another world' },
  { key: 'fence', to: '/yards-fences/fence', label: 'Build your fence' },
  { key: 'diversify', to: '/yards-fences/diversify', label: 'Diversify' },
  { key: 'examples', to: '/yards-fences/examples', label: 'Examples' },
  { key: 'methodology', to: '/yards-fences/methodology', label: 'Methodology' },
];

/** The Draw the Fence view a pathname shows. */
export function yardsNavKeyOf(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== YARDS_SEGMENT) return null;
  return s.length === 1 ? 'home' : (YARDS_NAV.find((n) => n.key === s[1])?.key ?? null);
}

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** Which module (or Home) a pathname belongs to. */
export function placeOf(pathname: string): Place {
  const s = segments(pathname);
  if (s.length === 0) return 'home';
  if (s[0] === 'sector') return 'sector';
  if (STRATEGIC_SEGMENTS.includes(s[0])) return 'strategic';
  if (s[0] === YARDS_SEGMENT) return 'yards';
  return null;
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
