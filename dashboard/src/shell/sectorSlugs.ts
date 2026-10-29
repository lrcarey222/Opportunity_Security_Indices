// The Sector slugs the shell needs to route and to carry context (tabs, PROFILE, lastLocation),
// copied from src/sector/contract.ts so the entry chunk does not carry the Sector contract (the
// Strategic first load must not grow). lastLocation.test.ts asserts they equal the contract's.
// The stage slugs are the six of lib/stages.ts (a small dependency-free module the Sector contract
// reads too), so the shell and the Sector routes cannot drift; the three legacy slugs (upstream,
// midstream, downstream) are not stage slugs: an old link that names one is redirected by the shell
// (routes.tsx LEGACY_SECTOR_ROUTES, design U1) before any Sector page or lastLocation sees it.

import { isLegacyStageSlug, SUPPLY_STAGE_SLUGS } from '../lib/stages.ts';

export const SHELL_TECH_SLUGS: readonly string[] = [
  'batteries', 'coal', 'electric-grid', 'electric-vehicles', 'gas', 'geothermal', 'green-hydrogen', 'nuclear', 'oil', 'solar', 'wind', 'semiconductors',
];
export const SHELL_STAGE_SLUGS: readonly string[] = SUPPLY_STAGE_SLUGS;
export const SHELL_RESERVED_SEGMENTS: readonly string[] = ['explore', 'concentration', 'minerals', 'sources', 'country', 'data'];

export const isShellTechSlug = (s: string): boolean => SHELL_TECH_SLUGS.includes(s);
export const isShellStageSlug = (s: string): boolean => SHELL_STAGE_SLUGS.includes(s);
/** A legacy three-stage slug of an old link ("midstream"): redirected, never a stage of a page. */
export const isShellLegacyStageSlug = (s: string): boolean => isLegacyStageSlug(s);
export const isShellReservedSegment = (s: string): boolean => SHELL_RESERVED_SEGMENTS.includes(s);
