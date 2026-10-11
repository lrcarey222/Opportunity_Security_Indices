// The Sector slugs the shell needs to route and to carry context (tabs, PROFILE, lastLocation),
// copied from src/sector/contract.ts so the entry chunk does not carry the Sector contract (the
// Strategic first load must not grow). lastLocation.test.ts asserts they equal the contract's.

export const SHELL_TECH_SLUGS: readonly string[] = [
  'batteries', 'coal', 'electric-grid', 'electric-vehicles', 'gas', 'geothermal', 'green-hydrogen', 'nuclear', 'oil', 'solar', 'wind', 'semiconductors',
];
export const SHELL_STAGE_SLUGS: readonly string[] = ['upstream', 'midstream', 'downstream'];
export const SHELL_RESERVED_SEGMENTS: readonly string[] = ['explore', 'concentration', 'minerals', 'sources', 'country', 'data'];

export const isShellTechSlug = (s: string): boolean => SHELL_TECH_SLUGS.includes(s);
export const isShellStageSlug = (s: string): boolean => SHELL_STAGE_SLUGS.includes(s);
export const isShellReservedSegment = (s: string): boolean => SHELL_RESERVED_SEGMENTS.includes(s);
