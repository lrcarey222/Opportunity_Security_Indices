// Old three-stage Sector links (design U1, 2026-10-28). Since the Sector pages moved to the six stages,
// /sector/<tech>/<upstream|midstream|downstream>[/<economy>] names no page. The site is a single-page
// app (the host serves index.html for every path), so the redirect is client-side: shell-level routes
// (LEGACY_SECTOR_ROUTES, mounted in routes.tsx after the Sector subtree) whose static stage segment
// outranks the Sector module's /sector/:tech/:stage, so no Sector page, header link or lastLocation
// ever sees a legacy stage.
//   * Downstream became Deployment exactly: /sector/<tech>/deployment[/<economy>], the query string and
//     the hash kept (a #trade link still opens the Trade section).
//   * Upstream and Midstream split into several stages, so no one page answers them: the link opens the
//     technology's chain page /sector/<tech> with the query kept and the economy (if any) as the
//     highlight ?c=; the hash is dropped (it named a section of a profile, which the chain page has not).
// Each carries a one-off notice (AppRoot's route notice) from lib/stages.ts legacyStageNote().

import { useLocation, type RouteObject } from 'react-router';
import { LEGACY_STAGE_SLUGS, legacySectorTarget, legacyStageNote } from '../lib/stages.ts';
import { Redirect } from './Redirect.tsx';
import { isShellTechSlug } from './sectorSlugs.ts';

/** A technology's display name from its slug ("electric-vehicles" -> "Electric Vehicles"; the Sector
 *  contract's TECH_LABELS are these words, lastLocation.test checks the slug rule). */
const techLabelOfSlug = (slug: string): string =>
  slug
    .split('-')
    .map((w) => (w ? w[0].toUpperCase() + w.slice(1) : w))
    .join(' ');

/** Where an old Sector link goes, and the notice it carries; null when the path is not one. Pure. */
export function legacySectorRedirect(
  pathname: string,
  search = '',
  hash = '',
): { to: { pathname: string; search: string; hash: string }; notice: string } | null {
  const t = legacySectorTarget(pathname);
  if (!t) return null;
  const tech = isShellTechSlug(t.techSlug) ? techLabelOfSlug(t.techSlug) : undefined;
  const note = legacyStageNote(t.legacy, tech);
  if (t.exact) return { to: { pathname: t.pathname, search, hash }, notice: note };
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  const iso = t.economy && /^[A-Za-z]{3}$/.test(t.economy) ? t.economy.toUpperCase() : null;
  if (iso && !q.has('c')) {
    // the highlight first, as the Sector module writes it
    const rest = [...q.entries()];
    for (const k of [...q.keys()]) q.delete(k);
    q.set('c', iso);
    for (const [k, v] of rest) q.append(k, v);
  }
  const s = q.toString();
  const where = tech ? `the ${tech} supply chain` : 'the supply chain';
  return { to: { pathname: t.pathname, search: s ? `?${s}` : '', hash: '' }, notice: `${note} Showing ${where}: pick a stage there.` };
}

/** The redirect element of the legacy routes (reads the whole location; the route params are not needed). */
export function LegacySectorRedirect() {
  const loc = useLocation();
  const r = legacySectorRedirect(loc.pathname, loc.search, loc.hash);
  // (only mounted on the legacy paths below; a miss goes to the Sector landing)
  return <Redirect to={r?.to ?? '/sector'} notice={r?.notice ?? null} />;
}

/** /sector/:tech/<legacy stage> and /sector/:tech/<legacy stage>/:iso3, for every legacy stage slug. */
export const LEGACY_SECTOR_ROUTES: RouteObject[] = LEGACY_STAGE_SLUGS.flatMap((s) => [
  { path: `sector/:tech/${s}`, element: <LegacySectorRedirect /> },
  { path: `sector/:tech/${s}/:iso3`, element: <LegacySectorRedirect /> },
]);
