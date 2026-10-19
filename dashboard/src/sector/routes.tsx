// Sector Insights routes (spec_sector.md 2.3). Builder B mounts `sectorRoute` under AppRoot at
// /sector. Every view is a lazy route chunk, and SectorLayout is one too, so the Strategic first load
// does not grow. Reserved segments (explore, concentration, minerals, sources, country, data) are
// static and outrank `:tech`. Unknown slugs go to the parent page with a route notice; the Country
// snapshot (4.9) is not in this build, so /sector/country/:iso3 redirects to /sector?c=ISO3.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';
import { Redirect } from '../shell/Redirect.tsx';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** /sector with a notice (unknown paths under /sector). */
function ToSectorHome() {
  const loc = useLocation();
  const bad = loc.pathname.replace(/^.*\/sector\/?/, '').split('/')[0];
  return <Redirect to="/sector" notice={bad ? `No sector page called '${bad}'; showing all sectors.` : 'Showing all sectors.'} />;
}

/** Phase-1 stand-in for the Country snapshot: the Atlas with the country highlighted. */
function CountryRedirect() {
  const { iso3 } = useParams();
  const loc = useLocation();
  const q = new URLSearchParams(loc.search);
  if (iso3) q.set('c', iso3.toUpperCase());
  const s = q.toString();
  return <Navigate to={{ pathname: '/sector', search: s ? `?${s}` : '' }} replace />;
}

/** The sector data downloads live on Sources. */
function ToDownloads() {
  const loc = useLocation();
  return <Navigate to={{ pathname: '/sector/sources', search: loc.search, hash: '#downloads' }} replace />;
}

/** The Sector Insights subtree. */
export const sectorRoute: RouteObject = {
  path: 'sector',
  lazy: async () => ({ Component: (await import('./SectorLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/SectorHomeView.tsx')) },
    { path: 'explore/:theme?', lazy: view(() => import('./views/explore/ExploreView.tsx')) },
    { path: 'concentration', lazy: view(() => import('./views/ConcentrationView.tsx')) },
    { path: 'minerals', lazy: view(() => import('./views/minerals/MineralIndexView.tsx')) },
    { path: 'minerals/:mineral', lazy: view(() => import('./views/minerals/MineralView.tsx')) },
    { path: 'sources', lazy: view(() => import('./views/SourcesView.tsx')) },
    { path: 'data', element: <ToDownloads /> },
    { path: 'country/:iso3?', element: <CountryRedirect /> },
    { path: ':tech', lazy: view(() => import('./views/TechChainView.tsx')) },
    { path: ':tech/:stage', lazy: view(() => import('./views/profile/SectorProfileView.tsx')) },
    { path: ':tech/:stage/:iso3', lazy: view(() => import('./views/CountryInSectorView.tsx')) },
    { path: '*', element: <ToSectorHome /> },
  ],
};
