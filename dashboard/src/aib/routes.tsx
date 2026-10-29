// Allied Industrial Base routes (IA refactor, 2026-10-16; strategic layer 2026-10-20). The shell mounts two
// subtrees under AppRoot, both inside AibLayout (the compacts document and the module nav):
//   * aibHubRoute at /aib: the Overview (index), Partnerships (/aib/partnerships), Country roles
//     (/aib/roles) and Diversification (/aib/diversification), the module's composition pages;
//   * aibRoute at /compacts: Sectoral Compacts. /compacts lands on the Strategic Sectoral Compacts
//     (StrategicCompactsView); a strategic compact is /compacts/s/:sid (StrategicCompactView); the compact
//     candidates, now their Workstreams, are listed at /compacts/workstreams (CompactsIndexView, the former
//     landing) and keep /compacts/:id (CompactView); /compacts/about stays.
// AibLayout and every view are lazy route chunks, so no other module's first load grows (this file is in
// the entry: it imports nothing from the module but the pure scenarioUrl.ts). The static segments
// (about, workstreams, s) outrank `:id`. Redirects keep the query (the scenario toggle, scn) and the hash:
// /compacts/SC-... -> /compacts/s/SC-...; a link to the former landing's filters or sections (tech, scope,
// #nodes, #map, #card-AIB-Cxx) -> /compacts/workstreams; /compacts/s -> /compacts.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';
import { Redirect } from '../shell/Redirect.tsx';
import { scenarioCarry } from './scenarioUrl.ts';

type Lazy = () => Promise<{ default: React.ComponentType }>;
const view = (load: Lazy) => async () => ({ Component: (await load()).default });

/** A strategic compact id (contract.ts AIB_STRATEGIC_ID_RE, case-insensitive here; repeated so the entry does not import the contract). */
const STRATEGIC_ID = /^SC-[A-Z0-9-]+$/i;
/** The former landing's query keys and sections (CompactsIndexView, now /compacts/workstreams; `stage` since 2026-10-29). */
const WORKSTREAM_KEYS = ['tech', 'stage', 'scope'];
const WORKSTREAM_HASH = /^#(nodes|map|card-.+)$/;

/** /compacts/:id/<anything else> -> the compact (the query string is kept). */
function ToCompact() {
  const { id } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/compacts/${encodeURIComponent(id ?? '')}`, search: loc.search }} replace />;
}

/** /compacts/s/:sid/<anything else> -> the strategic compact (the query string is kept). */
function ToStrategic() {
  const { sid } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/compacts/s/${encodeURIComponent(sid ?? '')}`, search: loc.search }} replace />;
}

/** /compacts/s -> the strategic landing (the query string kept). */
function ToLanding() {
  const loc = useLocation();
  return <Navigate to={{ pathname: '/compacts', search: loc.search, hash: loc.hash }} replace />;
}

/** /compacts/workstreams/<anything> -> the Workstreams index (the query string kept). */
function ToWorkstreams() {
  const loc = useLocation();
  return <Navigate to={{ pathname: '/compacts/workstreams', search: loc.search }} replace />;
}

/** The workstream page, or a strategic compact's id written as /compacts/SC-... sent to /compacts/s/SC-.... */
function workstreamOrStrategic(Workstream: React.ComponentType): React.ComponentType {
  return function CompactRoute() {
    const { id } = useParams();
    const loc = useLocation();
    if (id && STRATEGIC_ID.test(id)) return <Navigate to={{ pathname: `/compacts/s/${encodeURIComponent(id.toUpperCase())}`, search: loc.search, hash: loc.hash }} replace />;
    return <Workstream />;
  };
}

/** The strategic landing, or a link to the former landing's filters or sections sent to /compacts/workstreams. */
function landingOrWorkstreams(Landing: React.ComponentType): React.ComponentType {
  return function CompactsLanding() {
    const loc = useLocation();
    const q = new URLSearchParams(loc.search);
    if (WORKSTREAM_KEYS.some((k) => q.has(k)) || WORKSTREAM_HASH.test(loc.hash)) {
      return <Navigate to={{ pathname: '/compacts/workstreams', search: loc.search, hash: loc.hash }} replace />;
    }
    return <Landing />;
  };
}

/** /aib/<anything else> -> the Overview, with a notice (the scenario toggle kept). */
function ToAibHome() {
  const loc = useLocation();
  return <Redirect to={`/aib${scenarioCarry(loc.search)}`} notice="That Allied Industrial Base page does not exist; showing the overview." />;
}

/** The Allied Industrial Base composition pages. */
export const aibHubRoute: RouteObject = {
  path: 'aib',
  lazy: async () => ({ Component: (await import('./AibLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/AibOverviewView.tsx')) },
    { path: 'partnerships', lazy: view(() => import('./views/AibPartnershipsView.tsx')) },
    { path: 'roles', lazy: view(() => import('./views/AibRolesView.tsx')) },
    { path: 'diversification', lazy: view(() => import('./views/AibDiversifyView.tsx')) },
    { path: '*', element: <ToAibHome /> },
  ],
};

/** The Sectoral Compacts subtree. */
export const aibRoute: RouteObject = {
  path: 'compacts',
  lazy: async () => ({ Component: (await import('./AibLayout.tsx')).default }),
  children: [
    { index: true, lazy: async () => ({ Component: landingOrWorkstreams((await import('./views/StrategicCompactsView.tsx')).default) }) },
    { path: 'workstreams', lazy: view(() => import('./views/CompactsIndexView.tsx')) },
    { path: 'workstreams/*', element: <ToWorkstreams /> },
    { path: 'about', lazy: view(() => import('./views/AibAboutView.tsx')) },
    { path: 's', element: <ToLanding /> },
    { path: 's/:sid', lazy: view(() => import('./views/StrategicCompactView.tsx')) },
    { path: 's/:sid/*', element: <ToStrategic /> },
    { path: ':id', lazy: async () => ({ Component: workstreamOrStrategic((await import('./views/CompactView.tsx')).default) }) },
    { path: ':id/*', element: <ToCompact /> },
  ],
};

/** Aliases the shell mounts beside aibRoute: /sectoral-compacts -> /compacts, /allied-industrial-base -> /aib. */
export const AIB_ALIASES: { path: string; to: string }[] = [
  { path: 'sectoral-compacts', to: '/compacts' },
  { path: 'allied-industrial-base', to: '/aib' },
];
