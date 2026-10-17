// Allied Industrial Base routes (IA refactor, 2026-10-16). The shell mounts two subtrees under AppRoot,
// both inside AibLayout (the compacts document and the module nav):
//   * aibHubRoute at /aib: the Overview (index), Partnerships (/aib/partnerships) and Country roles
//     (/aib/roles), the module's composition pages;
//   * aibRoute at /compacts: Sectoral Compacts (landing, /compacts/:id, /compacts/about), paths kept.
// AibLayout and every view are lazy route chunks, so no other module's first load grows. `about` is
// static and outranks `:id`.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';
import { Redirect } from '../shell/Redirect.tsx';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** /compacts/:id/<anything else> -> the compact (the query string is kept). */
function ToCompact() {
  const { id } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/compacts/${encodeURIComponent(id ?? '')}`, search: loc.search }} replace />;
}

/** /aib/<anything else> -> the Overview, with a notice. */
function ToAibHome() {
  return <Redirect to="/aib" notice="That Allied Industrial Base page does not exist; showing the overview." />;
}

/** The Allied Industrial Base composition pages. */
export const aibHubRoute: RouteObject = {
  path: 'aib',
  lazy: async () => ({ Component: (await import('./AibLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/AibOverviewView.tsx')) },
    { path: 'partnerships', lazy: view(() => import('./views/AibPartnershipsView.tsx')) },
    { path: 'roles', lazy: view(() => import('./views/AibRolesView.tsx')) },
    { path: '*', element: <ToAibHome /> },
  ],
};

/** The Sectoral Compacts subtree. */
export const aibRoute: RouteObject = {
  path: 'compacts',
  lazy: async () => ({ Component: (await import('./AibLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/CompactsIndexView.tsx')) },
    { path: 'about', lazy: view(() => import('./views/AibAboutView.tsx')) },
    { path: ':id', lazy: view(() => import('./views/CompactView.tsx')) },
    { path: ':id/*', element: <ToCompact /> },
  ],
};

/** Aliases the shell mounts beside aibRoute: /sectoral-compacts -> /compacts, /allied-industrial-base -> /aib. */
export const AIB_ALIASES: { path: string; to: string }[] = [
  { path: 'sectoral-compacts', to: '/compacts' },
  { path: 'allied-industrial-base', to: '/aib' },
];
