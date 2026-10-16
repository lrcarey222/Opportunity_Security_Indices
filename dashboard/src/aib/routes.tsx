// Sectoral Compacts routes. The shell mounts `aibRoute` under AppRoot at /compacts, like countryRoute.
// AibLayout and every view are lazy route chunks, so no other module's first load grows. `about` is
// static and outranks `:id`.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** /compacts/:id/<anything else> -> the compact (the query string is kept). */
function ToCompact() {
  const { id } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/compacts/${encodeURIComponent(id ?? '')}`, search: loc.search }} replace />;
}

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

/** Aliases the shell mounts beside aibRoute: /sectoral-compacts -> /compacts. */
export const AIB_ALIASES: { path: string; to: string }[] = [{ path: 'sectoral-compacts', to: '/compacts' }];
