// Draw the Fence routes. The site is its own app: the layout (header, assessment bar, footer) is the
// root, and every view is a lazy chunk.

import { Navigate, useRouteError, type RouteObject } from 'react-router';
import FenceLayout, { PAGE } from './FenceLayout.tsx';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

function RouteError() {
  const err = useRouteError();
  return (
    <div className={PAGE}>
      <h1 className="text-xl font-medium text-paper">This page could not load.</h1>
      <p className="mt-2 text-sm text-paper-dim">{err instanceof Error ? err.message : 'Reload the page to try again.'}</p>
      <button type="button" className="btn btn-primary mt-4" onClick={() => window.location.reload()}>
        Reload
      </button>
    </div>
  );
}

export const fenceRoutes: RouteObject[] = [
  {
    path: '/',
    element: <FenceLayout />,
    errorElement: <RouteError />,
    children: [
      { index: true, lazy: view(() => import('./views/LandingView.tsx')) },
      { path: 'assess', lazy: view(() => import('./views/AssessView.tsx')) },
      { path: 'result', lazy: view(() => import('./views/ResultView.tsx')) },
      { path: 'scenario', lazy: view(() => import('./views/ScenarioView.tsx')) },
      { path: 'fence', lazy: view(() => import('./views/FenceView.tsx')) },
      { path: 'supply', lazy: view(() => import('./views/SupplyView.tsx')) },
      { path: 'examples', lazy: view(() => import('./views/ExamplesView.tsx')) },
      { path: 'methodology', lazy: view(() => import('./views/MethodologyView.tsx')) },
      { path: '*', element: <Navigate to="/" replace /> },
    ],
  },
];

/** The router basename from Vite's base ('/' or '/draw-the-fence/'). */
export function basename(): string {
  const b = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return b.length > 1 && b.endsWith('/') ? b.slice(0, -1) : b;
}
