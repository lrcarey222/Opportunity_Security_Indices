// Draw the Fence routes, mounted under AppRoot at /yards-fences. Every view is a lazy chunk, so no
// other module's first load grows.

import type { RouteObject } from 'react-router';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

export const yardsRoute: RouteObject = {
  path: 'yards-fences',
  lazy: async () => ({ Component: (await import('./YardsLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/LandingView.tsx')) },
    { path: 'assess', lazy: view(() => import('./views/AssessView.tsx')) },
    { path: 'result', lazy: view(() => import('./views/ResultView.tsx')) },
    { path: 'scenario', lazy: view(() => import('./views/ScenarioView.tsx')) },
    { path: 'fence', lazy: view(() => import('./views/FenceView.tsx')) },
    { path: 'diversify', lazy: view(() => import('./views/DiversifyView.tsx')) },
    { path: 'examples', lazy: view(() => import('./views/ExamplesView.tsx')) },
    { path: 'methodology', lazy: view(() => import('./views/MethodologyView.tsx')) },
    { path: '*', lazy: view(() => import('./views/LandingView.tsx')) },
  ],
};
