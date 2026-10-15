// Routes (spec_sector.md 2.3, 2.4; spec_react.md 8.1). AppRoot (header, module nav, route notice,
// <main>, footer; no engine) is the root. Under it:
//   * index: HomeOrLegacy: Home, or the legacy redirect of `/?h=80…` to `/overview?h=80…`;
//   * a pathless lazy StrategicLayout (engine, scenario, runs, dials) around the eight Strategic
//     views, with their paths and titles unchanged;
//   * sectorRoute (src/sector/routes.tsx, builder C) at /sector;
//   * countryRoute (src/country/routes.tsx, the country builder) at /countries (spec_country.md A2.1);
//     the Strategic "Country scores" view keeps /country/:iso3;
//   * the shared glossary page /glossary (shell/GlossaryView.tsx, spec_country.md C3.3);
//   * aliases /sector-insights, /strategic-insights and the Country module's (/country-insights);
//     unknown paths go Home with a notice;
//   * route errors (a lazy chunk that fails to load) show a notice with Reload under the header.
// Every view is a lazy route chunk, so the Strategic first load does not carry Home or Sector code.

import { Navigate, useLocation, type RouteObject } from 'react-router';
import { AppRoot, ToHome } from './shell/AppRoot.tsx';
import { HomeOrLegacy } from './shell/HomeOrLegacy.tsx';
import { BootScreen } from './shell/screens.tsx';
import { RootError, RouteError } from './shell/RouteError.tsx';
import { sectorRoute } from './sector/routes.tsx';
import { COUNTRY_ALIASES, countryRoute } from './country/routes.tsx';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** An alias that keeps the query string (and hash) of the link. */
function Alias({ to }: { to: string }) {
  const loc = useLocation();
  return <Navigate to={{ pathname: to, search: loc.search, hash: loc.hash }} replace />;
}

export const STRATEGIC_ROUTES: RouteObject[] = [
  { path: 'overview', lazy: view(() => import('./views/Overview/OverviewView.tsx')) },
  { path: 'country/:iso3?', lazy: view(() => import('./views/Country/CountryView.tsx')) },
  { path: 'space', lazy: view(() => import('./views/Space/SpaceView.tsx')) },
  { path: 'strategic/:iso3?', lazy: view(() => import('./views/Strategic/StrategicView.tsx')) },
  { path: 'partners/:iso3?', lazy: view(() => import('./views/Partners/PartnersView.tsx')) },
  { path: 'network/:stage?', lazy: view(() => import('./views/Network/NetworkView.tsx')) },
  { path: 'methods', lazy: view(() => import('./views/Methods/MethodsView.tsx')) },
  { path: 'data', lazy: view(() => import('./views/Data/DataView.tsx')) },
];

export const routes: RouteObject[] = [
  {
    path: '/',
    element: <AppRoot />,
    hydrateFallbackElement: <BootScreen />,
    errorElement: <RootError />,
    children: [
      { index: true, element: <HomeOrLegacy /> },
      { id: 'strategic', lazy: view(() => import('./shell/StrategicLayout.tsx')), errorElement: <RouteError />, children: STRATEGIC_ROUTES },
      // a failing Sector chunk shows the notice under the shared header (the module's own errorElement wins)
      { errorElement: <RouteError />, ...sectorRoute },
      { errorElement: <RouteError />, ...countryRoute },
      { path: 'glossary', lazy: view(() => import('./shell/GlossaryView.tsx')), errorElement: <RouteError /> },
      { path: 'sector-insights', element: <Alias to="/sector" /> },
      { path: 'strategic-insights', element: <Alias to="/overview" /> },
      ...COUNTRY_ALIASES.map((a) => ({ path: a.path, element: <Alias to={a.to} /> })),
      { path: '*', element: <ToHome notice="That page does not exist; showing the home page." /> },
    ],
  },
];

/** The router basename from Vite's base ('/' or '/explorer/'). */
export function basename(): string {
  const b = import.meta.env.BASE_URL ?? '/';
  return b.length > 1 && b.endsWith('/') ? b.slice(0, -1) : b;
}
