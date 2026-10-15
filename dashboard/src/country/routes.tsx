// Country Insights routes (spec_country.md A2.1, A4). The palette builder mounts `countryRoute` under
// AppRoot at /countries, like sectorRoute. CountryLayout and every view are lazy route chunks, so
// neither the Strategic nor the Sector first load grows. `about` is static and outranks `:iso3`.
// The Strategic "Country scores" view keeps /country/:iso3; nothing here touches it.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** /countries/:iso3/<anything else> -> the profile (the query string is kept). */
function ToProfile() {
  const { iso3 } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/countries/${(iso3 ?? '').toUpperCase()}`, search: loc.search }} replace />;
}

/** The Country Insights subtree. */
export const countryRoute: RouteObject = {
  path: 'countries',
  lazy: async () => ({ Component: (await import('./CountryLayout.tsx')).default }),
  children: [
    { index: true, lazy: view(() => import('./views/CountryHomeView.tsx')) },
    { path: 'about', lazy: view(() => import('./views/CountryAboutView.tsx')) },
    { path: ':iso3', lazy: view(() => import('./views/CountryProfileView.tsx')) },
    { path: ':iso3/memo', lazy: view(() => import('./views/MemoView.tsx')) },
    { path: ':iso3/*', element: <ToProfile /> },
  ],
};

/** Aliases the shell may mount beside countryRoute (A2.1): /country-insights -> /countries. */
export const COUNTRY_ALIASES: { path: string; to: string }[] = [{ path: 'country-insights', to: '/countries' }];
