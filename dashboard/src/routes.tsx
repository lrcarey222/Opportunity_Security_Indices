// Routes (spec_sector.md 2.3, 2.4; spec_react.md 8.1; IA refactor 2026-10-16: Home | Sectors |
// Countries | Allied Industrial Base | About). AppRoot (header, tabs, route notice, <main>, footer; no
// engine) is the root. Under it:
//   * index: HomeOrLegacy: Home, or the legacy redirect of `/?h=80…` to `/overview?h=80…`;
//   * a pathless lazy StrategicLayout (engine, scenario, runs, dials) around the five legacy Strategic
//     views, bare paths and titles unchanged (advanced pages); the header files them under Countries
//     or Allied Industrial Base (shell/modules.ts tabOf). Their one-economy deep links open that
//     section of the Country profile, which embeds the same views: /country/:iso3 -> #sectors,
//     /strategic/:iso3 -> #opportunities, /partners/:iso3 -> #partnerships (query string kept);
//   * sectorRoute (src/sector/routes.tsx) at /sector: Sectors. Old three-stage Sector links
//     (/sector/:tech/<upstream|midstream|downstream>[/:iso3]) are shell routes (shell/legacySector.tsx,
//     design U1): Downstream opens the Deployment page with the query and hash kept, Upstream and
//     Midstream the technology's chain page, each with a notice naming the stages that replaced it;
//   * countryRoute (src/country/routes.tsx) at /countries: Countries;
//   * aibHubRoute at /aib (Overview, Partnerships, Country roles, Diversification) and aibRoute at /compacts (Sectoral
//     Compacts), both in src/aib/routes.tsx: Allied Industrial Base;
//   * About: /about (shell/AboutView.tsx), with Methods (/methods) and Data & sources (/data) in a
//     pathless lazy AboutEngineLayout (the engine and a one-line perspective, no dial panel), and the
//     shared glossary page /glossary (shell/GlossaryView.tsx);
//   * aliases /sectors and /sector-insights (-> /sector), /strategic-insights (-> /countries: the
//     Strategic module is gone), the Country module's (/country-insights) and the AIB module's
//     (/sectoral-compacts, /allied-industrial-base);
//   * /network[/:stage], the page of the original allied network design (retired 2026-10-17), goes to
//     the Allied Industrial Base overview with a notice;
//     unknown paths go Home with a notice;
//   * route errors (a lazy chunk that fails to load) show a notice with Reload under the header.
// Every view is a lazy route chunk, so the Strategic first load does not carry Home or Sector code.

import { Navigate, useLocation, useParams, type RouteObject } from 'react-router';
import { AppRoot, ToHome } from './shell/AppRoot.tsx';
import { HomeOrLegacy } from './shell/HomeOrLegacy.tsx';
import { BootScreen } from './shell/screens.tsx';
import { RootError, RouteError } from './shell/RouteError.tsx';
import { Redirect } from './shell/Redirect.tsx';
import { sectorRoute } from './sector/routes.tsx';
import { LEGACY_SECTOR_ROUTES } from './shell/legacySector.tsx';
import { COUNTRY_ALIASES, countryRoute } from './country/routes.tsx';
import { AIB_ALIASES, aibHubRoute, aibRoute } from './aib/routes.tsx';

const view = (load: () => Promise<{ default: React.ComponentType }>) => async () => ({ Component: (await load()).default });

/** An alias that keeps the query string (and hash) of the link. */
function Alias({ to }: { to: string }) {
  const loc = useLocation();
  return <Navigate to={{ pathname: to, search: loc.search, hash: loc.hash }} replace />;
}

/** The notice of an old link to the retired allied network design page (/network, /network/:stage). */
export const NETWORK_RETIRED_NOTICE =
  'This link points to a page the Explorer does not have. The Allied Industrial Base analysis shows how the coalition absorbs a supply shock, where new investment would close the gaps and which country plays which role.';

/** Old Strategic deep links about one economy -> that section of its Country profile, which embeds
 *  the same views (IA refactor, plan 2). The query string (a custom perspective) is kept; the views
 *  themselves keep their bare paths (/country, /strategic, /partners) as advanced pages. */
export const LEGACY_PROFILE_LINKS: { path: string; section: string }[] = [
  { path: 'country/:iso3', section: 'sectors' },
  { path: 'strategic/:iso3', section: 'opportunities' },
  { path: 'partners/:iso3', section: 'partnerships' },
];

function ToProfile({ section }: { section: string }) {
  const { iso3 = '' } = useParams();
  const loc = useLocation();
  return <Navigate to={{ pathname: `/countries/${iso3.toUpperCase()}`, search: loc.search, hash: `#${section}` }} replace />;
}

export const STRATEGIC_ROUTES: RouteObject[] = [
  { path: 'overview', lazy: view(() => import('./views/Overview/OverviewView.tsx')) },
  { path: 'country', lazy: view(() => import('./views/Country/CountryView.tsx')) },
  { path: 'space', lazy: view(() => import('./views/Space/SpaceView.tsx')) },
  { path: 'strategic', lazy: view(() => import('./views/Strategic/StrategicView.tsx')) },
  { path: 'partners', lazy: view(() => import('./views/Partners/PartnersView.tsx')) },
];

/** Methods and Data & sources: About pages that read the engine (paths kept). */
export const ABOUT_ENGINE_ROUTES: RouteObject[] = [
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
      { id: 'about-engine', lazy: view(() => import('./shell/AboutEngineLayout.tsx')), errorElement: <RouteError />, children: ABOUT_ENGINE_ROUTES },
      // a failing Sector chunk shows the notice under the shared header (the module's own errorElement wins)
      { errorElement: <RouteError />, ...sectorRoute },
      // after the Sector subtree: their static stage segment outranks /sector/:tech/:stage, and on a tie
      // (/sector/minerals/upstream) the Sector module's own route, defined first, wins
      ...LEGACY_SECTOR_ROUTES,
      { errorElement: <RouteError />, ...countryRoute },
      { errorElement: <RouteError />, ...aibHubRoute },
      { errorElement: <RouteError />, ...aibRoute },
      { path: 'about', lazy: view(() => import('./shell/AboutView.tsx')), errorElement: <RouteError /> },
      { path: 'glossary', lazy: view(() => import('./shell/GlossaryView.tsx')), errorElement: <RouteError /> },
      { path: 'sectors', element: <Alias to="/sector" /> },
      { path: 'sector-insights', element: <Alias to="/sector" /> },
      { path: 'strategic-insights', element: <Alias to="/countries" /> },
      { path: 'network/:stage?', element: <Redirect to="/aib" notice={NETWORK_RETIRED_NOTICE} /> },
      ...LEGACY_PROFILE_LINKS.map((l) => ({ path: l.path, element: <ToProfile section={l.section} /> })),
      ...COUNTRY_ALIASES.map((a) => ({ path: a.path, element: <Alias to={a.to} /> })),
      ...AIB_ALIASES.map((a) => ({ path: a.path, element: <Alias to={a.to} /> })),
      { path: '*', element: <ToHome notice="That page does not exist; showing the home page." /> },
    ],
  },
];

/** The first route of the artifact preview from its link's hash: '#countries.JPN' -> '/countries/JPN'
 *  (the host passes only a plain token, letters, digits and . _ ~ -, so '.' stands for '/'). */
export function artifactEntry(hash: string): string {
  const token = hash.replace(/^#/, '');
  return /^[A-Za-z0-9._~-]+$/.test(token) ? `/${token.split('.').join('/')}` : '/';
}

/** The router basename from Vite's base ('/' or '/explorer/'). */
export function basename(): string {
  const b = import.meta.env.BASE_URL ?? '/';
  return b.length > 1 && b.endsWith('/') ? b.slice(0, -1) : b;
}
